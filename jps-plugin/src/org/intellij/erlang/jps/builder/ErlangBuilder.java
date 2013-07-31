package org.intellij.erlang.jps.builder;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.util.CommonProcessors;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializationException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.jps.model.JpsErlangModuleExtension;
import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jdom.Document;
import org.jdom.JDOMException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.incremental.resources.ResourcesBuilder;
import org.jetbrains.jps.incremental.resources.StandardResourceBuilderEnabler;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.JpsDependencyElement;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.module.JpsModuleDependency;
import org.jetbrains.jps.model.module.JpsModuleSourceRoot;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author @nik
 */
public class ErlangBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  public static final String DEPENDENCIES_CONFIG_FILE_PATH = "erlang-builder/deps-config.xml";
  public static final String NAME = "erlc";
  private final static Logger LOG = Logger.getInstance(ErlangBuilder.class);

  public ErlangBuilder() {
    super(Arrays.asList(ErlangTargetType.PRODUCTION, ErlangTargetType.TESTS));

    //TODO provide a way to copy erlang resources
    //disables java resource builder for erlang modules
    ResourcesBuilder.registerEnabler(new StandardResourceBuilderEnabler() {
      @Override
      public boolean isResourceProcessingEnabled(JpsModule module) {
        return !(module.getModuleType() instanceof JpsErlangModuleType);
      }
    });
  }

  @Override
  public void build(@NotNull ErlangTarget target, 
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder, 
                    @NotNull BuildOutputConsumer outputConsumer, 
                    @NotNull final CompileContext context) throws ProjectBuildException, IOException {
    LOG.debug(target.getPresentableName());
    if (!holder.hasDirtyFiles() && !holder.hasRemovedFiles()) {
      return;
    }

    JpsModule module = target.getModule();
    File outputDirectory = getBuildOutputDirectory(module, target, context);
    JpsSdk<JpsDummyElement> sdk = getSdk(context, module);
    File executable = JpsErlangSdkType.getByteCodeCompilerExecutable(sdk.getHomePath());
    GeneralCommandLine commandLine = new GeneralCommandLine();

    commandLine.setWorkDirectory(outputDirectory);
    commandLine.setExePath(executable.getAbsolutePath());
    addCodePath(commandLine, module, target, context);
    addParseTransforms(commandLine, module);
    addIncludePaths(commandLine, module);
    addErlangModules(commandLine, module, target, context);

    runBuildProcess(context, commandLine);
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  @NotNull
  private static File getBuildOutputDirectory(JpsModule module, ErlangTarget target, CompileContext context) throws ProjectBuildException {
    JpsJavaExtensionService instance = JpsJavaExtensionService.getInstance();
    File outputDirectory = instance.getOutputDirectory(module, target.isTests());
    if (outputDirectory == null) {
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "No output dir for module " + module.getName()));
      throw new ProjectBuildException();
    }
    if (!outputDirectory.exists()) {
      FileUtil.createDirectory(outputDirectory);
    }
    return outputDirectory;
  }

  @NotNull
  private static JpsSdk<JpsDummyElement> getSdk(CompileContext context, JpsModule module) throws ProjectBuildException {
    JpsSdk<JpsDummyElement> sdk = module.getSdk(JpsErlangSdkType.INSTANCE);
    if (sdk == null) {
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "No SDK for module " + module.getName()));
      throw new ProjectBuildException();
    }
    return sdk;
  }

  private static void runBuildProcess(final CompileContext context, GeneralCommandLine commandLine) throws ProjectBuildException {
    Process process;
    try {
      process = commandLine.createProcess();
    } catch (ExecutionException e) {
      throw new ProjectBuildException("Failed to launch erlang compiler", e);
    }
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, commandLine.getCommandLineString(), Charset.defaultCharset());
    ProcessAdapter adapter = new
      ProcessAdapter() {
        @Override
        public void onTextAvailable(ProcessEvent event, Key outputType) {
          ErlangCompilerError error = ErlangCompilerError.create("", event.getText());
          if (error != null) {
            boolean isError = error.getCategory() == CompilerMessageCategory.ERROR;
            BuildMessage.Kind kind = isError ? BuildMessage.Kind.ERROR : BuildMessage.Kind.WARNING;
            CompilerMessage msg = new CompilerMessage(
              NAME, kind,
              error.getErrorMessage(),
              VirtualFileManager.extractPath(error.getUrl()), -1, -1, -1, error.getLine(), -1);
            context.processMessage(msg);
          }
        }
      };
    handler.addProcessListener(adapter);
    handler.startNotify();
    handler.waitFor();
  }

  private static void addIncludePaths(GeneralCommandLine commandLine, JpsModule module) {
    JpsErlangModuleExtension extension = JpsErlangModuleExtension.getExtension(module);
    if (extension != null) {
      for (String includePath : extension.getIncludePaths()) {
        commandLine.addParameters("-I", includePath);
      }
    }
  }

  private static void addErlangModules(GeneralCommandLine commandLine, JpsModule module, ErlangTarget target, CompileContext context) {
    if (!addErlangModulesFromConfig(commandLine, module, target, context)) {
      addErlangModulesDefault(commandLine, module, target);
    }
  }

  private static void addErlangModulesDefault(GeneralCommandLine commandLine, JpsModule module, ErlangTarget target) {
    CommonProcessors.CollectProcessor<File> erlFilesCollector = new CommonProcessors.CollectProcessor<File>() {
      @Override
      protected boolean accept(File file) {
        return !file.isDirectory() && FileUtilRt.extensionEquals(file.getName(), "erl");
      }
    };
    List<JpsModuleSourceRoot> sourceRoots = new ArrayList<JpsModuleSourceRoot>();
    ContainerUtil.addAll(sourceRoots, module.getSourceRoots(JavaSourceRootType.SOURCE));
    if (target.isTests()) {
      ContainerUtil.addAll(sourceRoots, module.getSourceRoots(JavaSourceRootType.TEST_SOURCE));
    }
    for (JpsModuleSourceRoot root : sourceRoots) {
      FileUtil.processFilesRecursively(root.getFile(), erlFilesCollector);
    }
    for (File f : erlFilesCollector.getResults()) {
      commandLine.addParameter(f.getAbsolutePath());
    }
  }

  private static boolean addErlangModulesFromConfig(GeneralCommandLine commandLine, JpsModule module,
                                                    ErlangTarget target, CompileContext context) {
    File dataStorageRoot = context.getProjectDescriptor().dataManager.getDataPaths().getDataStorageRoot();
    File depsConfigFile = new File(dataStorageRoot, DEPENDENCIES_CONFIG_FILE_PATH);
    if (!depsConfigFile.exists()) return false;
    ErlangModuleBuildOrders buildOrders;
    try {
      Document document = JDOMUtil.loadDocument(depsConfigFile);
      buildOrders = XmlSerializer.deserialize(document, ErlangModuleBuildOrders.class);
    } catch (XmlSerializationException e) {
      return false;
    } catch (JDOMException e) {
      return false;
    } catch (IOException e) {
      return false;
    }
    if (buildOrders == null) return false;
    for (ErlangModuleBuildOrderDescriptor buildOrder : buildOrders.myModuleBuildOrderDescriptors) {
      if (StringUtil.equals(buildOrder.myModuleName, module.getName())) {
        commandLine.addParameters(buildOrder.myOrderedErlangModulePaths);
        if (target.isTests()) {
          commandLine.addParameters(buildOrder.myOrderedErlangTestModulePaths);
        }
        return true;
      }
    }
    return false;
  }

  private static void addParseTransforms(GeneralCommandLine commandLine, JpsModule module) throws ProjectBuildException {
    JpsErlangModuleExtension extension = JpsErlangModuleExtension.getExtension(module);
    List<String> parseTransforms = extension != null ? extension.getParseTransforms() : null;
    if (parseTransforms == null || parseTransforms.isEmpty()) return;
    for (String ptModule : parseTransforms) {
      commandLine.addParameter("+{parse_transform, " + ptModule + "}");
    }
  }

  private static void addCodePath(GeneralCommandLine commandLine, JpsModule module,
                                                 ErlangTarget target, CompileContext context) throws ProjectBuildException {
    addModuleOutputToCodePath(commandLine, module, target, context);
    for (JpsDependencyElement dependency : module.getDependenciesList().getDependencies()) {
      if (!(dependency instanceof JpsModuleDependency)) continue;
      JpsModuleDependency moduleDependency = (JpsModuleDependency) dependency;
      JpsModule depModule = moduleDependency.getModule();
      if (depModule != null) {
        addModuleOutputToCodePath(commandLine, depModule, target, context);
      }
    }
  }

  private static void addModuleOutputToCodePath(GeneralCommandLine commandLine, JpsModule module, ErlangTarget target, CompileContext context) throws ProjectBuildException {
    File outputDirectory = getBuildOutputDirectory(module, target, context);
    commandLine.addParameters("-pa", outputDirectory.getPath());
  }
}
