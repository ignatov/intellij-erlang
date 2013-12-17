package org.intellij.erlang.jps.builder;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.CommonProcessors;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializationException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.jps.model.*;
import org.jdom.Document;
import org.jdom.JDOMException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
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
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.JpsDependencyElement;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.module.JpsModuleDependency;
import org.jetbrains.jps.model.module.JpsModuleSourceRoot;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.*;

/**
 * @author @nik, savenko
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
      public boolean isResourceProcessingEnabled(@NotNull JpsModule module) {
        return !(module.getModuleType() instanceof JpsErlangModuleType);
      }
    });
  }

  @Override
  public void build(@NotNull ErlangTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws ProjectBuildException, IOException {
    LOG.debug(target.getPresentableName());
    if (!holder.hasDirtyFiles() && !holder.hasRemovedFiles()) return;

    JpsModule module = target.getModule();
    JpsProject project = module.getProject();
    ErlangCompilerOptions compilerOptions = JpsErlangCompilerOptionsExtension.getOrCreateExtension(project).getOptions();
    if (compilerOptions.myUseRebarCompiler) return;

    File outputDirectory = getBuildOutputDirectory(module, target.isTests(), context);
    JpsSdk<JpsDummyElement> sdk = getSdk(context, module);
    File executable = JpsErlangSdkType.getByteCodeCompilerExecutable(sdk.getHomePath());
    GeneralCommandLine commandLine = new GeneralCommandLine();
    List<String> erlangModulePaths = getErlangModulePaths(module, target, context);

    commandLine.setWorkDirectory(outputDirectory);
    commandLine.setExePath(executable.getAbsolutePath());
    addCodePath(commandLine, module, target, context);
    addParseTransforms(commandLine, module);
    addDebugInfo(commandLine, erlangModulePaths, outputDirectory, compilerOptions.myAddDebugInfoEnabled);
    addIncludePaths(commandLine, module);
    commandLine.addParameters(erlangModulePaths);

    runBuildProcess(context, commandLine);
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  @NotNull
  private static File getBuildOutputDirectory(@NotNull JpsModule module,
                                              boolean forTests,
                                              @NotNull CompileContext context) throws ProjectBuildException {
    JpsJavaExtensionService instance = JpsJavaExtensionService.getInstance();
    File outputDirectory = instance.getOutputDirectory(module, forTests);
    if (outputDirectory == null) {
      String errorMessage = "No output dir for module " + module.getName();
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }
    if (!outputDirectory.exists()) {
      FileUtil.createDirectory(outputDirectory);
    }
    return outputDirectory;
  }

  @NotNull
  private static JpsSdk<JpsDummyElement> getSdk(@NotNull CompileContext context,
                                                @NotNull JpsModule module) throws ProjectBuildException {
    JpsSdk<JpsDummyElement> sdk = module.getSdk(JpsErlangSdkType.INSTANCE);
    if (sdk == null) {
      String errorMessage = "No SDK for module " + module.getName();
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }
    return sdk;
  }

  private static void runBuildProcess(@NotNull CompileContext context,
                                      @NotNull GeneralCommandLine commandLine) throws ProjectBuildException {
    Process process;
    try {
      process = commandLine.createProcess();
    } catch (ExecutionException e) {
      throw new ProjectBuildException("Failed to launch erlang compiler", e);
    }
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, commandLine.getCommandLineString(), Charset.defaultCharset());
    ProcessAdapter adapter = new ErlangCompilerProcessAdapter(context, NAME, "");
    handler.addProcessListener(adapter);
    handler.startNotify();
    handler.waitFor();
  }

  private static void addDebugInfo(@NotNull GeneralCommandLine commandLine,
                                   @NotNull List<String> modulePaths,
                                   File outputDirectory,
                                   boolean addDebugInfoEnabled) throws ProjectBuildException {
    if (!addDebugInfoEnabled) return;
    commandLine.addParameter("+debug_info");
    try {
      for (String modulePath : modulePaths) {
        File srcFile = new File(modulePath);
        File dstFile = new File(outputDirectory, srcFile.getName());
        FileUtil.copy(srcFile, dstFile);
      }
    } catch (IOException e) {
      throw new ProjectBuildException("Failed to copy sources to output directory.", e);
    }
  }

  private static void addIncludePaths(@NotNull GeneralCommandLine commandLine, @Nullable JpsModule module) {
    JpsErlangModuleExtension extension = JpsErlangModuleExtension.getExtension(module);
    if (extension != null) {
      for (String includePath : extension.getIncludePaths()) {
        commandLine.addParameters("-I", includePath);
      }
    }
  }

  @NotNull
  private static List<String> getErlangModulePaths(@NotNull JpsModule module,
                                                   @NotNull ErlangTarget target,
                                                   @NotNull CompileContext context) {
    List<String> moduleFiles = getErlangModulePathsFromConfig(module, target, context);
    return moduleFiles != null ? moduleFiles : getErlangModulePathsDefault(module, target);
  }

  @NotNull
  private static List<String> getErlangModulePathsDefault(@NotNull JpsModule module, @NotNull ErlangTarget target) {
    CommonProcessors.CollectProcessor<File> erlFilesCollector = new CommonProcessors.CollectProcessor<File>() {
      @Override
      protected boolean accept(@NotNull File file) {
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
    return ContainerUtil.map(erlFilesCollector.getResults(), new Function<File, String>() {
      @NotNull
      @Override
      public String fun(@NotNull File file) {
        return file.getAbsolutePath();
      }
    });
  }

  @Nullable
  private static List<String> getErlangModulePathsFromConfig(@NotNull JpsModule module,
                                                             @NotNull ErlangTarget target,
                                                             @NotNull CompileContext context) {
    File dataStorageRoot = context.getProjectDescriptor().dataManager.getDataPaths().getDataStorageRoot();
    File depsConfigFile = new File(dataStorageRoot, DEPENDENCIES_CONFIG_FILE_PATH);
    if (!depsConfigFile.exists()) return null;
    ErlangModuleBuildOrders buildOrders;
    try {
      Document document = JDOMUtil.loadDocument(depsConfigFile);
      buildOrders = XmlSerializer.deserialize(document, ErlangModuleBuildOrders.class);
    } catch (XmlSerializationException e) {
      return null;
    } catch (JDOMException e) {
      return null;
    } catch (IOException e) {
      return null;
    }
    if (buildOrders == null) return null;
    for (ErlangModuleBuildOrderDescriptor buildOrder : buildOrders.myModuleBuildOrderDescriptors) {
      if (StringUtil.equals(buildOrder.myModuleName, module.getName())) {
        List<String> modules = buildOrder.myOrderedErlangModulePaths;
        if (target.isTests()) {
          modules = ContainerUtil.concat(modules, buildOrder.myOrderedErlangTestModulePaths);
        }
        return modules;
      }
    }
    return null;
  }

  private static void addParseTransforms(@NotNull GeneralCommandLine commandLine,
                                         @Nullable JpsModule module) throws ProjectBuildException {
    JpsErlangModuleExtension extension = JpsErlangModuleExtension.getExtension(module);
    List<String> parseTransforms = extension != null ? extension.getParseTransforms() : Collections.<String>emptyList();
    if (parseTransforms.isEmpty()) return;
    for (String ptModule : parseTransforms) {
      commandLine.addParameter("+{parse_transform, " + ptModule + "}");
    }
  }

  private static void addCodePath(@NotNull GeneralCommandLine commandLine,
                                  @NotNull JpsModule module,
                                  @NotNull ErlangTarget target,
                                  @NotNull CompileContext context) throws ProjectBuildException {
    ArrayList<JpsModule> codePathModules = new ArrayList<JpsModule>();
    collectDependentModules(module, codePathModules, new HashSet<String>());

    addModuleToCodePath(commandLine, module, target.isTests(), context);
    for (JpsModule codePathModule : codePathModules) {
      if (codePathModule != module) {
        addModuleToCodePath(commandLine, codePathModule, false, context);
      }
    }
  }

  private static void collectDependentModules(@NotNull JpsModule module,
                                              @NotNull Collection<JpsModule> addedModules,
                                              @NotNull Set<String> addedModuleNames) {
    String moduleName = module.getName();
    if (addedModuleNames.contains(moduleName)) return;
    addedModuleNames.add(moduleName);
    addedModules.add(module);
    for (JpsDependencyElement dependency : module.getDependenciesList().getDependencies()) {
      if (!(dependency instanceof JpsModuleDependency)) continue;
      JpsModuleDependency moduleDependency = (JpsModuleDependency) dependency;
      JpsModule depModule = moduleDependency.getModule();
      if (depModule != null) {
        collectDependentModules(depModule, addedModules, addedModuleNames);
      }
    }
  }

  private static void addModuleToCodePath(@NotNull GeneralCommandLine commandLine,
                                          @NotNull JpsModule module,
                                          boolean forTests,
                                          @NotNull CompileContext context) throws ProjectBuildException {
    File outputDirectory = getBuildOutputDirectory(module, forTests, context);
    commandLine.addParameters("-pa", outputDirectory.getPath());
    for (String rootUrl : module.getContentRootsList().getUrls()) {
      try {
        String path = new URL(rootUrl).getPath();
        commandLine.addParameters("-pa", path);
      } catch (MalformedURLException e) {
        context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "Failed to find content root for module: " + module.getName()));
      }
    }
  }
}
