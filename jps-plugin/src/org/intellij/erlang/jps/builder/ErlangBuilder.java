package org.intellij.erlang.jps.builder;

import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.util.CommonProcessors;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
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
import org.jetbrains.jps.model.module.JpsModule;
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
    JpsJavaExtensionService instance = JpsJavaExtensionService.getInstance();

    File outputDirectory = instance.getOutputDirectory(module, target.isTests());
    if (outputDirectory == null) {
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "No output dir for module " + module.getName()));
      throw new ProjectBuildException();
    }

    if (!outputDirectory.exists()) FileUtil.createDirectory(outputDirectory);

    JpsSdk<JpsDummyElement> sdk = module.getSdk(JpsErlangSdkType.INSTANCE);
    if (sdk == null) {
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "No SDK for module " + module.getName()));
      throw new ProjectBuildException();
    }

    File executable = JpsErlangSdkType.getByteCodeCompilerExecutable(sdk.getHomePath());

    List<String> commandList = new ArrayList<String>();
    commandList.add(executable.getAbsolutePath());

    CommonProcessors.CollectProcessor<File> processor = new CommonProcessors.CollectProcessor<File>() {
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
      commandList.add("-I");
      commandList.add(root.getFile().getAbsolutePath());
      FileUtil.processFilesRecursively(root.getFile(), processor);
    }

    for (File f : processor.getResults()) {
      commandList.add(f.getAbsolutePath());
    }

    LOG.debug(StringUtil.join(commandList, " "));
    Process process = new ProcessBuilder(commandList).directory(outputDirectory).start();
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, null, Charset.defaultCharset());
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

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }
}
