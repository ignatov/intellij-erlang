package org.intellij.erlang.jps.builder;

import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.util.CommonProcessors;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.builders.FileProcessor;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.model.JpsDummyElement;
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
  }

  @Override
  public void build(@NotNull ErlangTarget target, 
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder, 
                    @NotNull BuildOutputConsumer outputConsumer, 
                    @NotNull final CompileContext context) throws ProjectBuildException, IOException {
    LOG.debug(target.getPresentableName());
    final Ref<Boolean> hasDirtyFiles = Ref.create(false);
    holder.processDirtyFiles(new FileProcessor<ErlangSourceRootDescriptor, ErlangTarget>() {
      @Override
      public boolean apply(ErlangTarget target, File file, ErlangSourceRootDescriptor root) throws IOException {
        hasDirtyFiles.set(true);
        return true;
      }
    });
    if (!hasDirtyFiles.get() && !holder.hasRemovedFiles()) {
      return;
    }

    JpsModule module = target.getModule();
    JpsJavaExtensionService instance = JpsJavaExtensionService.getInstance();

    File outputDirectory = instance.getOutputDirectory(module, target.isTests());
    if (outputDirectory == null) {
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "No output dir for module " + module.getName()));
      throw new ProjectBuildException();
    }

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
    for (JpsModuleSourceRoot root : module.getSourceRoots()) {
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
