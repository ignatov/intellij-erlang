package org.intellij.erlang.jps.rebar;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.jps.builder.ErlangCompilerProcessAdapter;
import org.intellij.erlang.jps.builder.ErlangSourceRootDescriptor;
import org.intellij.erlang.jps.builder.ErlangTarget;
import org.intellij.erlang.jps.builder.ErlangTargetType;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangCompilerOptionsExtension;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.module.JpsModule;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Collections;

public class RebarBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  private static final String NAME = "rebar";
  private static final String REBAR_CONFIG_FILE_NAME = "rebar.config";
  public RebarBuilder() {
    super(Collections.singleton(ErlangTargetType.PRODUCTION));
  }

  @Override
  public void build(@NotNull ErlangTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws ProjectBuildException, IOException {
    if (!holder.hasDirtyFiles() && !holder.hasRemovedFiles()) return;

    JpsModule module = target.getModule();
    JpsProject project = module.getProject();
    ErlangCompilerOptions compilerOptions = JpsErlangCompilerOptionsExtension.getOrCreateExtension(project).getOptions();
    if (!compilerOptions.myUseRebarCompiler) return;

    String rebarExecutablePath = getRebarExecutablePath(project);
    if (rebarExecutablePath == null) {
      String errorMessage = "Rebar path is not set";
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }

    for (String contentRootUrl : module.getContentRootsList().getUrls()) {
      String contentRootPath = new URL(contentRootUrl).getPath();
      File contentRootDir = new File(contentRootPath);
      File rebarConfigFile = new File(contentRootDir, REBAR_CONFIG_FILE_NAME);
      if (!rebarConfigFile.exists()) continue;
      runRebar(contentRootPath, rebarExecutablePath, compilerOptions.myAddDebugInfoEnabled, context);
    }
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  private static void runRebar(@Nullable String contentRootPath,
                               @NotNull String rebarPath,
                               boolean addDebugInfo,
                               @NotNull final CompileContext context) throws ProjectBuildException {
    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setWorkDirectory(contentRootPath);
    commandLine.setExePath(rebarPath);
    commandLine.addParameter("compile");

    if (addDebugInfo) {
      commandLine.getEnvironment().put("ERL_FLAGS", "+debug_info");
    }

    Process process;
    try {
      process = commandLine.createProcess();
    } catch (ExecutionException e) {
      throw new ProjectBuildException("Failed to run rebar", e);
    }
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, commandLine.getCommandLineString(), Charset.defaultCharset());
    ProcessAdapter adapter = new ErlangCompilerProcessAdapter(context, NAME, commandLine.getWorkDirectory().getPath()); //TODO provide rebar messages handling
    handler.addProcessListener(adapter);
    handler.startNotify();
    handler.waitFor();
  }

  @Nullable
  private static String getRebarExecutablePath(@Nullable JpsProject project) {
    JpsRebarConfigurationExtension rebarConfigurationExtension = JpsRebarConfigurationExtension.getExtension(project);
    String rebarPath = rebarConfigurationExtension != null ? rebarConfigurationExtension.getRebarPath() : null;
    return StringUtil.isEmptyOrSpaces(rebarPath) ? null : rebarPath;
  }
}
