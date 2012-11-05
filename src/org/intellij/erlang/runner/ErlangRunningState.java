package org.intellij.erlang.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.compiler.CompilerPaths;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangRunningState extends CommandLineState {
  private final Module module;
  private final ErlangApplicationConfiguration myConfiguration;

  public ErlangRunningState(ExecutionEnvironment env, Module module, ErlangApplicationConfiguration configuration) {
    super(env);
    this.module = module;
    myConfiguration = configuration;
  }

  @NotNull
  @Override
  protected ProcessHandler startProcess() throws ExecutionException {
    final Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
    assert sdk != null;

    GeneralCommandLine commandLine = getCommand(sdk);

    return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
  }

  private GeneralCommandLine getCommand(Sdk sdk) {
    final GeneralCommandLine commandLine = new GeneralCommandLine();
    VirtualFile moduleOutputDirectory = CompilerPaths.getModuleOutputDirectory(module, false);
    String erl = FileUtil.toSystemDependentName(ErlangSdkType.getTopLevelExecutable(sdk.getHomePath()).getAbsolutePath());
    String canonicalPath = moduleOutputDirectory.getCanonicalPath();
    commandLine.setWorkDirectory(canonicalPath);

    commandLine.setExePath(erl);
    commandLine.addParameters("-run");
    commandLine.addParameters(StringUtil.split(myConfiguration.getModuleAndFunction(), " "));
    commandLine.addParameters(StringUtil.split(myConfiguration.getParams(), " "));
    commandLine.addParameters("-s", "init", "stop", "-noshell");

    final TextConsoleBuilder consoleBuilder = TextConsoleBuilderFactory.getInstance().createBuilder(module.getProject());
    setConsoleBuilder(consoleBuilder);
    return commandLine;
  }
}
