/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.KillableColoredProcessHandler;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ScriptRunnerUtil;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangExternalToolsNotificationListener;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class RebarRunningStateUtil {
  private static final String REBAR = "rebar3";
  private static final Logger LOG = Logger.getInstance(RebarRunningStateUtil.class);

  private RebarRunningStateUtil() {
  }

  @NotNull
  public static GeneralCommandLine getRebarCommandLine(@NotNull RebarRunConfigurationBase configuration) {
    Project project = configuration.getProject();
    RebarSettings rebarSettings = RebarSettings.getInstance(project);
    String sdkPath = ErlangSdkType.getSdkPath(project);
    String escriptPath = sdkPath != null ?
                         JpsErlangSdkType.getScriptInterpreterExecutable(sdkPath).getAbsolutePath() :
                         findEscriptExecutable();
    GeneralCommandLine commandLine = new GeneralCommandLine();

    commandLine.withWorkDirectory(getWorkingDirectory(configuration));
    commandLine.setExePath(escriptPath);
    commandLine.addParameter(rebarSettings.getRebarPath());

    List<String> split = Arrays.asList(configuration.getCommand().split("\\s+"));
    if (!rebarSettings.isRebar3() && configuration.isSkipDependencies() && !split.contains("skip_deps=true")) {
      commandLine.addParameter("skip_deps=true");
    }
    commandLine.addParameters(split);
    commandLine.withEnvironment(configuration.getEnvData().getEnvs());

    return commandLine;
  }

  @NotNull
  public static OSProcessHandler runRebar(Project project, GeneralCommandLine commandLine) throws ExecutionException {
    try {
      return new KillableColoredProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
    }
    catch (ExecutionException e) {
      String message = e.getMessage();
      boolean isEmpty = message.equals("Executable is not specified");
      boolean notCorrect = message.startsWith("Cannot run program");
      if (isEmpty || notCorrect) {
        Notifications.Bus.notify(
          new Notification("Rebar run configuration", "Rebar settings",
                           "Rebar executable path is " + (isEmpty ? "empty" : "not specified correctly") +
                           "<br/><a href='configure'>Configure</a>",
                           NotificationType.ERROR, new ErlangExternalToolsNotificationListener(project)), project);
      }
      throw e;
    }
  }

  @NotNull
  public static String getWorkingDirectory(@NotNull RebarRunConfigurationBase configuration) {
    Module module = configuration.getConfigurationModule().getModule();
    if (module != null) {
      VirtualFile[] contentRoots = ModuleRootManager.getInstance(module).getContentRoots();
      if (contentRoots.length >= 1) {
        return contentRoots[0].getPath();
      }
    }
    return ObjectUtils.notNull(configuration.getProject().getBasePath(), "");
  }

  @NotNull
  public static String getRebarPath(@Nullable String directory) {
    if (directory != null) {
      File rebar = new File(directory, REBAR);
      if (rebar.exists() && rebar.canExecute()) {
        return rebar.getPath();
      }
    }
    return which(REBAR);
  }

  @NotNull
  public static String findEscriptExecutable() {
    String which = which(JpsErlangSdkType.SCRIPT_INTERPRETER);
    if (StringUtil.isNotEmpty(which)) return which;
    return JpsErlangSdkType.getExecutableFileName(JpsErlangSdkType.SCRIPT_INTERPRETER);
  }

  @NotNull
  private static String which(@NotNull String name) {
    if (!(SystemInfo.isMac || SystemInfo.isLinux || SystemInfo.isUnix)) {
      return "";
    }

    GeneralCommandLine command = new GeneralCommandLine("which");
    command.addParameter(name);

    try {
      String output;
      if (ApplicationManager.getApplication().isDispatchThread()) {
        output = ApplicationManager.getApplication()
                   .executeOnPooledThread(() -> getProcessOutputSafe(command))
                   .get();
      } else {
        output = getProcessOutputSafe(command);
      }
      return output.trim();
    } catch (Exception e) {
      return "";
    }
  }

  private static String getProcessOutputSafe(GeneralCommandLine command) {
    try {
      return ScriptRunnerUtil.getProcessOutput(command);
    } catch (Exception e) {
      LOG.warn(e);
      return "";
    }
  }
}