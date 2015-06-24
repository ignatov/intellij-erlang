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
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangExternalToolsNotificationListener;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class RebarRunningStateUtil {
  private RebarRunningStateUtil() {
  }

  @NotNull
  public static GeneralCommandLine getRebarCommandLine(@NotNull RebarRunConfigurationBase configuration) throws ExecutionException {
    Project project = configuration.getProject();
    RebarSettings rebarSettings = RebarSettings.getInstance(project);
    String sdkPath = ErlangSdkType.getSdkPath(project);
    String escriptPath = sdkPath != null ?
      JpsErlangSdkType.getScriptInterpreterExecutable(sdkPath).getAbsolutePath() :
      JpsErlangSdkType.getExecutableFileName(JpsErlangSdkType.SCRIPT_INTERPRETER);
    GeneralCommandLine commandLine = new GeneralCommandLine();

    commandLine.withWorkDirectory(getWorkingDirectory(configuration));
    commandLine.setExePath(escriptPath);
    commandLine.addParameter(rebarSettings.getRebarPath());

    List<String> split = ContainerUtil.list(configuration.getCommand().split("\\s+"));
    if (configuration.isSkipDependencies() && !split.contains("skip_deps=true")) {
      commandLine.addParameter("skip_deps=true");
    }
    commandLine.addParameters(split);

    return commandLine;
  }

  @NotNull
  public static OSProcessHandler runRebar(Project project, GeneralCommandLine commandLine) throws ExecutionException {
    try {
      return new OSProcessHandler(commandLine.createProcess(), commandLine.getCommandLineString());
    } catch (ExecutionException e) {
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
    return ObjectUtils.assertNotNull(configuration.getProject().getBasePath());
  }
}