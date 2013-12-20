package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.project.Project;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.utils.ErlangExternalToolsNotificationListener;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class RebarRunningStateUtil {
  private RebarRunningStateUtil() {
  }

  public static GeneralCommandLine getRebarCommandLine(@NotNull RebarRunConfigurationBase configuration) {
    Project project = configuration.getProject();
    RebarSettings rebarSettings = RebarSettings.getInstance(project);
    GeneralCommandLine commandLine = new GeneralCommandLine();

    commandLine.setWorkDirectory(project.getBasePath());
    commandLine.setExePath(rebarSettings.getRebarPath());

    List<String> split = ContainerUtil.list(configuration.getCommand().split("\\s+"));
    if (configuration.isSkipDependencies() && !split.contains("skip_deps=true")) {
      commandLine.addParameter("skip_deps=true");
    }
    commandLine.addParameters(split);

    return commandLine;
  }

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
}