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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Platform;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessTerminatedListener;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.progress.util.BackgroundTaskUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.projectImport.ProjectImportBuilder;
import com.intellij.projectImport.ProjectImportWizardStep;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.rebar.runner.RebarRunningStateUtil;
import org.intellij.erlang.rebar.settings.RebarConfigurationForm;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class RebarProjectRootStep extends ProjectImportWizardStep {
  private static final Logger LOG = Logger.getInstance(RebarProjectImportBuilder.class);

  private JPanel myPanel;
  private TextFieldWithBrowseButton myProjectRootComponent;
  private JCheckBox myGetDepsCheckbox;
  private JCheckBox myImportExamplesCheckBox;
  private RebarConfigurationForm myRebarConfigurationForm;
  private static final boolean ourEnabled = !SystemInfo.isWindows;

  public RebarProjectRootStep(WizardContext context) {
    super(context);
    String projectFileDirectory = context.getProjectFileDirectory();
    //noinspection DialogTitleCapitalization
    myProjectRootComponent.addBrowseFolderListener("Select `rebar.config` of a Rebar Project to Import", "", null,
                                                   FileChooserDescriptorFactory.createSingleFolderDescriptor());
    myProjectRootComponent.setText(projectFileDirectory); // provide project path

    myGetDepsCheckbox.setVisible(ourEnabled);
    BackgroundTaskUtil.executeOnPooledThread(myProjectRootComponent, () -> {
      var path = RebarRunningStateUtil.getRebarPath(projectFileDirectory);
      ApplicationManager.getApplication().invokeLater(() -> myRebarConfigurationForm.setPath(path));
    });
  }

  @Override
  public boolean validate() {
    String projectRootPath = myProjectRootComponent.getText();
    if (StringUtil.isEmpty(projectRootPath)) {
      return false;
    }
    VirtualFile projectRoot = LocalFileSystem.getInstance().refreshAndFindFileByPath(projectRootPath);
    if (projectRoot == null) {
      return false;
    }
    if (myGetDepsCheckbox.isSelected() && !ApplicationManager.getApplication().isUnitTestMode()) {
      if (!myRebarConfigurationForm.isPathValid()) {
        return false;
      }
      fetchDependencies(projectRoot, myRebarConfigurationForm.getPath());
    }
    RebarProjectImportBuilder builder = getBuilder();
    builder.setImportExamples(myImportExamplesCheckBox.isSelected());
    builder.setRebarPath(myRebarConfigurationForm.getPath());
    builder.setIsImportingProject(getWizardContext().isCreatingNewProject());
    return builder.setProjectRoot(projectRoot);
  }

  @Override
  @NotNull
  public JComponent getComponent() {
    return myPanel;
  }

  @Override
  public void updateDataModel() {
    String projectRoot = myProjectRootComponent.getText();
    if (!projectRoot.isEmpty()) {
      suggestProjectNameAndPath(null, projectRoot);
    }
  }

  @Override
  @NotNull
  public JComponent getPreferredFocusedComponent() {
    return myProjectRootComponent.getTextField();
  }

  @Override
  @NotNull
  protected RebarProjectImportBuilder getBuilder() {
    return (RebarProjectImportBuilder) super.getBuilder();
  }

  private static void fetchDependencies(@NotNull final VirtualFile projectRoot, @NotNull final String rebarPath) {
    Project project = ProjectImportBuilder.getCurrentProject();
    String sdkPath = project != null ? ErlangSdkType.getSdkPath(project) : null;
    final String escriptPath = sdkPath != null
                               ? JpsErlangSdkType.getScriptInterpreterExecutable(sdkPath).getAbsolutePath()
                               : RebarRunningStateUtil.findEscriptExecutable();

    ProgressManager.getInstance().run(new Task.Modal(project, "Fetching Dependencies", true) {
      public void run(@NotNull final ProgressIndicator indicator) {
        indicator.setIndeterminate(true);
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.withWorkDirectory(projectRoot.getCanonicalPath());
        commandLine.setExePath(escriptPath);
        commandLine.addParameter(rebarPath);
        commandLine.addParameter("get-deps");
        try {
          OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLine.getPreparedCommandLine(Platform.current()));
          handler.addProcessListener(new ProcessAdapter() {
            @Override
            public void onTextAvailable(@NotNull ProcessEvent event, @NotNull Key outputType) {
              String text = event.getText();
              indicator.setText2(text);
            }
          });
          ProcessTerminatedListener.attach(handler);
          handler.startNotify();
          handler.waitFor();
          indicator.setText2("Refreshing");
        }
        catch (ExecutionException e) {
          LOG.warn(e);
        }
      }
    });
  }
}
