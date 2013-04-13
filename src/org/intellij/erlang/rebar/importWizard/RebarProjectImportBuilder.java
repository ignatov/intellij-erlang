/*
 * Copyright 2012 Sergey Ignatov
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

import com.google.common.collect.Sets;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.RunManagerEx;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.process.*;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationInfoEx;
import com.intellij.openapi.module.ModifiableModuleModel;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkTypeId;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.ex.ProjectRootManagerEx;
import com.intellij.openapi.roots.ui.configuration.ModulesProvider;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileVisitor;
import com.intellij.openapi.vfs.newvfs.impl.VirtualDirectoryImpl;
import com.intellij.packaging.artifacts.ModifiableArtifactModel;
import com.intellij.projectImport.ProjectImportBuilder;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.apache.log4j.Logger;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.editor.ErlangModuleType;
import org.intellij.erlang.rebar.runner.RebarRunConfiguration;
import org.intellij.erlang.rebar.runner.RebarRunConfigurationFactory;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RebarProjectImportBuilder extends ProjectImportBuilder<ImportedOtpApp> {
  private static final Pattern APP_NAME_PATTERN = Pattern.compile("\\{\\s*application\\s*,\\s*(.*?)\\s*,");
  private static final Pattern APP_DEPS_LIST_PATTERN = Pattern.compile("\\{\\s*applications\\s*,\\s*\\[\\s*(.*?)\\s*\\]", Pattern.DOTALL);
  private static final Pattern DIRS_LIST_PATTERN = Pattern.compile("\\{\\s*sub_dirs\\s*,\\s*\\[\\s*(.*?)\\s*\\]", Pattern.DOTALL);
  private static final Pattern SPLIT_DIR_LIST_PATTERN = Pattern.compile("\"\\s*(,\\s*\")?");

  private static final Logger LOG = Logger.getLogger(RebarProjectImportBuilder.class);

  private boolean myOpenProjectSettingsAfter = false;
  @Nullable private VirtualFile myProjectRoot = null;
  @NotNull private List<ImportedOtpApp> myFoundOtpApps = Collections.emptyList();
  @NotNull private List<ImportedOtpApp> mySelectedOtpApps = Collections.emptyList();
  private boolean myImportExamples;

  @NotNull
  @NonNls
  @Override
  public String getName() {
    return "Rebar";
  }

  @Override
  public Icon getIcon() {
    return ErlangIcons.REBAR;
  }

  @Override
  public boolean isSuitableSdkType(@NotNull SdkTypeId sdkType) {
    return sdkType == ErlangSdkType.getInstance();
  }

  @Override
  public List<ImportedOtpApp> getList() {
    return new ArrayList<ImportedOtpApp>(myFoundOtpApps);
  }

  @Override
  public void setList(@Nullable List<ImportedOtpApp> selectedOtpApps) throws ConfigurationException {
    if (selectedOtpApps != null) {
      mySelectedOtpApps = selectedOtpApps;
    }
  }

  @Override
  public boolean isMarked(@Nullable ImportedOtpApp importedOtpApp) {
    return importedOtpApp != null && mySelectedOtpApps.contains(importedOtpApp);
  }

  @Override
  public boolean isOpenProjectSettingsAfter() {
    return myOpenProjectSettingsAfter;
  }

  @Override
  public void setOpenProjectSettingsAfter(boolean openProjectSettingsAfter) {
    myOpenProjectSettingsAfter = openProjectSettingsAfter;
  }

  @Override
  public void cleanup() {
    myOpenProjectSettingsAfter = false;
    myProjectRoot = null;
    myFoundOtpApps = Collections.emptyList();
    mySelectedOtpApps = Collections.emptyList();
  }
  
  public static void fetchDependencies(@NotNull final VirtualFile projectRoot) {
    ProgressManager.getInstance().run(new Task.Modal(getCurrentProject(), "Fetching dependencies", true) {
      public void run(@NotNull final ProgressIndicator indicator) {
        String rebarExecutable = getRebarExecutable(projectRoot.getCanonicalPath());
        if (StringUtil.isEmptyOrSpaces(rebarExecutable)) return;
        
        indicator.setIndeterminate(true);
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setExePath(rebarExecutable);
        commandLine.setWorkDirectory(projectRoot.getCanonicalPath());
        commandLine.addParameter("get-deps");
        try {
          OSProcessHandler handler = new OSProcessHandler(commandLine.createProcess(), commandLine.getPreparedCommandLine());
          handler.addProcessListener(new ProcessAdapter() {
            @Override
            public void onTextAvailable(ProcessEvent event, Key outputType) {
              String text = event.getText();
              indicator.setText2(text);
            }
          });
          ProcessTerminatedListener.attach(handler);
          handler.startNotify();
          handler.waitFor();
          indicator.setText2("Refreshing");
        } catch (ExecutionException e) {
          LOG.warn(e);
        }
      }
    });
  }

  @NotNull
  public static String getRebarExecutable(@Nullable String directory) {
    boolean isPosix = SystemInfo.isMac || SystemInfo.isLinux || SystemInfo.isUnix;
    if (!isPosix) return "";

    if (directory != null) {
      File rebar = new File(directory, "rebar");
      if (rebar.exists() && rebar.canExecute()) {
        return rebar.getPath();
      }
    }

    String output = "";
    try {
      GeneralCommandLine which = new GeneralCommandLine("which");
      which.addParameter("rebar");
      output = ScriptRunnerUtil.getProcessOutput(which);
    } catch (Exception ignored) {
    }
    return output;
  }

  public boolean setProjectRoot(@NotNull final VirtualFile projectRoot) {
    if (projectRoot.equals(myProjectRoot)) {
      return true;
    }
    
    final boolean unitTestMode = ApplicationManager.getApplication().isUnitTestMode();
    
    myProjectRoot = projectRoot;
    if (!unitTestMode && projectRoot instanceof VirtualDirectoryImpl) {
      ((VirtualDirectoryImpl) projectRoot).refreshAndFindChild("deps");
    }

    ProgressManager.getInstance().run(new Task.Modal(getCurrentProject(), "Scanning Rebar projects", true) {
      public void run(@NotNull final ProgressIndicator indicator) {

        final List<VirtualFile> rebarConfigFiles = findRebarConfigs(myProjectRoot, indicator);
        final LinkedHashSet<ImportedOtpApp> importedOtpApps = new LinkedHashSet<ImportedOtpApp>(rebarConfigFiles.size());

        if (!unitTestMode) {
          VfsUtilCore.visitChildrenRecursively(projectRoot, new VirtualFileVisitor() {
            @Override
            public boolean visitFile(@NotNull VirtualFile file) {
              indicator.checkCanceled();
  
              if (file.isDirectory()) {
                indicator.setText2(file.getPath());
                if (isExamplesDirectory(file)) return false;
              }

              ContainerUtil.addAllNotNull(importedOtpApps, createImportedOtpApp(file));
              return true;
            }
          });
        }

        for (VirtualFile rebarConfigFile : rebarConfigFiles) {
          resolveOtpAppsByRebarConfig(rebarConfigFile, importedOtpApps);
        }
        myFoundOtpApps = ContainerUtil.newArrayList(importedOtpApps);
      }
    });

    Collections.sort(myFoundOtpApps, new Comparator<ImportedOtpApp>() {
      @Override
      public int compare(ImportedOtpApp o1, ImportedOtpApp o2) {
        final int nameCompareResult = String.CASE_INSENSITIVE_ORDER.compare(o1.getName(), o2.getName());
        if (nameCompareResult == 0) {
          return String.CASE_INSENSITIVE_ORDER.compare(o1.getRoot().getPath(), o2.getRoot().getPath());
        }
        return nameCompareResult;
      }
    });

    mySelectedOtpApps = myFoundOtpApps;
    return !myFoundOtpApps.isEmpty();
  }

  @SuppressWarnings("DialogTitleCapitalization")
  @Override
  public boolean validate(Project current, Project dest) {
    if (!findIdeaModuleFiles(mySelectedOtpApps)) {
      return true;
    }
    final int resultCode = Messages.showYesNoCancelDialog(
      ApplicationInfoEx.getInstanceEx().getFullApplicationName() + " module files found:\n\n" +
        StringUtil.join(mySelectedOtpApps, new Function<ImportedOtpApp, String>() {
          public String fun(ImportedOtpApp importedOtpApp) {
            final VirtualFile ideaModuleFile = importedOtpApp.getIdeaModuleFile();
            return ideaModuleFile != null ? "    " + ideaModuleFile.getPath() + "\n" : "";
          }
        }, "") +
        "\nWould you like to reuse them?", "Module files found",
      Messages.getQuestionIcon());
    if (resultCode == DialogWrapper.OK_EXIT_CODE) {
      return true;
    }
    else if (resultCode == DialogWrapper.CANCEL_EXIT_CODE) {
      try {
        deleteIdeaModuleFiles(mySelectedOtpApps);
        return true;
      } catch (IOException e) {
        LOG.error(e);
        return false;
      }
    }
    else {
      return false;
    }
  }

  @Override
  public List<Module> commit(@NotNull Project project,
                             @Nullable ModifiableModuleModel moduleModel,
                             @NotNull ModulesProvider modulesProvider,
                             @Nullable ModifiableArtifactModel modifiableArtifactModel) {
    final Set<String> selectedAppNames = Sets.newHashSet();
    for (ImportedOtpApp importedOtpApp : mySelectedOtpApps) {
      selectedAppNames.add(importedOtpApp.getName());
    }
    final Sdk projectSdk = fixProjectSdk(project);
    final List<Module> createdModules = new ArrayList<Module>();
    final List<ModifiableRootModel> createdRootModels = new ArrayList<ModifiableRootModel>();
    final ModifiableModuleModel obtainedModuleModel =
      (moduleModel != null ? moduleModel : ModuleManager.getInstance(project).getModifiableModel());
    for (ImportedOtpApp importedOtpApp : mySelectedOtpApps) {
      final VirtualFile ideaModuleDir = importedOtpApp.getRoot();
      final String ideaModuleFile = ideaModuleDir.getCanonicalPath() + File.separator + importedOtpApp.getName() + ".iml";
      final Module module = obtainedModuleModel.newModule(ideaModuleFile, ErlangModuleType.getInstance().getId());
      createdModules.add(module);
      if (importedOtpApp.getIdeaModuleFile() == null) {
        final ModifiableRootModel rootModel = ModuleRootManager.getInstance(module).getModifiableModel();
        // Make it inherit SDK from the project.
        rootModel.inheritSdk();
        // Initialize source and test paths.
        final ContentEntry content = rootModel.addContentEntry(importedOtpApp.getRoot());
        addSourceDirToContent(content, ideaModuleDir, "src", false);
        addSourceDirToContent(content, ideaModuleDir, "include", false);
        addSourceDirToContent(content, ideaModuleDir, "test", true);
        // Exclude standard folders
        excludeDirFromContent(content, ideaModuleDir, "doc");
        // Initialize output paths according to Rebar conventions.
        final CompilerModuleExtension compilerModuleExt = rootModel.getModuleExtension(CompilerModuleExtension.class);
        compilerModuleExt.inheritCompilerOutputPath(false);
        compilerModuleExt.setCompilerOutputPath(ideaModuleDir + File.separator + "ebin");
        compilerModuleExt.setCompilerOutputPathForTests(ideaModuleDir + File.separator + ".eunit");
        createdRootModels.add(rootModel);
        // Set inter-module dependencies
        resolveModuleDeps(rootModel, importedOtpApp, projectSdk, selectedAppNames);
        // Commit module if model is given.
        if (moduleModel != null) {
          ApplicationManager.getApplication().runWriteAction(new Runnable() {
            public void run() {
              rootModel.commit();
            }
          });
        }
      }
    }
    // Commit project structure.
    LOG.info("Commit project structure");
    if (moduleModel == null) {
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        public void run() {
          for (ModifiableRootModel rootModel : createdRootModels) {
            rootModel.commit();
          }
          obtainedModuleModel.commit();
        }
      });
    }

    RebarRunConfigurationFactory rebarFactory = RebarRunConfigurationFactory.getInstance();
    final RunManagerEx runManager = RunManagerEx.getInstanceEx(project);
    RunnerAndConfigurationSettings runnerAndSettings = runManager.createConfiguration("Compile", rebarFactory);
    RunConfiguration configuration = runnerAndSettings.getConfiguration();
    if (configuration instanceof RebarRunConfiguration) {
      ((RebarRunConfiguration)configuration).setCommand("compile");
    }
    runManager.addConfiguration(runnerAndSettings, false);


    String canonicalPath = myProjectRoot != null ? myProjectRoot.getCanonicalPath() : null;
    String rebarExecutable = getRebarExecutable(canonicalPath);
    if (!StringUtil.isEmptyOrSpaces(rebarExecutable)) {
      RebarSettings.getInstance(project).setRebarPath(rebarExecutable);
    }

    return createdModules;
  }

  @Nullable
  private static Sdk fixProjectSdk(@NotNull Project project) {
    final ProjectRootManagerEx projectRootMgr = ProjectRootManagerEx.getInstanceEx(project);
    final Sdk selectedSdk = projectRootMgr.getProjectSdk();
    if (selectedSdk == null || selectedSdk.getSdkType() != ErlangSdkType.getInstance()) {
      final Sdk moreSuitableSdk = ProjectJdkTable.getInstance().findMostRecentSdkOfType(ErlangSdkType.getInstance());
      projectRootMgr.setProjectSdk(moreSuitableSdk);
      return moreSuitableSdk;
    }
    return selectedSdk;
  }

  private static void addSourceDirToContent(@NotNull ContentEntry content,
                                            @NotNull VirtualFile root,
                                            @NotNull String sourceDir,
                                            boolean test) {
    final VirtualFile sourceDirFile = root.findChild(sourceDir);
    if (sourceDirFile != null) {
      content.addSourceFolder(sourceDirFile, test);
    }
  }

  private static void excludeDirFromContent(ContentEntry content, VirtualFile root, String excludeDir) {
    final VirtualFile excludeDirFile = root.findChild(excludeDir);
    if (excludeDirFile != null) {
      content.addExcludeFolder(excludeDirFile);
    }
  }

  @NotNull
  private List<VirtualFile> findRebarConfigs(@NotNull VirtualFile root, @NotNull final ProgressIndicator indicator) {
    root.refresh(false, true);
    final List<VirtualFile> foundRebarConfigs = new ArrayList<VirtualFile>();
    VfsUtilCore.visitChildrenRecursively(root, new VirtualFileVisitor() {
      @Override
      public boolean visitFile(@NotNull VirtualFile virtualFile) {
        indicator.checkCanceled();
        if (virtualFile.isDirectory()) {
          if (isExamplesDirectory(virtualFile)) return false;
          indicator.setText2(virtualFile.getPath());
        }
        else if (virtualFile.getName().equalsIgnoreCase("rebar.config")) {
          foundRebarConfigs.add(virtualFile);
        }
        return true;
      }
    });

    return foundRebarConfigs;
  }

  private boolean isExamplesDirectory(VirtualFile virtualFile) {
    return "examples".equals(virtualFile.getName()) && !myImportExamples;
  }

  private static void resolveOtpAppsByRebarConfig(@NotNull VirtualFile rebarConfigFile,
                                                  @NotNull Collection<ImportedOtpApp> importedOtpApps) {
    final VirtualFile root = rebarConfigFile.getParent();

    final List<VirtualFile> tentativeAppRoots = new ArrayList<VirtualFile>();
    tentativeAppRoots.add(root);
    try {
      final String content = new String(rebarConfigFile.contentsToByteArray());
      final Matcher matcher = DIRS_LIST_PATTERN.matcher(content);
      if (matcher.find()) {
        final String subDirsValue = matcher.group(1);
        final String[] relativeSubDirs = SPLIT_DIR_LIST_PATTERN.split(subDirsValue);
        for (String relativeSubDir : relativeSubDirs) {
          ContainerUtil.addAllNotNull(tentativeAppRoots, root.findFileByRelativePath(relativeSubDir));
        }
      }
    } catch (IOException e) { // Ignore
    }

    for (VirtualFile tentativeAppRoot : tentativeAppRoots) {
      final ImportedOtpApp importedOtpApp = createImportedOtpApp(tentativeAppRoot);
      if (importedOtpApp != null) {
        if (!importedOtpApps.contains(importedOtpApp)) {
          importedOtpApps.add(importedOtpApp);
        }
      }
    }
  }

  @Nullable
  private static ImportedOtpApp createImportedOtpApp(@NotNull VirtualFile appRoot) {
    final VirtualFile appResourceFile = findAppResourceFile(appRoot);
    if (appResourceFile == null) {
      return null;
    }
    final String content;
    try {
      content = new String(appResourceFile.contentsToByteArray());
    } catch (IOException e) {
      return null;
    }
    final Matcher appNameMatcher = APP_NAME_PATTERN.matcher(content);
    if (!appNameMatcher.find()) {
      return null;
    }
    final String appName = appNameMatcher.group(1);
    final Matcher appDepsMatcher = APP_DEPS_LIST_PATTERN.matcher(content);
    final Set<String> appDeps = new HashSet<String>();
    if (appDepsMatcher.find()) {
      for (String appDepName : appDepsMatcher.group(1).trim().split("\\s*,\\s*")) {
        if (!appDepName.isEmpty()) {
          appDeps.add(appDepName);
        }
      }
    }
    return new ImportedOtpApp(appName, appRoot, appDeps);
  }

  @Nullable
  private static VirtualFile findAppResourceFile(@NotNull VirtualFile applicationRoot) {
    VirtualFile appResourceFile = null;
    final VirtualFile sourceDir = applicationRoot.findChild("src");
    if (sourceDir != null) {
      appResourceFile = findFileByExtension(sourceDir, "app.src");
    }
    if (appResourceFile == null) {
      final VirtualFile ebinDir = applicationRoot.findChild("ebin");
      if (ebinDir != null) {
        appResourceFile = findFileByExtension(ebinDir, "app");
      }
    }
    return appResourceFile;
  }

  @Nullable
  private static VirtualFile findFileByExtension(@NotNull VirtualFile dir, @NotNull String extension) {
    for (VirtualFile file : dir.getChildren()) {
      final String fileName = file.getName();
      if (!file.isDirectory() && (fileName.endsWith(extension))) {
        return file;
      }
    }
    return null;  //To change body of created methods use File | Settings | File Templates.
  }

  private static void deleteIdeaModuleFiles(@NotNull final List<ImportedOtpApp> importedOtpApps) throws IOException {
    final IOException[] ex = new IOException[1];
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        for (ImportedOtpApp importedOtpApp : importedOtpApps) {
          final VirtualFile ideaModuleFile = importedOtpApp.getIdeaModuleFile();
          if (ideaModuleFile != null) {
            try {
              ideaModuleFile.delete(this);
              importedOtpApp.setIdeaModuleFile(null);
            } catch (IOException e) {
              ex[0] = e;
            }
          }
        }
      }
    });
    if (ex[0] != null) {
      throw ex[0];
    }
  }

  private static boolean findIdeaModuleFiles(@NotNull List<ImportedOtpApp> importedOtpApps) {
    boolean ideaModuleFileExists = false;
    for (ImportedOtpApp importedOtpApp : importedOtpApps) {
      final VirtualFile applicationRoot = importedOtpApp.getRoot();
      final String ideaModuleName = importedOtpApp.getName();
      final VirtualFile imlFile = applicationRoot.findChild(ideaModuleName + ".iml");
      if (imlFile != null) {
        ideaModuleFileExists = true;
        importedOtpApp.setIdeaModuleFile(imlFile);
      }
      else {
        final VirtualFile emlFile = applicationRoot.findChild(ideaModuleName + ".eml");
        if (emlFile != null) {
          ideaModuleFileExists = true;
          importedOtpApp.setIdeaModuleFile(emlFile);
        }
      }
    }
    return ideaModuleFileExists;
  }

  @NotNull
  private static Set<String> resolveModuleDeps(@NotNull ModifiableRootModel rootModel,
                                               @NotNull ImportedOtpApp importedOtpApp,
                                               @Nullable Sdk projectSdk,
                                               @NotNull Set<String> allImportedAppNames) {
    final HashSet<String> unresolvedAppNames = Sets.newHashSet();
    for (String depAppName : importedOtpApp.getDeps()) {
      if (allImportedAppNames.contains(depAppName)) {
        rootModel.addInvalidModuleEntry(depAppName);
      }
      else if (projectSdk != null && isSdkOtpApp(depAppName, projectSdk)) {
        // SDK is already a dependency
      }
      else {
        rootModel.addInvalidModuleEntry(depAppName);
        unresolvedAppNames.add(depAppName);
      }
    }
    return unresolvedAppNames;
  }

  private static boolean isSdkOtpApp(@NotNull String otpAppName, @NotNull Sdk sdk) {
    final Pattern appDirNamePattern = Pattern.compile(otpAppName + "-.*");
    for (VirtualFile srcSdkDir : sdk.getRootProvider().getFiles(OrderRootType.SOURCES)) {
      for (VirtualFile child : srcSdkDir.getChildren()) {
        if (child.isDirectory() && appDirNamePattern.matcher(child.getName()).find()) {
          return true;
        }
      }
    }
    return false;
  }

  public void setImportExamples(boolean importExamples) {
    myImportExamples = importExamples;
  }
}
