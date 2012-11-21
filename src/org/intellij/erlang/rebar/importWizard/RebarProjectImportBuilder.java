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
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.ui.configuration.ModulesProvider;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileVisitor;
import com.intellij.packaging.artifacts.ModifiableArtifactModel;
import com.intellij.projectImport.ProjectImportBuilder;
import com.intellij.util.Function;
import org.apache.log4j.Logger;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.editor.ErlangModuleType;
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
  static final Pattern APPLICATION_NAME_PATTERN = Pattern.compile("\\{\\s*application\\s*,\\s*(.*?)\\s*,");
  static final Pattern DIRS_LIST_PATTERN = Pattern.compile("\\{\\s*sub_dirs\\s*,\\s*\\[(.*?)\\]");
  static final Pattern SPLIT_DIR_LIST_PATTERN = Pattern.compile("\"\\s*(,\\s*\")?");

  private static final Logger ourLogger = Logger.getLogger(RebarProjectImportBuilder.class);

  private boolean myOpenProjectSettingsAfter = false;
  @Nullable
  private VirtualFile myProjectRoot = null;
  @NotNull
  private List<ImportedOtpApp> myFoundOtpApps = Collections.emptyList();
  @NotNull
  private List<ImportedOtpApp> mySelectedOtpApps = Collections.emptyList();

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
        ourLogger.error(e);
        return false;
      }
    }
    else {
      return false;
    }
  }

  @Override
  public List<Module> commit(@NotNull Project project,
                             @Nullable ModifiableModuleModel modifiableModuleModel,
                             @NotNull ModulesProvider modulesProvider,
                             @Nullable ModifiableArtifactModel modifiableArtifactModel) {
    final List<Module> createdModules = new ArrayList<Module>();
    final ModifiableModuleModel moduleModel = modifiableModuleModel != null
      ? modifiableModuleModel
      : ModuleManager.getInstance(project).getModifiableModel();
    for (ImportedOtpApp importedOtpApp : mySelectedOtpApps) {
      final VirtualFile ideaModuleDir = importedOtpApp.getRoot();
      final String ideaModuleFile = ideaModuleDir.getCanonicalPath() + File.separator + importedOtpApp.getName() + ".iml";
      final Module module = moduleModel.newModule(ideaModuleFile, ErlangModuleType.getInstance());
      createdModules.add(module);
      if (importedOtpApp.getIdeaModuleFile() == null) {
        final ModifiableRootModel rootModel = ModuleRootManager.getInstance(module).getModifiableModel();
        // Initialize source and test paths.
        final ContentEntry content = rootModel.addContentEntry(importedOtpApp.getRoot());
        addSourceDirToContent(content, ideaModuleDir, "src", false);
        addSourceDirToContent(content, ideaModuleDir, "include", false);
        addSourceDirToContent(content, ideaModuleDir, "test", true);
        // Initialize output paths.
        final CompilerModuleExtension compilerModuleExt = rootModel.getModuleExtension(CompilerModuleExtension.class);
        compilerModuleExt.inheritCompilerOutputPath(false);
        compilerModuleExt.setCompilerOutputPath(ideaModuleDir + File.separator + "ebin");
        compilerModuleExt.setCompilerOutputPathForTests(ideaModuleDir + File.separator + ".eunit");
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          public void run() {
            rootModel.commit();
          }
        });
        ourLogger.info("rootModel: " + rootModel);
      }
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        public void run() {
          moduleModel.commit();
        }
      });
    }
    ourLogger.info("commit: " + createdModules);
    return createdModules;
  }

  private static void addSourceDirToContent(ContentEntry content, VirtualFile root, String sourceDir, boolean test) {
    final VirtualFile sourceDirFile = root.findChild(sourceDir);
    if (sourceDirFile != null) {
      content.addSourceFolder(sourceDirFile, test);
    }
  }

  boolean setProjectRoot(@NotNull VirtualFile projectRoot) {
    if (projectRoot.equals(myProjectRoot)) {
      return true;
    }
    myProjectRoot = projectRoot;
    ProgressManager.getInstance().run(new Task.Modal(getCurrentProject(), "Scanning Rebar projects", true) {
      public void run(@NotNull ProgressIndicator indicator) {
        final List<VirtualFile> rebarConfigFiles = findRebarConfigs(myProjectRoot, indicator);
        final List<ImportedOtpApp> importedOtpApps = new ArrayList<ImportedOtpApp>(rebarConfigFiles.size());
        for (VirtualFile rebarConfigFile : rebarConfigFiles) {
          resolveOtpAppsByRebarConfig(rebarConfigFile, importedOtpApps);
        }
        myFoundOtpApps = importedOtpApps;
      }
    });

    Collections.sort(myFoundOtpApps, new Comparator<ImportedOtpApp>() {
      @Override
      public int compare(ImportedOtpApp o1, ImportedOtpApp o2) {
        return String.CASE_INSENSITIVE_ORDER.compare(o1.getName(), o2.getName());
      }
    });

    mySelectedOtpApps = myFoundOtpApps;
    return !myFoundOtpApps.isEmpty();
  }

  @NotNull
  private static List<VirtualFile> findRebarConfigs(@NotNull VirtualFile root, @NotNull final ProgressIndicator indicator) {
    root.refresh(false, true);
    final List<VirtualFile> foundRebarConfigs = new ArrayList<VirtualFile>();
    VfsUtilCore.visitChildrenRecursively(root, new VirtualFileVisitor() {
      @Override
      public boolean visitFile(@NotNull VirtualFile virtualFile) {
        indicator.checkCanceled();
        if (virtualFile.isDirectory()) {
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

  private static void resolveOtpAppsByRebarConfig(@NotNull VirtualFile rebarConfigFile,
                                                  @NotNull List<ImportedOtpApp> importedOtpApps) {
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
          final VirtualFile tentativeAppRoot = root.findFileByRelativePath(relativeSubDir);
          if (tentativeAppRoot != null) {
            tentativeAppRoots.add(tentativeAppRoot);
          }
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
  private static ImportedOtpApp createImportedOtpApp(@NotNull VirtualFile applicationRoot) {
    final VirtualFile appResourceFile = findAppResourceFile(applicationRoot);
    if (appResourceFile == null) {
      return null;
    }
    String applicationName;
    try {
      final String content = new String(appResourceFile.contentsToByteArray());
      final Matcher matcher = APPLICATION_NAME_PATTERN.matcher(content);
      if (!matcher.find()) {
        return null;
      }
      applicationName = matcher.group(1);
    } catch (IOException e) {
      applicationName = applicationRoot.getName().split("\\.")[0];
    }
    return new ImportedOtpApp(applicationName, applicationRoot);
  }

  @Nullable
  private static VirtualFile findAppResourceFile(@NotNull VirtualFile applicationRoot) {
    VirtualFile appResourceFile = null;
    final VirtualFile sourceDir = applicationRoot.findChild("src");
    if (sourceDir != null) {
      final VirtualFile ebinDir = applicationRoot.findChild("ebin");
      appResourceFile = findFileByExtension(sourceDir, "app.src");
      if (appResourceFile == null && ebinDir != null) {
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
}
