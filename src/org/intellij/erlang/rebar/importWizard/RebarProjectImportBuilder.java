/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.compiler.CompilerWorkspaceConfiguration;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ex.ApplicationInfoEx;
import com.intellij.openapi.diagnostic.Logger;
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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileVisitor;
import com.intellij.openapi.vfs.newvfs.impl.VirtualDirectoryImpl;
import com.intellij.packaging.artifacts.ModifiableArtifactModel;
import com.intellij.projectImport.ProjectImportBuilder;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;

public class RebarProjectImportBuilder extends ProjectImportBuilder<ImportedOtpApp> {
  private static final Logger LOG = Logger.getInstance(RebarProjectImportBuilder.class);

  private boolean myOpenProjectSettingsAfter = false;
  @Nullable private VirtualFile myProjectRoot = null;
  @NotNull private List<ImportedOtpApp> myFoundOtpApps = Collections.emptyList();
  @NotNull private List<ImportedOtpApp> mySelectedOtpApps = Collections.emptyList();
  private boolean myImportExamples;
  @NotNull private String myRebarPath = "";
  private boolean myIsImportingProject;

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
    return new ArrayList<>(myFoundOtpApps);
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

  public boolean setProjectRoot(@NotNull final VirtualFile projectRoot) {
    if (projectRoot.equals(myProjectRoot)) {
      return true;
    }
    
    boolean unitTestMode = ApplicationManager.getApplication().isUnitTestMode();
    
    myProjectRoot = projectRoot;
    if (!unitTestMode && projectRoot instanceof VirtualDirectoryImpl) {
      ((VirtualDirectoryImpl) projectRoot).refreshAndFindChild("deps");
      ((VirtualDirectoryImpl) projectRoot).refreshAndFindChild("_build");
    }

    ProgressManager.getInstance().run(new Task.Modal(getCurrentProject(), "Scanning Rebar projects", true) {
      public void run(@NotNull final ProgressIndicator indicator) {

        List<VirtualFile> rebarConfigFiles = findRebarConfigs(myProjectRoot, indicator);
        final LinkedHashSet<ImportedOtpApp> importedOtpApps = new LinkedHashSet<>(rebarConfigFiles.size());

        VfsUtilCore.visitChildrenRecursively(projectRoot, new VirtualFileVisitor() {
          @Override
          public boolean visitFile(@NotNull VirtualFile file) {
            indicator.checkCanceled();

            if (file.isDirectory()) {
              indicator.setText2(file.getPath());
              if (isExamplesDirectory(file) || isRelDirectory(projectRoot.getPath(), file.getPath())) return false;
            }

            ContainerUtil.addAllNotNull(importedOtpApps, createImportedOtpApp(file));
            return true;
          }
        });

        myFoundOtpApps = ContainerUtil.newArrayList(importedOtpApps);
      }
    });

    Collections.sort(myFoundOtpApps, (o1, o2) -> {
      int nameCompareResult = String.CASE_INSENSITIVE_ORDER.compare(o1.getName(), o2.getName());
      if (nameCompareResult == 0) {
        return String.CASE_INSENSITIVE_ORDER.compare(o1.getRoot().getPath(), o2.getRoot().getPath());
      }
      return nameCompareResult;
    });

    mySelectedOtpApps = myFoundOtpApps;
    return !myFoundOtpApps.isEmpty();
  }

  private static boolean isRelDirectory(String projectRootPath, String path) {
    return (projectRootPath + "/rel").equals(path);
  }

  @SuppressWarnings("DialogTitleCapitalization")
  @Override
  public boolean validate(Project current, Project dest) {
    if (!findIdeaModuleFiles(mySelectedOtpApps)) {
      return true;
    }
    int resultCode = Messages.showYesNoCancelDialog(
      ApplicationInfoEx.getInstanceEx().getFullApplicationName() + " module files found:\n\n" +
      StringUtil.join(mySelectedOtpApps, importedOtpApp -> {
        VirtualFile ideaModuleFile = importedOtpApp.getIdeaModuleFile();
        return ideaModuleFile != null ? "    " + ideaModuleFile.getPath() + "\n" : "";
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
    Set<String> selectedAppNames = ContainerUtil.newHashSet();
    for (ImportedOtpApp importedOtpApp : mySelectedOtpApps) {
      selectedAppNames.add(importedOtpApp.getName());
    }
    Sdk projectSdk = fixProjectSdk(project);
    List<Module> createdModules = new ArrayList<>();
    final List<ModifiableRootModel> createdRootModels = new ArrayList<>();
    final ModifiableModuleModel obtainedModuleModel =
      moduleModel != null ? moduleModel : ModuleManager.getInstance(project).getModifiableModel();
    for (ImportedOtpApp importedOtpApp : mySelectedOtpApps) {
      VirtualFile ideaModuleDir = importedOtpApp.getRoot();
      String ideaModuleFile = ideaModuleDir.getCanonicalPath() + File.separator + importedOtpApp.getName() + ".iml";
      Module module = obtainedModuleModel.newModule(ideaModuleFile, ErlangModuleType.getInstance().getId());
      createdModules.add(module);
      importedOtpApp.setModule(module);
      if (importedOtpApp.getIdeaModuleFile() == null) {
        ModifiableRootModel rootModel = ModuleRootManager.getInstance(module).getModifiableModel();
        // Make it inherit SDK from the project.
        rootModel.inheritSdk();
        // Initialize source and test paths.
        ContentEntry content = rootModel.addContentEntry(importedOtpApp.getRoot());
        addSourceDirToContent(content, ideaModuleDir, "src", false);
        addSourceDirToContent(content, ideaModuleDir, "test", true);
        addIncludeDirectories(content, importedOtpApp);
        // Exclude standard folders
        excludeDirFromContent(content, ideaModuleDir, "doc");
        // Initialize output paths according to Rebar conventions.
        CompilerModuleExtension compilerModuleExt = rootModel.getModuleExtension(CompilerModuleExtension.class);
        compilerModuleExt.inheritCompilerOutputPath(false);
        compilerModuleExt.setCompilerOutputPath(ideaModuleDir + File.separator + "ebin");
        compilerModuleExt.setCompilerOutputPathForTests(ideaModuleDir + File.separator + ".eunit");
        createdRootModels.add(rootModel);
        // Set inter-module dependencies
        resolveModuleDeps(rootModel, importedOtpApp, projectSdk, selectedAppNames);
      }
    }
    // Commit project structure.
    LOG.info("Commit project structure");
    ApplicationManager.getApplication().runWriteAction(() -> {
      for (ModifiableRootModel rootModel : createdRootModels) {
        rootModel.commit();
      }
      obtainedModuleModel.commit();
    });

    addErlangFacets(mySelectedOtpApps);
    RebarSettings.getInstance(project).setRebarPath(myRebarPath);
    if (myIsImportingProject) {
      ErlangCompilerSettings.getInstance(project).setUseRebarCompilerEnabled(true);
    }
    CompilerWorkspaceConfiguration.getInstance(project).CLEAR_OUTPUT_DIRECTORY = false;

    return createdModules;
  }

  private static void addErlangFacets(final List<ImportedOtpApp> apps) {
    ApplicationManager.getApplication().runWriteAction(() -> {
      for (ImportedOtpApp app : apps) {
        Module module = app.getModule();
        if (module == null) continue;
        ErlangFacet facet = ErlangFacet.getFacet(module);
        if (facet == null) {
          ErlangFacet.createFacet(module);
          facet = ErlangFacet.getFacet(module);
        }
        if (facet != null) {
          ErlangFacetConfiguration configuration = facet.getConfiguration();
          configuration.addParseTransforms(app.getParseTransforms());
        }
      }
    });
  }

  @Nullable
  private static Sdk fixProjectSdk(@NotNull Project project) {
    final ProjectRootManagerEx projectRootMgr = ProjectRootManagerEx.getInstanceEx(project);
    Sdk selectedSdk = projectRootMgr.getProjectSdk();
    if (selectedSdk == null || selectedSdk.getSdkType() != ErlangSdkType.getInstance()) {
      final Sdk moreSuitableSdk = ProjectJdkTable.getInstance().findMostRecentSdkOfType(ErlangSdkType.getInstance());
      ApplicationManager.getApplication().runWriteAction(() -> projectRootMgr.setProjectSdk(moreSuitableSdk));
      return moreSuitableSdk;
    }
    return selectedSdk;
  }

  private static void addSourceDirToContent(@NotNull ContentEntry content,
                                            @NotNull VirtualFile root,
                                            @NotNull String sourceDir,
                                            boolean test) {
    VirtualFile sourceDirFile = root.findChild(sourceDir);
    if (sourceDirFile != null) {
      content.addSourceFolder(sourceDirFile, test);
    }
  }

  private static void addIncludeDirectories(@NotNull ContentEntry content, ImportedOtpApp app) {
    for (VirtualFile includeDirectory : app.getIncludePaths()) {
      ErlangIncludeDirectoryUtil.markAsIncludeDirectory(content, includeDirectory);
    }
  }

  private static void excludeDirFromContent(ContentEntry content, VirtualFile root, String excludeDir) {
    VirtualFile excludeDirFile = root.findChild(excludeDir);
    if (excludeDirFile != null) {
      content.addExcludeFolder(excludeDirFile);
    }
  }

  @NotNull
  private List<VirtualFile> findRebarConfigs(@NotNull final VirtualFile root, @NotNull final ProgressIndicator indicator) {
    root.refresh(false, true);
    final List<VirtualFile> foundRebarConfigs = new ArrayList<>();
    VfsUtilCore.visitChildrenRecursively(root, new VirtualFileVisitor() {
      @Override
      public boolean visitFile(@NotNull VirtualFile file) {
        indicator.checkCanceled();
        if (file.isDirectory()) {
          if (isExamplesDirectory(file) || isRelDirectory(root.getPath(), file.getPath())) return false;
          indicator.setText2(file.getPath());
        }
        else if (file.getName().equalsIgnoreCase("rebar.config")) {
          foundRebarConfigs.add(file);
        }
        return true;
      }
    });

    return foundRebarConfigs;
  }

  private boolean isExamplesDirectory(VirtualFile virtualFile) {
    return "examples".equals(virtualFile.getName()) && !myImportExamples;
  }

  @Nullable
  private static ImportedOtpApp createImportedOtpApp(@NotNull VirtualFile appRoot) {
    VirtualFile appResourceFile = findAppResourceFile(appRoot);
    if (appResourceFile == null) {
      return null;
    }
    return new ImportedOtpApp(appRoot, appResourceFile);
  }

  @Nullable
  private static VirtualFile findAppResourceFile(@NotNull VirtualFile applicationRoot) {
    VirtualFile appResourceFile = null;
    VirtualFile sourceDir = applicationRoot.findChild("src");
    if (sourceDir != null) {
      appResourceFile = findFileByExtension(sourceDir, "app.src");
    }
    if (appResourceFile == null) {
      VirtualFile ebinDir = applicationRoot.findChild("ebin");
      if (ebinDir != null) {
        appResourceFile = findFileByExtension(ebinDir, "app");
      }
    }
    return appResourceFile;
  }

  @Nullable
  private static VirtualFile findFileByExtension(@NotNull VirtualFile dir, @NotNull String extension) {
    for (VirtualFile file : dir.getChildren()) {
      String fileName = file.getName();
      if (!file.isDirectory() && fileName.endsWith(extension)) {
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
          VirtualFile ideaModuleFile = importedOtpApp.getIdeaModuleFile();
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
      VirtualFile applicationRoot = importedOtpApp.getRoot();
      String ideaModuleName = importedOtpApp.getName();
      VirtualFile imlFile = applicationRoot.findChild(ideaModuleName + ".iml");
      if (imlFile != null) {
        ideaModuleFileExists = true;
        importedOtpApp.setIdeaModuleFile(imlFile);
      }
      else {
        VirtualFile emlFile = applicationRoot.findChild(ideaModuleName + ".eml");
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
    HashSet<String> unresolvedAppNames = ContainerUtil.newHashSet();
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
    Pattern appDirNamePattern = Pattern.compile(otpAppName + "-.*");
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

  public void setRebarPath(@NotNull String rebarPath) {
    myRebarPath = rebarPath;
  }

  public void setIsImportingProject(boolean isImportingProject) {
    myIsImportingProject = isImportingProject;
  }
}