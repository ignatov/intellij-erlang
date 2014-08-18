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

package org.intellij.erlang.settings;

import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.roots.libraries.LibraryTablesRegistrar;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.TitledSeparator;
import com.intellij.util.ArrayUtil;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.dialyzer.DialyzerSettings;
import org.intellij.erlang.emacs.EmacsSettings;
import org.intellij.erlang.rebar.settings.RebarConfigurationForm;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.utils.ExtProcessUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;
import java.util.List;

public class ErlangExternalToolsConfigurable implements SearchableConfigurable, Configurable.NoScroll {
  public static final String ERLANG_RELATED_TOOLS = "Erlang External Tools";
  public static final String ERLANG_LIBRARY_NAME = "Erlang SDK";
  private final Project myProject;
  private JPanel myPanel;
  private TextFieldWithBrowseButton myEmacsPathSelector;
  private JTextField myEmacsVersionText;
  private RebarConfigurationForm myRebarConfigurationForm;
  private TextFieldWithBrowseButton myPltPathSelector;
  private String myPrevEmacsPath;
  private EmacsSettings myEmacsSettings;
  private RebarSettings myRebarSettings;
  private DialyzerSettings myDialyzerSettings;
  private TextFieldWithBrowseButton mySdkPathSelector;
  private TitledSeparator mySdkTitledSeparator;
  private JLabel mySdkPathLabel;

  public ErlangExternalToolsConfigurable(@NotNull Project project) {
    myProject = project;
    myRebarSettings = RebarSettings.getInstance(project);
    myEmacsSettings = EmacsSettings.getInstance(project);
    myDialyzerSettings = DialyzerSettings.getInstance(project);
    myEmacsPathSelector.addBrowseFolderListener("Select Emacs executable", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    myPltPathSelector.addBrowseFolderListener("Select dialyzer PLT", "", null, FileChooserDescriptorFactory.createSingleLocalFileDescriptor());
    mySdkPathSelector.addBrowseFolderListener("Select Erlang SDK path", "", null, FileChooserDescriptorFactory.getDirectoryChooserDescriptor("Erlang SDK root"));
    myPrevEmacsPath = myEmacsSettings.getEmacsPath();

    if (StringUtil.isEmpty(myRebarSettings.getRebarPath())) {
      VirtualFile baseDir = project.getBaseDir();
      if (baseDir != null) {
        VirtualFile rebar = baseDir.findChild("rebar");
        if (rebar != null) {
          String canonicalPath = rebar.getCanonicalPath();
          if (canonicalPath != null) {
            myRebarSettings.setRebarPath(canonicalPath);
          }
        }
      }
    }

    if (StringUtil.isEmpty(myEmacsSettings.getEmacsPath()) && (SystemInfo.isLinux || SystemInfo.isMac)) {
      String suggestedPath = "/usr/bin/emacs";
      File file = new File(suggestedPath);
      if (file.exists() && FileUtil.canExecute(file)) {
        myEmacsSettings.setEmacsPath(suggestedPath);
      }
    }
    
    if (!ErlangSystemUtil.isSmallIde()) {
      mySdkPathSelector.setVisible(false);
      mySdkTitledSeparator.setVisible(false);
      mySdkPathLabel.setVisible(false);
    }

    reset();
  }

  @NotNull
  @Override
  public String getId() {
    return ERLANG_RELATED_TOOLS;
  }

  @Nullable
  @Override
  public Runnable enableSearch(String option) {
    return null;
  }

  @NonNls
  @Override
  public String getDisplayName() {
    return ERLANG_RELATED_TOOLS;
  }

  @Nullable
  @Override
  public String getHelpTopic() {
    return null;
  }

  @Nullable
  @Override
  public JComponent createComponent() {
    myRebarConfigurationForm.createComponent();
    return myPanel;
  }

  @Override
  public boolean isModified() {
    String emacsSelectedPath = myEmacsPathSelector.getText();
    if (!myPrevEmacsPath.equals(emacsSelectedPath)) validateEmacsPath();

    return
         !myRebarSettings.getRebarPath().equals(myRebarConfigurationForm.getPath())
      || !myEmacsSettings.getEmacsPath().equals(emacsSelectedPath)
      || !myDialyzerSettings.getCurrentPltPath().equals(myPltPathSelector.getText())
      || !getErlangSdkPath().equals(mySdkPathSelector.getText());
  }

  @Override
  public void apply() throws ConfigurationException {
    myRebarSettings.setRebarPath(myRebarConfigurationForm.getPath());
    myEmacsSettings.setEmacsPath(myEmacsPathSelector.getText());
    myDialyzerSettings.setCurrentPltPath(myPltPathSelector.getText());
    setUpOrUpdateSdk(mySdkPathSelector.getText());
  }

  @Override
  public void reset() {
    myRebarConfigurationForm.setPath(myRebarSettings.getRebarPath());
    myEmacsPathSelector.setText(myEmacsSettings.getEmacsPath());
    myPltPathSelector.setText(myDialyzerSettings.getCurrentPltPath());
    mySdkPathSelector.setText(getErlangSdkPath());
    validateEmacsPath();
  }

  @Override
  public void disposeUIResources() {
  }

  private void validateEmacsPath() {
    String version = ExtProcessUtil.restrictedTimeExec(myEmacsPathSelector.getText() + " --version", 3000);
    List<String> split = StringUtil.split(version, "\n");
    if (StringUtil.containsIgnoreCase(version, "emacs") && split.size() > 0) {
      myEmacsVersionText.setText(ContainerUtil.getFirstItem(split));
    }
    else {
      myEmacsVersionText.setText("N/A");
    }
  }

  @NotNull
  public String getErlangSdkPath() {
    return getErlangSdkPath(myProject);
  }

  @NotNull
  public static String getErlangSdkPath(@NotNull Project project) {
    AccessToken token = ApplicationManager.getApplication().acquireReadActionLock();
    try {
      LibraryTable table = LibraryTablesRegistrar.getInstance().getLibraryTable(project);
      Library lib = table.getLibraryByName(ERLANG_LIBRARY_NAME);
      String[] urls = lib == null ? ArrayUtil.EMPTY_STRING_ARRAY : lib.getUrls(OrderRootType.CLASSES);
      return VfsUtilCore.urlToPath(ObjectUtils.notNull(ArrayUtil.getFirstElement(urls), ""));
    }
    finally {
      token.finish();
    }
  }
  
  private void setUpOrUpdateSdk(@NotNull final String path) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        LibraryTable table = LibraryTablesRegistrar.getInstance().getLibraryTable(myProject);
        Library get = table.getLibraryByName(ERLANG_LIBRARY_NAME);
        Library lib = get != null ? get : table.createLibrary(ERLANG_LIBRARY_NAME);

        Library.ModifiableModel libraryModel = lib.getModifiableModel();
        String libUrl = ArrayUtil.getFirstElement(lib.getUrls(OrderRootType.CLASSES));
        if (libUrl != null) {
          libraryModel.removeRoot(libUrl, OrderRootType.CLASSES);
        }

        String url = VfsUtilCore.pathToUrl(path);
        libraryModel.addRoot(url, OrderRootType.CLASSES);
        libraryModel.commit();
        
        boolean remove = path.isEmpty();
        if (remove) {
          updateModules(lib, true);
          table.removeLibrary(lib);
        }

        table.getModifiableModel().commit();

        if (!remove) {
          updateModules(lib, false);
        }
      }
    });
  }

  private void updateModules(@NotNull Library lib, boolean remove) {
    Module[] modules = ModuleManager.getInstance(myProject).getModules();
    for (Module module : modules) {
      ModifiableRootModel model = ModuleRootManager.getInstance(module).getModifiableModel();
      if (!remove) {
        if (model.findLibraryOrderEntry(lib) == null) {
          LibraryOrderEntry entry = model.addLibraryEntry(lib);
          entry.setScope(DependencyScope.PROVIDED);
        }
      }
      else {
        LibraryOrderEntry entry = model.findLibraryOrderEntry(lib);
        if (entry != null) {
          model.removeOrderEntry(entry);
        }
      }
      model.commit();
    }
  }
}
