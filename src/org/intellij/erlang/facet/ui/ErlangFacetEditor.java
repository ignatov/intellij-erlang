package org.intellij.erlang.facet.ui;

import com.intellij.facet.Facet;
import com.intellij.facet.ui.FacetEditorContext;
import com.intellij.facet.ui.FacetEditorTab;
import com.intellij.facet.ui.FacetValidatorsManager;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.ui.PathEditor;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangFacetEditor extends FacetEditorTab {
  private JPanel myRootPanel;
  private JPanel myIncludePathsEditorPanel;

  private final ErlangFacetConfiguration myConfiguration;
  private final PathEditor myIncludePathsEditor;

  public ErlangFacetEditor(final FacetEditorContext editorContext, final FacetValidatorsManager validatorsManager, ErlangFacetConfiguration configuration) {
    if (editorContext.isNewFacet()) {
      configuration.addIncludeDirectories(editorContext.getModule());
    }
    myConfiguration = configuration;

    FileChooserDescriptor chooserDescriptor = new FileChooserDescriptor(false, true, false, false, false, true);
    myIncludePathsEditor = new PathEditor(chooserDescriptor);
    myIncludePathsEditor.setAddBaseDir(ModuleRootManager.getInstance(editorContext.getModule()).getContentRoots()[0]);

    JComponent editorComponent = myIncludePathsEditor.createComponent();
    myIncludePathsEditorPanel.add(editorComponent, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_WANT_GROW, editorComponent.getMinimumSize(), editorComponent.getPreferredSize(), editorComponent.getMaximumSize()));
  }

  @Nls
  @Override
  public String getDisplayName() {
    return "Erlang";
  }

  @Nullable
  @Override
  public JComponent createComponent() {
    reset();
    return myRootPanel;
  }

  @Override
  public boolean isModified() {
    return myIncludePathsEditor.isModified();
  }

  @Override
  public void reset() {
    myIncludePathsEditor.resetPath(getConfigurationIncludeDirectories());
  }

  @Override
  public void disposeUIResources() {
  }

  @Override
  public void apply() throws ConfigurationException {
    myIncludePathsEditor.resetPath(Arrays.asList(myIncludePathsEditor.getRoots()));
    myConfiguration.setIncludePaths(getUiIncludeDirectories());
  }

  @Override
  public void onFacetInitialized(@NotNull Facet facet) {
  }

  private List<VirtualFile> getConfigurationIncludeDirectories() {
    List<String> includePaths = myConfiguration.getIncludePaths();
    List<VirtualFile> includeDirectories = new ArrayList<VirtualFile>(includePaths.size());
    for (String includePath : includePaths) {
      VirtualFile includeDir = LocalFileSystem.getInstance().findFileByPath(includePath);
      ContainerUtil.addIfNotNull(includeDirectories, includeDir);
    }
    return includeDirectories;
  }

  private List<String> getUiIncludeDirectories() {
    VirtualFile[] includeDirs = myIncludePathsEditor.getRoots();
    List<String> includePaths = new ArrayList<String>(includeDirs.length);
    for (VirtualFile includeDir : includeDirs) {
      includePaths.add(includeDir.getPath());
    }
    return includePaths;
  }
}