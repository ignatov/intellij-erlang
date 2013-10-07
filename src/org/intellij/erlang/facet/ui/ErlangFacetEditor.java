package org.intellij.erlang.facet.ui;

import com.intellij.facet.ui.FacetEditorContext;
import com.intellij.facet.ui.FacetEditorTab;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.ui.PathEditor;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.text.CharFilter;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.DocumentAdapter;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangFacetEditor extends FacetEditorTab {
  private JPanel myRootPanel;
  private JPanel myIncludePathsEditorPanel;
  private JTextField myParseTransformsEditorField;

  private final ErlangFacetConfiguration myConfiguration;
  private final PathEditor myIncludePathsEditor;
  private boolean myIsModified = false;

  public ErlangFacetEditor(FacetEditorContext editorContext, ErlangFacetConfiguration configuration) {
    if (editorContext.isNewFacet()) {
      configuration.addIncludeDirectoriesToIncludePath(editorContext.getModule());
    }
    myConfiguration = configuration;

    FileChooserDescriptor chooserDescriptor = new FileChooserDescriptor(false, true, false, false, false, true);
    myIncludePathsEditor = new PathEditor(chooserDescriptor);
    myIncludePathsEditor.setAddBaseDir(ModuleRootManager.getInstance(editorContext.getModule()).getContentRoots()[0]);

    JComponent includePathsEditorComponent = myIncludePathsEditor.createComponent();
    myIncludePathsEditorPanel.add(includePathsEditorComponent, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_WANT_GROW, includePathsEditorComponent.getMinimumSize(), includePathsEditorComponent.getPreferredSize(), includePathsEditorComponent.getMaximumSize()));

    myParseTransformsEditorField.getDocument().addDocumentListener(new DocumentAdapter() {
      @Override
      protected void textChanged(DocumentEvent e) {
        myIsModified = true;
      }
    });
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
    return myIncludePathsEditor.isModified() || myIsModified;
  }

  @Override
  public void reset() {
    myIncludePathsEditor.resetPath(getConfigurationIncludeDirectories());
    myParseTransformsEditorField.setText(getConfigurationParseTransforms());
    myIsModified = false;
  }

  @Override
  public void disposeUIResources() {
  }

  @Override
  public void apply() throws ConfigurationException {
    myIncludePathsEditor.resetPath(Arrays.asList(myIncludePathsEditor.getRoots()));
    myConfiguration.setIncludePathsFrom(getUiIncludeDirectories());
    myConfiguration.setParseTransformsFrom(getUiParseTransforms());
    myIsModified = false;
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

  private String getConfigurationParseTransforms() {
    return StringUtil.join(myConfiguration.getParseTransforms(), ", ");
  }

  private List<String> getUiParseTransforms() {
    String parseTransformsString = myParseTransformsEditorField.getText();
    List<String> split = StringUtil.split(parseTransformsString, ",");
    return ContainerUtil.mapNotNull(split, new Function<String, String>() {
      @Nullable
      @Override
      public String fun(String s) {
        String strippedModuleName = StringUtil.strip(s, CharFilter.NOT_WHITESPACE_FILTER);
        return StringUtil.isEmpty(strippedModuleName) ? null : strippedModuleName;
      }
    });
  }
}