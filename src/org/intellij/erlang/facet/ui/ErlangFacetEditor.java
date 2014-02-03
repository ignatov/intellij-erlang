package org.intellij.erlang.facet.ui;

import com.intellij.facet.ui.FacetEditorContext;
import com.intellij.facet.ui.FacetEditorTab;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.util.text.CharFilter;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.DocumentAdapter;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.util.List;

public class ErlangFacetEditor extends FacetEditorTab {
  private JPanel myRootPanel;
  private JTextField myParseTransformsEditorField;

  private final ErlangFacetConfiguration myConfiguration;
  private boolean myIsModified = false;

  public ErlangFacetEditor(@SuppressWarnings("UnusedParameters") FacetEditorContext editorContext, ErlangFacetConfiguration configuration) {
    myConfiguration = configuration;
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
    return myIsModified;
  }

  @Override
  public void reset() {
    myParseTransformsEditorField.setText(getConfigurationParseTransforms());
    myIsModified = false;
  }

  @Override
  public void disposeUIResources() {
  }

  @Override
  public void apply() throws ConfigurationException {
    myConfiguration.setParseTransformsFrom(getUiParseTransforms());
    myIsModified = false;
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