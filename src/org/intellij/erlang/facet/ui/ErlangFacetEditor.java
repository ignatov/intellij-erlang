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

package org.intellij.erlang.facet.ui;

import com.intellij.facet.ui.FacetEditorContext;
import com.intellij.facet.ui.FacetEditorTab;
import com.intellij.openapi.util.text.CharFilter;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.DocumentAdapter;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

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

  @NotNull
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
  public void apply() {
    myConfiguration.setParseTransformsFrom(getUiParseTransforms());
    myIsModified = false;
  }

  private String getConfigurationParseTransforms() {
    return StringUtil.join(myConfiguration.getParseTransforms(), ", ");
  }

  private List<String> getUiParseTransforms() {
    String parseTransformsString = myParseTransformsEditorField.getText();
    List<String> split = StringUtil.split(parseTransformsString, ",");
    return ContainerUtil.mapNotNull(split, s -> {
      String strippedModuleName = StringUtil.strip(s, CharFilter.NOT_WHITESPACE_FILTER);
      return StringUtil.isEmpty(strippedModuleName) ? null : strippedModuleName;
    });
  }
}