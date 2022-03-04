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
import java.util.ArrayList;
import java.util.StringTokenizer;

public class ErlangFacetEditor extends FacetEditorTab {
  private JPanel myRootPanel;
  private JTextField myParseTransformsEditorField;
  private JTextField myFlagsEditorField;


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
    myFlagsEditorField.getDocument().addDocumentListener(new DocumentAdapter() {
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
    myFlagsEditorField.setText(getConfigurationExtraFlags());
    myIsModified = false;
  }

  @Override
  public void disposeUIResources() {
  }

  @Override
  public void apply() {
    myConfiguration.setParseTransformsFrom(getUiParseTransforms());
    myConfiguration.setExtraFlagsFrom(getUiExtraFlags());
    myIsModified = false;
  }

  private String getConfigurationParseTransforms() {
    return StringUtil.join(myConfiguration.getParseTransforms(), ", ");
  }

  private String getConfigurationExtraFlags() {
    return StringUtil.join(myConfiguration.getExtraFlags(), " ");
  }

  private List<String> getUiExtraFlags() {
    String extraFlagsString = myFlagsEditorField.getText();
    StringTokenizer st=new StringTokenizer(extraFlagsString," '");
    boolean quoted=false;
    List<String> result=new ArrayList<String>();
    StringBuilder sb=new StringBuilder();
    while(st.hasMoreTokens())
    {
      String token=st.nextToken();
      if(token.equals("'")) {
         quoted = !quoted;
      }
      if(!quoted && token.equals(" ")) {
        if(sb.length()>0)
          result.add(sb.toString());
        sb = new StringBuilder();
      } else {
        sb.append(token);
      }
    }
    if(sb.length()>0)
      result.add(sb.toString());
    return result;
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