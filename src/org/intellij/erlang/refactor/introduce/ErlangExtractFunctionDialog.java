/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.refactor.introduce;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.ValidationInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.DocumentAdapter;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import java.util.List;

public class ErlangExtractFunctionDialog extends DialogWrapper {
  private final List<ErlangNamedElement> myInParams;
  private JLabel mySignatureLabel;
  private JPanel myMainPanel;
  private JTextField myFunctionNameField;

  protected ErlangExtractFunctionDialog(@NotNull Project project, @NotNull String functionName, @NotNull List<ErlangNamedElement> inParams) {
    super(project);
    myInParams = inParams;
    myFunctionNameField.getDocument().addDocumentListener(new DocumentAdapter() {
      @Override
      protected void textChanged(DocumentEvent e) {
        mySignatureLabel.setText(createSignature());
      }
    });
    setTitle("Extract Function");
    init();
    myFunctionNameField.setText(functionName);
    mySignatureLabel.setText(createSignature());
  }

  private String createSignature() {
    return ErlangExtractFunctionHandler.generateSignature(myFunctionNameField.getText(), myInParams);
  }

  @Nullable
  @Override
  protected JComponent createCenterPanel() {
    return myMainPanel;
  }

  @Nullable
  @Override
  public JComponent getPreferredFocusedComponent() {
    return myFunctionNameField;
  }

  public String getFunctionName() {
    return myFunctionNameField.getText();
  }

  @Override
  protected boolean postponeValidation() {
    return false;
  }

  @Nullable
  @Override
  protected ValidationInfo doValidate() {
    if (!StringUtil.isJavaIdentifier(getFunctionName())) {
      return new ValidationInfo("Not a valid name!", myFunctionNameField);
    }
    return super.doValidate();
  }
}
