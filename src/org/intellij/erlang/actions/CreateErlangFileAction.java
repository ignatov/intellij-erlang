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

package org.intellij.erlang.actions;

import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.InputValidatorEx;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDirectory;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.impl.ErlangElementFactory;

/**
 * @author ignatov
 */
public class CreateErlangFileAction extends CreateFileFromTemplateAction {
  public static final String NEW_ERLANG_FILE = "New Erlang File";

  public CreateErlangFileAction() {
    super(NEW_ERLANG_FILE, "", ErlangIcons.FILE);
  }

  @Override
  protected void buildDialog(final Project project, PsiDirectory directory, CreateFileFromTemplateDialog.Builder builder) {
    builder.
      setTitle(NEW_ERLANG_FILE).
      addKind("Empty module", ErlangIcons.FILE, "Erlang File").
      addKind("OTP application", ErlangIcons.FILE, "Erlang Application").
      addKind("OTP gen_server", ErlangIcons.FILE, "Erlang Gen Server").
      setValidator(new InputValidatorEx() {
        @Override
        public boolean checkInput(String inputString) {
          return true;
        }

        @Override
        public boolean canClose(String inputString) {
          return !StringUtil.isEmptyOrSpaces(inputString) && getErrorText(inputString) == null;
        }

        @Override
        public String getErrorText(String inputString) {
          String error = " is not a valid Erlang module name";
          if (StringUtil.isEmpty(inputString)) return null;
          try {
            ErlangElementFactory.createQAtomFromText(project, inputString);
            if (inputString != null && inputString.equals(FileUtil.sanitizeFileName(inputString))) {
              return null;
            }
            return "'" + inputString + "'" + error;
          } catch (Exception ignored) {
          }
          return "'" + inputString + "'" + error;
        }
      })
    ;
  }

  @Override
  protected String getActionName(PsiDirectory directory, String newName, String templateName) {
    return NEW_ERLANG_FILE;
  }

  @Override
  public int hashCode() {
    return 0;
  }

  @Override
  public boolean equals(Object obj) {
    return obj instanceof CreateErlangFileAction;
  }
}