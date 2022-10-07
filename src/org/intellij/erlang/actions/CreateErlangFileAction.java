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

package org.intellij.erlang.actions;

import com.intellij.ide.actions.CreateFileFromTemplateAction;
import com.intellij.ide.actions.CreateFileFromTemplateDialog;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.InputValidatorEx;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiDirectory;
import org.intellij.erlang.icons.ErlangIcons;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class CreateErlangFileAction extends CreateFileFromTemplateAction implements DumbAware {
  private static final String NEW_ERLANG_FILE = "New Erlang File";
  private static final String DIALOG_TITLE = "Create a new Erlang file from template";

  public CreateErlangFileAction() {
    super(NEW_ERLANG_FILE, "", ErlangIcons.FILE);
  }

  @Override
  protected void buildDialog(final @NotNull Project project, @NotNull PsiDirectory directory, CreateFileFromTemplateDialog.Builder builder) {
    builder.
      setTitle(DIALOG_TITLE).
      addKind("Empty module", ErlangIcons.FILE, "Erlang Module").
      addKind("Header file", ErlangIcons.HEADER, "Erlang Header").
      addKind("EUnit tests", ErlangIcons.EUNIT, "Erlang EUnit Tests").
      //-----------
      addKind("OTP application",
              ErlangIcons.OTP_APPLICATION,
              "Erlang Application").
      addKind("OTP application, minimal",
              ErlangIcons.OTP_APPLICATION,
              "Erlang Application Minimal").
      addKind("OTP application resource file",
              ErlangIcons.OTP_APP_RESOURCE,
              "Erlang Application Resource File").
      //-----------
      addKind("OTP supervisor, Legacy OTP ≤17",
              ErlangIcons.OTP_SUPERVISOR,
              "Erlang Supervisor Legacy").
      addKind("OTP supervisor, OTP ≥18",
              ErlangIcons.OTP_SUPERVISOR,
              "Erlang Supervisor").
      addKind("OTP supervisor, OTP ≥18, minimal",
              ErlangIcons.OTP_SUPERVISOR,
              "Erlang Supervisor Minimal").
      //-----------
      addKind("OTP gen_server",
              ErlangIcons.OTP_GEN_SERVER,
              "Erlang Gen Server").
      addKind("OTP gen_server, minimal",
              ErlangIcons.OTP_GEN_SERVER,
              "Erlang Gen Server Minimal").
      //-----------
      addKind("OTP gen_statem, OTP ≥19",
              ErlangIcons.OTP_GEN_STATEM,
              "Erlang Gen Statem").
      addKind("OTP gen_fsm, deprecated in OTP 19",
              ErlangIcons.OTP_GEN_FSM,
              "Erlang Gen FSM").
      addKind("OTP gen_fsm, minimal, deprecated in OTP 19",
              ErlangIcons.OTP_GEN_FSM,
              "Erlang Gen FSM Minimal").
      addKind("OTP gen_event",
              ErlangIcons.OTP_GEN_EVENT,
              "Erlang Gen Event").

      setValidator(new InputValidatorEx() {
        @Override
        public boolean checkInput(String inputString) {
          return getErrorText(inputString) == null;
        }

        @Override
        public boolean canClose(String inputString) {
          return getErrorText(inputString) == null;
        }

        @Nullable
        @Override
        public String getErrorText(String inputString) {
          return !StringUtil.isEmpty(inputString) && FileUtil.sanitizeFileName(inputString, false).equals(inputString) ? null :
            "'" + inputString + "'" + " is not a valid Erlang module name";
        }
      })
    ;
  }

  @Override
  protected String getActionName(PsiDirectory directory, @NotNull String newName, String templateName) {
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
