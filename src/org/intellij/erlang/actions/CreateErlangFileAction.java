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
  public static final String ERLANG_FILE = "Erlang file";

  public CreateErlangFileAction() {
    super(NEW_ERLANG_FILE, "", ErlangIcons.FILE);
  }

  @Override
  protected void buildDialog(final Project project, PsiDirectory directory, CreateFileFromTemplateDialog.Builder builder) {
    builder.
      setTitle(NEW_ERLANG_FILE).
      addKind(ERLANG_FILE, ErlangIcons.FILE, "Erlang File").
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