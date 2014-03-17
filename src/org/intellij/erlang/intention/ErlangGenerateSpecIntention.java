package org.intellij.erlang.intention;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangGenerateSpecFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangGenerateSpecIntention extends ErlangBaseNamedElementIntention {
  public ErlangGenerateSpecIntention() {
    super(ErlangGenerateSpecFix.NAME, ErlangGenerateSpecFix.NAME);
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    if (!(file instanceof ErlangFile)) return false;
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function == null) return false;

    return ErlangPsiImplUtil.getSpecification(function) == null;
  }


  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    if (!(file instanceof ErlangFile)) {
      throw new IncorrectOperationException("Only applicable to Erlang files.");
    }
    ErlangFunction function = findFunction(file, editor.getCaretModel().getOffset());
    if (function == null) {
      throw new IncorrectOperationException("Cursor should be placed on Erlang function.");
    }
    if (ErlangPsiImplUtil.getSpecification(function) != null) {
      throw new IncorrectOperationException("Specification for this function already exists.");
    }
    ErlangGenerateSpecFix.generateSpec(editor, function);
  }

  @Nullable
  private static ErlangFunction findFunction(PsiFile file, int offset) {
    return findElement(file, offset, ErlangFunction.class);
  }
}
