package org.intellij.erlang.editor;

import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegateAdapter;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author savenko
 */
public class ErlangEnterHandler extends EnterHandlerDelegateAdapter {

  @Override
  public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffset, @NotNull Ref<Integer> caretAdvance, @NotNull DataContext dataContext, EditorActionHandler originalHandler) {
    if (!(file instanceof ErlangFile)) return Result.Continue;

    if (completeBeginEnd(file, editor)) return Result.Stop;

    return Result.Continue;
  }

  private static boolean completeBeginEnd(final @NotNull PsiFile file, final @NotNull Editor editor) {
    final PsiElement begin = file.findElementAt(editor.getCaretModel().getOffset() - 1);
    final PsiElement beginEndExpr = begin != null ? begin.getParent() : null;

    if (!(begin instanceof LeafPsiElement && ((LeafPsiElement) begin).getElementType().equals(ErlangTypes.ERL_BEGIN))) return false;
    if (!(beginEndExpr instanceof ErlangBeginEndExpression)) return false;
    if (hasEnd(beginEndExpr)) return false;

    final Project project = file.getProject();
    final boolean hasSiblings = hasSiblings((ErlangBeginEndExpression) beginEndExpr);

    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        TextRange range = begin.getTextRange();
        int start = range.getStartOffset();
        int end = range.getEndOffset();
        String s = "begin\n\nend" + (hasSiblings ? "," : "");
        editor.getDocument().replaceString(start, end, s);

        PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument());
        CodeStyleManager.getInstance(project).adjustLineIndent(file, TextRange.from(start, end + s.length()));
        CaretModel caretModel = editor.getCaretModel();
        caretModel.moveCaretRelatively(0, 1, false, false, true);
        CodeStyleManager.getInstance(project).adjustLineIndent(file, caretModel.getOffset());
      }
    });

    return true;
  }

  private static boolean hasEnd(PsiElement element) {
    while (element != null && !(element instanceof ErlangFunctionClause)) {
      if (shouldEndWithEnd(element)) {
        if (getEnd(element) == null) return false;
      }
      element = element.getParent();
    }
    return true;
  }

  private static boolean hasSiblings(@NotNull ErlangBeginEndExpression beginEndExpr) {
    ErlangBeginEndBody beginEndBody = beginEndExpr.getBeginEndBody();

    if (beginEndBody == null) return false;

    ErlangExpression expression = PsiTreeUtil.getChildOfType(beginEndBody, ErlangExpression.class);

    if (expression == null) return false;
    if (expression instanceof ErlangCatchExpression) {
      ErlangTryExpression tryExpression = PsiTreeUtil.getParentOfType(beginEndExpr, ErlangTryExpression.class);
      return tryExpression == null || (tryExpression.getTryCatch() != null);
    }

    return true;
  }

  private static boolean shouldEndWithEnd(@NotNull PsiElement element) {
    return element instanceof ErlangBeginEndExpression ||
           element instanceof ErlangCaseExpression ||
           element instanceof ErlangIfClauses ||
           element instanceof ErlangReceiveExpression ||
           element instanceof ErlangFunExpression && PsiTreeUtil.getChildOfType(element, ErlangFunClauses.class) != null ||
           element instanceof ErlangTryCatch ||
           element instanceof ErlangTryExpression; //try expression does not actually end with end
  }

  @Nullable
  private static PsiElement getEnd(@Nullable PsiElement element) {
    if (element == null) return null;
    if (element instanceof ErlangTryExpression) return getEnd(((ErlangTryExpression) element).getTryCatch());

    PsiElement lastChild = element.getLastChild();

    if (lastChild instanceof LeafPsiElement && ((LeafPsiElement)lastChild).getElementType().equals(ErlangTypes.ERL_END)) return lastChild;

    return null;
  }
}