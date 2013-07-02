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
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author savenko
 */
public class ErlangEnterHandler extends EnterHandlerDelegateAdapter {

  @Override
  public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffset, @NotNull Ref<Integer> caretAdvance, @NotNull DataContext dataContext, EditorActionHandler originalHandler) {
    if (!(file instanceof ErlangFile)) return Result.Continue;

    if (completeBeginEnd(file, editor) ||
        completeCaseOf(file, editor) ||
        completeReceive(file, editor) ||
        completeIf(file, editor)) {
      return Result.Stop;
    }

    return Result.Continue;
  }

  private static boolean completeBeginEnd(final @NotNull PsiFile file, final @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_BEGIN, ErlangBeginEndExpression.class);
  }

  private static boolean completeCaseOf(final @NotNull PsiFile file, final @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_OF, ErlangCaseExpression.class);
  }

  private static boolean completeReceive(final @NotNull PsiFile file, final @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_RECEIVE, ErlangReceiveExpression.class);
  }

  private static boolean completeIf(final @NotNull PsiFile file, final @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_IF, ErlangIfExpression.class);
  }

  private static boolean completeExpression(final @NotNull PsiFile file, final @NotNull Editor editor, @NotNull IElementType lastElementType, @NotNull Class expectedParentClass) {
    final PsiElement lastElement = file.findElementAt(editor.getCaretModel().getOffset() - 1);
    final PsiElement parent = lastElement != null ? lastElement.getParent() : null;

    if (!(lastElement instanceof LeafPsiElement && ((LeafPsiElement) lastElement).getElementType().equals(lastElementType))) return false;
    if (!(expectedParentClass.isInstance(parent))) return false;
    if (hasEnd(parent)) return false;

    appendEndAndMoveCaret(file, editor, lastElement.getTextRange().getEndOffset(), hasSiblings(parent));

    return true;
  }

  private static void appendEndAndMoveCaret(final @NotNull PsiFile file, final @NotNull Editor editor, final int offset, final boolean addComma) {
    final Project project = editor.getProject();

    if (project == null) return;
    
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        String endText = "\n\nend" + (addComma ? "," : "");
        editor.getDocument().insertString(offset, endText);

        PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument());
        CodeStyleManager.getInstance(project).adjustLineIndent(file, TextRange.from(offset, offset + endText.length()));
        CaretModel caretModel = editor.getCaretModel();
        caretModel.moveCaretRelatively(0, 1, false, false, true);
        CodeStyleManager.getInstance(project).adjustLineIndent(file, caretModel.getOffset());
      }
    });
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

  private static boolean hasSiblings(@NotNull PsiElement expression) {
    if (expression instanceof ErlangBeginEndExpression) return hasSiblings((ErlangBeginEndExpression) expression);
    if (expression instanceof ErlangCaseExpression)     return hasSiblings((ErlangCaseExpression) expression);
    if (expression instanceof ErlangReceiveExpression)  return hasSiblings((ErlangReceiveExpression) expression);
    if (expression instanceof ErlangIfExpression)       return hasSiblings((ErlangIfExpression) expression);

    return false;
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

  private static boolean hasSiblings(@NotNull ErlangCaseExpression caseExpression) {
    List<ErlangCrClause> crClauses = caseExpression.getCrClauseList();

    if (crClauses.isEmpty()) return false;

    return crClauses.get(0).getClauseBody() == null;
  }

  private static boolean hasSiblings(@NotNull ErlangReceiveExpression receiveExpression) {
    List<ErlangCrClause> crClauses = receiveExpression.getCrClauseList();

    if (crClauses.isEmpty()) return false;

    return crClauses.get(0).getClauseBody() == null;
  }

  private static boolean hasSiblings(@NotNull ErlangIfExpression ifExpression) {
    ErlangIfClauses ifClauses = ifExpression.getIfClauses();

    if (ifClauses == null) return false;

    List<ErlangIfClause> ifClauseList = ifClauses.getIfClauseList();

    if (ifClauseList.isEmpty()) return false;

    return ifClauseList.get(0).getClauseBody() == null;
  }

  private static boolean shouldEndWithEnd(@NotNull PsiElement element) {
    return element instanceof ErlangBeginEndExpression ||
           element instanceof ErlangCaseExpression ||
           element instanceof ErlangIfExpression ||
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