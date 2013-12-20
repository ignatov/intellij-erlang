package org.intellij.erlang.editor;

import com.intellij.codeInsight.CodeInsightSettings;
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

public class ErlangEnterHandler extends EnterHandlerDelegateAdapter {

  @Override
  public Result preprocessEnter(@NotNull PsiFile file, @NotNull Editor editor, @NotNull Ref<Integer> caretOffset, @NotNull Ref<Integer> caretAdvance, @NotNull DataContext dataContext, EditorActionHandler originalHandler) {
    if (!(file instanceof ErlangFile)) return Result.Continue;
    if (!CodeInsightSettings.getInstance().INSERT_BRACE_ON_ENTER) return Result.Continue;

    if (completeBeginEnd(file, editor) ||
        completeCaseOf(file, editor) ||
        completeReceive(file, editor) ||
        completeIf(file, editor) ||
        completeTry(file, editor)) {
      return Result.Stop;
    }

    return Result.Continue;
  }

  private static boolean completeBeginEnd(@NotNull PsiFile file, @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_BEGIN, ErlangBeginEndExpression.class);
  }

  private static boolean completeCaseOf(@NotNull PsiFile file, @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_OF, ErlangCaseExpression.class);
  }

  private static boolean completeReceive(@NotNull PsiFile file, @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_RECEIVE, ErlangReceiveExpression.class);
  }

  private static boolean completeIf(@NotNull PsiFile file, @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_IF, ErlangIfExpression.class);
  }

  private static boolean completeTry(@NotNull PsiFile file, @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_CATCH, ErlangTryCatch.class) ||
           completeExpression(file, editor, ErlangTypes.ERL_AFTER, ErlangTryCatch.class) ||
           completeExpression(file, editor, ErlangTypes.ERL_OF, ErlangTryExpression.class);
  }

  private static boolean completeExpression(@NotNull PsiFile file, @NotNull Editor editor, @NotNull IElementType lastElementType, @NotNull Class expectedParentClass) {
     PsiElement lastElement = file.findElementAt(editor.getCaretModel().getOffset() - 1);
     PsiElement parent = lastElement != null ? lastElement.getParent() : null;

    if (!(lastElement instanceof LeafPsiElement && ((LeafPsiElement) lastElement).getElementType().equals(lastElementType))) return false;
    if (!(expectedParentClass.isInstance(parent))) return false;
    if (hasEnd(parent)) return false;

    appendEndAndMoveCaret(file, editor, lastElement.getTextRange().getEndOffset(), needCommaAfter(parent));

    return true;
  }

  private static void appendEndAndMoveCaret(@NotNull final PsiFile file, @NotNull final Editor editor, final int offset, final boolean addComma) {
    final Project project = editor.getProject();
    if (project == null) return;
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        String endText = "\n\nend" + (addComma ? "," : "");
        editor.getDocument().insertString(offset, endText);

        PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument());
        CodeStyleManager.getInstance(project).adjustLineIndent(file, TextRange.from(offset, endText.length()));
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

  private static boolean needCommaAfter(@NotNull PsiElement parent) {
    if (parent instanceof ErlangBeginEndExpression) return needCommaAfter((ErlangBeginEndExpression) parent);
    if (parent instanceof ErlangClauseOwner)        return needCommaAfter((ErlangClauseOwner) parent);
    if (parent instanceof ErlangIfExpression)       return needCommaAfter((ErlangIfExpression) parent);
    return false;
  }

  private static boolean needCommaAfter(@NotNull ErlangBeginEndExpression beginEndExpr) {
    ErlangBeginEndBody beginEndBody = beginEndExpr.getBeginEndBody();
    if (beginEndBody == null) return false;
    ErlangExpression expression = PsiTreeUtil.getChildOfType(beginEndBody, ErlangExpression.class);

    if (expression == null) return false;
    if (expression instanceof ErlangCatchExpression) {
      ErlangTryExpression tryExpression = PsiTreeUtil.getParentOfType(beginEndExpr, ErlangTryExpression.class);
      return tryExpression == null || tryExpression.getTryCatch() != null;
    }
    return true;
  }

  private static boolean needCommaAfter(@NotNull ErlangClauseOwner owner) {
    List<ErlangCrClause> crClauses = owner.getCrClauseList();
    if (crClauses.isEmpty()) return false;
    return crClauses.get(0).getClauseBody() == null;
  }

  private static boolean needCommaAfter(@NotNull ErlangIfExpression ifExpression) {
    List<ErlangIfClause> ifClauseList = ifExpression.getIfClauseList();
    if (ifClauseList.isEmpty()) return false;
    ErlangIfClause firstIfClause = ifClauseList.get(0);
    return firstIfClause.getClauseBody() == null ||
      PsiTreeUtil.findChildOfType(firstIfClause, ErlangIfExpression.class) != null;
  }

  private static boolean shouldEndWithEnd(@NotNull PsiElement element) {
    return element instanceof ErlangBeginEndExpression ||
           element instanceof ErlangCaseExpression ||
           element instanceof ErlangIfExpression ||
           element instanceof ErlangReceiveExpression ||
           element instanceof ErlangFunExpression && PsiTreeUtil.getChildOfType(element, ErlangFunClauses.class) != null ||
           element instanceof ErlangTryExpression;
  }

  @Nullable
  private static PsiElement getEnd(@Nullable PsiElement element) {
    if (element == null) return null;

    PsiElement lastChild = element.getLastChild();

    if (lastChild instanceof LeafPsiElement && ((LeafPsiElement)lastChild).getElementType().equals(ErlangTypes.ERL_END)) return lastChild;

    return null;
  }
}