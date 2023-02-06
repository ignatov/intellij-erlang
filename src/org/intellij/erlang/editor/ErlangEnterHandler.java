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

package org.intellij.erlang.editor;

import com.intellij.codeInsight.CodeInsightSettings;
import com.intellij.codeInsight.editorActions.enter.EnterHandlerDelegateAdapter;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorActionHandler;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ObjectUtils;
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
        completeTry(file, editor) ||
        completeMaybe(file, editor)) {
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
    return completeExpression(file, editor, ErlangTypes.ERL_CATCH, ErlangTryExpression.class) ||
           completeExpression(file, editor, ErlangTypes.ERL_AFTER, ErlangTryExpression.class) ||
           completeExpression(file, editor, ErlangTypes.ERL_OF, ErlangTryExpression.class);
  }

  private static boolean completeMaybe(@NotNull PsiFile file, @NotNull Editor editor) {
    return completeExpression(file, editor, ErlangTypes.ERL_MAYBE, ErlangMaybeExpression.class) ||
           completeExpression(file, editor, ErlangTypes.ERL_ELSE, ErlangMaybeExpression.class);
  }

  private static boolean completeExpression(@NotNull PsiFile file,
                                            @NotNull Editor editor,
                                            @NotNull IElementType lastElementType,
                                            @NotNull Class<?> expectedParentClass) {
    PsiElement lastElement = getPrecedingLeafOnSameLineOfType(file, editor, lastElementType);
    PsiElement parent = lastElement != null ? lastElement.getParent() : null;
    parent = expectedParentClass.isInstance(parent) && !hasEnd(parent) ? parent : null;

    if (parent != null) {
      appendEndAndMoveCaret(file, editor, lastElement.getTextRange().getEndOffset(), needCommaAfter(parent));
      return  true;
    }

    return false;
  }

  @Nullable
  private static LeafPsiElement getPrecedingLeafOnSameLineOfType(@NotNull PsiFile file, @NotNull Editor editor,
                                                                 @NotNull IElementType type) {
    CaretModel caretModel = editor.getCaretModel();
    PsiElement element = file.findElementAt(caretModel.getOffset() - 1);
    if (element instanceof PsiWhiteSpace) {
      ASTNode node = element.getNode();
      ASTNode previousLeaf = FormatterUtil.getPreviousLeaf(node, TokenType.WHITE_SPACE, TokenType.ERROR_ELEMENT);
      element = previousLeaf != null ? previousLeaf.getPsi() : null;
    }
    LeafPsiElement leaf = ObjectUtils.tryCast(element, LeafPsiElement.class);
    return leaf != null && leaf.getElementType() == type &&
      editor.offsetToLogicalPosition(leaf.getTextOffset()).line == caretModel.getLogicalPosition().line ? leaf : null;
  }

  private static void appendEndAndMoveCaret(@NotNull final PsiFile file, @NotNull final Editor editor, final int offset, final boolean addComma) {
    final Project project = editor.getProject();
    if (project == null) return;
    ApplicationManager.getApplication().runWriteAction(() -> {
      CaretModel caretModel = editor.getCaretModel();
      caretModel.moveToOffset(offset);

      String endText = "\n\nend" + (addComma ? "," : "");
      editor.getDocument().insertString(offset, endText);

      PsiDocumentManager.getInstance(project).commitDocument(editor.getDocument());
      CodeStyleManager.getInstance(project).adjustLineIndent(file, TextRange.from(offset, endText.length()));

      caretModel.moveCaretRelatively(0, 1, false, false, true);
      CodeStyleManager.getInstance(project).adjustLineIndent(file, caretModel.getOffset());
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
    if (parent instanceof ErlangMaybeExpression)    return needCommaAfter((ErlangMaybeExpression) parent);
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
      return tryExpression == null || tryExpression.getCatch() != null || tryExpression.getAfter() != null;
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

  private static boolean needCommaAfter(@NotNull ErlangMaybeExpression maybeExpression) {
    ErlangMaybeMatchExprs maybeMatchExprs = maybeExpression.getMaybeMatchExprs();
    if (maybeMatchExprs == null) return false;

    ErlangExpression expression = PsiTreeUtil.getChildOfType(maybeMatchExprs, ErlangExpression.class);
    if(expression == null) return false;

    return true;
  }

  private static boolean shouldEndWithEnd(@NotNull PsiElement element) {
    return element instanceof ErlangBeginEndExpression ||
           element instanceof ErlangCaseExpression ||
           element instanceof ErlangIfExpression ||
           element instanceof ErlangReceiveExpression ||
           element instanceof ErlangFunExpression && PsiTreeUtil.getChildOfType(element, ErlangFunClauses.class) != null ||
           element instanceof ErlangTryExpression ||
           element instanceof ErlangMaybeExpression;
  }

  @Nullable
  private static PsiElement getEnd(@Nullable PsiElement element) {
    if (element == null) return null;

    PsiElement lastChild = element.getLastChild();

    if (lastChild instanceof LeafPsiElement && ((LeafPsiElement)lastChild).getElementType().equals(ErlangTypes.ERL_END)) return lastChild;

    return null;
  }
}