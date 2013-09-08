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

import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.Pass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiParserFacade;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.introduce.inplace.InplaceVariableIntroducer;
import com.intellij.refactoring.introduce.inplace.OccurrencesChooser;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.refactor.ErlangRefactoringUtil;
import org.intellij.erlang.refactor.VariableTextBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashSet;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangIntroduceVariableHandler implements RefactoringActionHandler {

  public enum ReplaceStrategy {
    ALL, SINGLE, ASK
  }

  private ReplaceStrategy myReplaceStrategy;

  public ErlangIntroduceVariableHandler(ReplaceStrategy replaceStrategy) {
    myReplaceStrategy = replaceStrategy;
  }

  public ErlangIntroduceVariableHandler() {
    this(ReplaceStrategy.ASK);
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file, @Nullable DataContext dataContext) {
    if (!CommonRefactoringUtil.checkReadOnlyStatus(file)) {
      return;
    }

    SelectionModel selectionModel = editor.getSelectionModel();
    if (selectionModel.hasSelection()) {
      Pair<PsiElement, PsiElement> pair = ErlangRefactoringUtil.selectionToElements(file, selectionModel);

      if (pair.first == null || pair.second == null) {
        showCannotPerformError(project, editor);
        return;
      }

      ErlangExpression selectedExpression = getSelectedExpression(pair.first, pair.second);

      if (selectedExpression == null) {
        showCannotPerformError(project, editor);
      }
      else {
        performOnElement(editor, selectedExpression);
      }
      return;
    }

    smartIntroduce(editor, file);
  }

  @Nullable
  private static ErlangExpression getSelectedExpression(@NotNull PsiElement element1, @NotNull PsiElement element2) {
    PsiElement parent = PsiTreeUtil.findCommonParent(element1, element2);
    if (parent == null) {
      return null;
    }
    if (parent instanceof ErlangExpression) {
      return (ErlangExpression) parent;
    }
    return PsiTreeUtil.getParentOfType(parent, ErlangExpression.class);
  }

  private void smartIntroduce(@NotNull final Editor editor, @NotNull PsiFile file) {
    ErlangRefactoringUtil.smartIntroduce(editor, file,
      new ErlangRefactoringUtil.Extractor() {
        @Override
        public boolean checkContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element) {
          return ErlangIntroduceVariableHandler.checkIntroduceContext(file, editor, element);
        }
        @Override
        public void process(@NotNull Editor editor, @NotNull ErlangExpression expression) {
          performOnElement(editor, expression);
        }
      }
    );
  }

  private void performActionOnElementOccurrences(@NotNull final Editor editor, @NotNull final ErlangExpression expression) {
    if (!editor.getSettings().isVariableInplaceRenameEnabled()) return;
    switch (myReplaceStrategy) {
      case ASK: {
        OccurrencesChooser.simpleChooser(editor).showChooser(
          expression,
          getOccurrences(expression),
          new Pass<OccurrencesChooser.ReplaceChoice>() {
            @Override
            public void pass(OccurrencesChooser.ReplaceChoice replaceChoice) {
              performInplaceIntroduce(editor, expression, replaceChoice == OccurrencesChooser.ReplaceChoice.ALL);
            }
          });
        break;
      }
      case ALL: {
        performInplaceIntroduce(editor, expression, true);
        break;
      }
      case SINGLE: {
        performInplaceIntroduce(editor, expression, false);
        break;
      }
    }
  }

  private void performOnElement(@NotNull Editor editor, @NotNull ErlangExpression expression) {
    performActionOnElementOccurrences(editor, expression);
  }

  private static void performInplaceIntroduce(@NotNull Editor editor, @NotNull ErlangExpression expression, boolean replaceAll) {
    List<PsiElement> occurrences = replaceAll ? getOccurrences(expression) : ContainerUtil.<PsiElement>list(expression);
    PsiElement declaration = performElement(editor, expression, occurrences);

    ErlangQVar target = PsiTreeUtil.findChildOfType(declaration, ErlangQVar.class);
    if (target == null) {
      return;
    }

    editor.getCaretModel().moveToOffset(target.getTextRange().getStartOffset());
    InplaceVariableIntroducer<PsiElement> introducer = new ErlangInplaceVariableIntroducer(target, editor, expression.getProject(), occurrences);
    introducer.performInplaceRefactoring(new LinkedHashSet<String>());
  }

  @Nullable
  private static PsiElement performElement(Editor editor, @NotNull ErlangExpression expression, @NotNull List<PsiElement> occurrences) {
    VariableTextBuilder builder = new VariableTextBuilder();
    expression.accept(builder);
    String newName = builder.result();
    ErlangExpression initializer = ErlangPsiImplUtil.getNotParenthesizedExpression(expression);
    String newText = initializer != null ? newName + " = " + initializer.getText() : null;
    Project project = expression.getProject();
    PsiElement declaration = newText != null ? ErlangElementFactory.createExpressionFromText(project, newText) : null;
    if (declaration == null) {
      showCannotPerformError(project, editor);
      return null;
    }

    declaration = performReplace(newName, declaration, expression, occurrences);
    declaration = CodeInsightUtilBase.forcePsiPostprocessAndRestoreElement(declaration);

    return declaration;
  }

  private static void modifyDeclaration(@NotNull PsiElement declaration) {
    if (PsiTreeUtil.getParentOfType(declaration, ErlangArgumentDefinition.class) != null) return;

    PsiElement comma = ErlangElementFactory.createLeafFromText(declaration.getProject(), ",\n");
    PsiElement newLineNode = PsiParserFacade.SERVICE.getInstance(declaration.getProject()).createWhiteSpaceFromText("\n");
    PsiElement parent = declaration.getParent();
    PsiElement psiElement = parent.addAfter(comma, declaration);
    parent.addAfter(newLineNode, psiElement);
  }

  private static PsiElement performReplace(@NotNull final String newName,
                                           @NotNull final PsiElement declaration,
                                           @NotNull ErlangExpression initializer,
                                           @NotNull final List<PsiElement> occurrences) {
    final Project project = declaration.getProject();
    return new WriteCommandAction<PsiElement>(project, "Extract variable", initializer.getContainingFile()) {
      protected void run(@NotNull Result<PsiElement> result) throws Throwable {
        PsiElement createdDeclaration = replaceLeftmostArgumentDefinition(declaration, occurrences);
        if (createdDeclaration == null) {
          createdDeclaration = addDeclaration(declaration, occurrences);
        }
        result.setResult(createdDeclaration);
        if (createdDeclaration != null) {
          modifyDeclaration(createdDeclaration);
        }
        PsiElement newExpression = ErlangElementFactory.createQVarFromText(project, newName);
        for (PsiElement occurrence : occurrences) {
          ErlangPsiImplUtil.getOutermostParenthesizedExpression((ErlangExpression) occurrence).replace(newExpression);
        }
      }
    }.execute().getResultObject();
  }

  private static PsiElement addDeclaration(@NotNull PsiElement declaration, @NotNull List<PsiElement> occurrences) {
    PsiElement anchor = findAnchor(occurrences);
    assert anchor != null;
    PsiElement parent = anchor.getParent();
    return parent.addBefore(declaration, anchor);
  }

  @Nullable
  private static PsiElement replaceLeftmostArgumentDefinition(@NotNull PsiElement declaration, @NotNull List<PsiElement> occurrences) {
    PsiElement argDef = extractLeftmostArgumentDefinition(occurrences);

    return argDef == null ? argDef : argDef.replace(declaration);
  }

  @Nullable
  private static PsiElement extractLeftmostArgumentDefinition(@NotNull List<PsiElement> occurrences) {
    int occurrenceOffset = Integer.MAX_VALUE;
    int occurrenceIndex = -1;
    int currentOccurrenceIndex = 0;

    for (PsiElement occurrence : occurrences) {
      ErlangArgumentDefinition argDef = PsiTreeUtil.getParentOfType(occurrence, ErlangArgumentDefinition.class);

      if (argDef != null) {
        int startOffset = argDef.getTextRange().getStartOffset();

        if (startOffset < occurrenceOffset) {
          occurrenceOffset = startOffset;
          occurrenceIndex = currentOccurrenceIndex;
        }
      }

      currentOccurrenceIndex++;
    }

    return occurrenceIndex == -1 ? null : occurrences.remove(occurrenceIndex);
  }

  private static boolean checkIntroduceContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element) {
    if (!isValidIntroduceContext(element)) {
      showCannotPerformError(file.getProject(), editor);
      return false;
    }
    return true;
  }

  private static void showCannotPerformError(@NotNull Project project, @NotNull Editor editor) {
    CommonRefactoringUtil.showErrorHint(project, editor, "Cannot perform refactoring", "Cannot Perform Refactoring", "refactoring.extractVariable");
  }

  private static boolean isValidIntroduceContext(@Nullable PsiElement element) {
    return PsiTreeUtil.getParentOfType(element, ErlangClauseBody.class) != null;
  }

  @Nullable
  private static PsiElement findAnchor(@NotNull List<PsiElement> occurrences) {
    PsiElement anchor = occurrences.get(0);
    next:
    do {
      //noinspection unchecked
      ErlangCompositeElement clause = PsiTreeUtil.getParentOfType(anchor, ErlangClauseBody.class, ErlangTryExpressionsClause.class);

      int minOffset = Integer.MAX_VALUE;
      for (PsiElement element : occurrences) {
        minOffset = Math.min(minOffset, element.getTextOffset());
        if (!PsiTreeUtil.isAncestor(clause, element, true)) {
          if (clause == null) return null;
          anchor = clause;
          continue next;
        }
      }

      if (clause == null) {
        return null;
      }

      PsiElement child = null;
      PsiElement[] children = clause.getChildren();
      for (PsiElement aChildren : children) {
        child = aChildren;
        if (child.getTextRange().contains(minOffset)) {
          break;
        }
      }

      return child;
    }
    while (true);
  }

  @NotNull
  private static List<PsiElement> getOccurrences(@NotNull ErlangExpression expression) {
    ErlangFunctionClause function = PsiTreeUtil.getParentOfType(expression, ErlangFunctionClause.class);
    return ErlangRefactoringUtil.getOccurrences(expression, function);
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull PsiElement[] elements, DataContext dataContext) {
  }

  private static class ErlangInplaceVariableIntroducer extends InplaceVariableIntroducer<PsiElement> {
    private ErlangQVar myTarget;

    public ErlangInplaceVariableIntroducer(ErlangQVar target,
                                           Editor editor,
                                           Project project,
                                           @NotNull List<PsiElement> occurrences) {
      super(target, editor, project, "Introduce Variable", occurrences.toArray(new PsiElement[occurrences.size()]), null);
      myTarget = target;
    }

    @Override
    protected PsiElement checkLocalScope() {
      return myTarget.getContainingFile();
    }
  }
}
