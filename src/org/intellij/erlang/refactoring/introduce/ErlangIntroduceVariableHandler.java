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

package org.intellij.erlang.refactoring.introduce;

import com.intellij.codeInsight.CodeInsightUtilCore;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.Pass;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiParserFacade;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.introduce.inplace.InplaceVariableIntroducer;
import com.intellij.refactoring.introduce.inplace.OccurrencesChooser;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.refactoring.ErlangRefactoringUtil;
import org.intellij.erlang.refactoring.ErlangRefactoringUtil.Occurrence;
import org.intellij.erlang.refactoring.VariableTextBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;

public class ErlangIntroduceVariableHandler implements RefactoringActionHandler {
  private final static Logger LOG = Logger.getInstance(ErlangIntroduceVariableHandler.class);

  public enum ReplaceStrategy {
    ALL, SINGLE, ASK
  }

  private final ReplaceStrategy myReplaceStrategy;

  public ErlangIntroduceVariableHandler(ReplaceStrategy replaceStrategy) {
    myReplaceStrategy = replaceStrategy;
  }

  public ErlangIntroduceVariableHandler() {
    this(ReplaceStrategy.ASK);
  }

  @Override
  public void invoke(@NotNull Project project,
                     @NotNull Editor editor,
                     @NotNull PsiFile file,
                     @Nullable DataContext dataContext) {
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

      Occurrence selected = getSelectedOccurrence(pair.first, pair.second);

      if (selected == null) {
        showCannotPerformError(project, editor);
      }
      else {
        performActionOnElementOccurrences(editor, selected);
      }
      return;
    }

    smartIntroduce(editor, file);
  }

  @Nullable
  private static Occurrence getSelectedOccurrence(@NotNull PsiElement element1, @NotNull PsiElement element2) {
    PsiElement parent = PsiTreeUtil.findCommonParent(element1, element2);
    if (parent == null) {
      return null;
    }
    if (parent instanceof LeafPsiElement && parent.getParent() instanceof ErlangAdditiveExpression) {
      parent = parent.getParent();
    }
    ErlangExpression runEnd = ErlangRefactoringUtil.getLeadingRunEnd(parent, element2);
    if (runEnd != null) {
      return new Occurrence(((ErlangAdditiveExpression) parent).getExpressionList().getFirst(), runEnd);
    }
    ErlangExpression expression = parent instanceof ErlangExpression ? (ErlangExpression) parent :
                                  PsiTreeUtil.getParentOfType(parent, ErlangExpression.class);
    return expression == null ? null : new Occurrence(expression, expression);
  }

  private void smartIntroduce(@NotNull Editor editor, @NotNull PsiFile file) {
    ErlangRefactoringUtil.smartIntroduce(editor, file,
                                         new ErlangRefactoringUtil.Extractor() {
                                           @Override
                                           public boolean checkContext(@NotNull PsiFile file,
                                                                       @NotNull Editor editor,
                                                                       @Nullable PsiElement element) {
                                             return checkIntroduceContext(file, editor, element);
                                           }

                                           @Override
                                           public void process(@NotNull Editor editor,
                                                               @NotNull Occurrence expression) {
                                             performActionOnElementOccurrences(editor, expression);
                                           }
                                         }
    );
  }

  private void performActionOnElementOccurrences(@NotNull final Editor editor,
                                                 @NotNull final Occurrence expression) {
    if (!editor.getSettings().isVariableInplaceRenameEnabled()) return;
    switch (myReplaceStrategy) {
      case ASK -> new OccurrencesChooser<Occurrence>(editor) {
        @Override
        protected TextRange getOccurrenceRange(Occurrence occurrence) {
          return occurrence.getTextRange();
        }
      }.showChooser(
        expression,
        getOccurrences(expression),
        new Pass<>() {
          @Override
          public void pass(OccurrencesChooser.ReplaceChoice replaceChoice) {
            performInplaceIntroduce(editor, expression, replaceChoice == OccurrencesChooser.ReplaceChoice.ALL);
          }
        });
      case ALL -> performInplaceIntroduce(editor, expression, true);
      case SINGLE -> performInplaceIntroduce(editor, expression, false);
    }
  }

  private static void performInplaceIntroduce(@NotNull Editor editor,
                                              @NotNull Occurrence selected,
                                              boolean replaceAll) {
    List<Occurrence> occurrences = replaceAll ? getOccurrences(selected) : Collections.singletonList(selected);
    Project project = selected.first().getProject();
    PsiElement declaration = performElement(editor, selected, occurrences);

    ErlangQVar target = PsiTreeUtil.findChildOfType(declaration, ErlangQVar.class);
    if (target == null) {
      return;
    }

    editor.getCaretModel().moveToOffset(target.getTextRange().getStartOffset());
    InplaceVariableIntroducer<PsiElement> introducer = new ErlangInplaceVariableIntroducer(target, editor, project, firstElements(occurrences));
    introducer.performInplaceRefactoring(new LinkedHashSet<>());
  }

  @Nullable
  private static PsiElement performElement(Editor editor,
                                           @NotNull Occurrence selected,
                                           @NotNull List<Occurrence> occurrences) {
    ErlangExpression expression = ErlangRefactoringUtil.getExpression(selected);
    VariableTextBuilder builder = new VariableTextBuilder();
    expression.accept(builder);
    String newName = builder.result();
    ErlangExpression initializer = ErlangPsiImplUtil.getNotParenthesizedExpression(expression);
    String newText = initializer != null ? newName + " = " + initializer.getText() : null;
    Project project = selected.first().getProject();
    if (PsiTreeUtil.hasErrorElements(expression)) {
      showCannotPerformError(project, editor, "Selected expression contains errors");
      return null;
    }
    PsiElement declaration = null;
    try {
      declaration = newText != null ? ErlangElementFactory.createExpressionFromText(project, newText) : null;
    }
    catch (Exception e) {
      LOG.error("Can't create a new expression:\n" + newText + "\n", e);
    }
    if (declaration == null) {
      showCannotPerformError(project, editor);
      return null;
    }

    declaration = performReplace(newName, declaration, selected.first().getContainingFile(), occurrences);
    declaration = CodeInsightUtilCore.forcePsiPostprocessAndRestoreElement(declaration);

    return declaration;
  }

  private static void modifyDeclaration(@NotNull PsiElement declaration) {
    if (PsiTreeUtil.getParentOfType(declaration, ErlangArgumentDefinition.class) != null) return;

    PsiElement comma = ErlangElementFactory.createLeafFromText(declaration.getProject(), ",\n");
    PsiElement newLineNode = PsiParserFacade.getInstance(declaration.getProject()).createWhiteSpaceFromText("\n");
    PsiElement parent = declaration.getParent();
    PsiElement psiElement = parent.addAfter(comma, declaration);
    parent.addAfter(newLineNode, psiElement);
  }

  private static PsiElement performReplace(@NotNull final String newName,
                                           @NotNull final PsiElement declaration,
                                           @NotNull PsiFile file,
                                           @NotNull final List<Occurrence> occurrences) {
    Project project = declaration.getProject();
    return WriteCommandAction.writeCommandAction(project, file).withName("Extract Variable").compute(() -> {
      boolean alone = occurrences.size() == 1 && !occurrences.getFirst().isRun() &&
                      occurrences.getFirst().first().getParent() instanceof ErlangClauseBody;
      PsiElement createdDeclaration = replaceLeftmostArgumentDefinition(declaration, occurrences);
      if (createdDeclaration == null) {
        createdDeclaration = addDeclaration(declaration, occurrences);
      }
      if (alone) {
        Occurrence firstItem = ContainerUtil.getFirstItem(occurrences);
        if (firstItem != null) firstItem.first().delete();
      }
      else {
        if (createdDeclaration != null) {
          modifyDeclaration(createdDeclaration);
        }

        PsiElement newExpression = ErlangElementFactory.createQVarFromText(project, newName);
        for (Occurrence occurrence : occurrences) {
          if (occurrence.isRun()) {
            PsiElement chain = occurrence.first().getParent();
            chain.addBefore(newExpression, occurrence.first());
            chain.deleteChildRange(occurrence.first(), occurrence.last());
          }
          else {
            ErlangPsiImplUtil.getOutermostParenthesizedExpression((ErlangExpression) occurrence.first()).replace(newExpression);
          }
        }
      }
      return createdDeclaration;
    });
  }

  private static PsiElement addDeclaration(@NotNull PsiElement declaration, @NotNull List<Occurrence> occurrences) {
    PsiElement anchor = findAnchor(firstElements(occurrences));
    assert anchor != null;
    PsiElement parent = anchor.getParent();
    return parent.addBefore(declaration, anchor);
  }

  @Nullable
  private static PsiElement replaceLeftmostArgumentDefinition(@NotNull PsiElement declaration,
                                                              @NotNull List<Occurrence> occurrences) {
    Occurrence argDef = extractLeftmostArgumentDefinition(occurrences);
    return argDef == null ? null : argDef.first().replace(declaration);
  }

  @Nullable
  private static Occurrence extractLeftmostArgumentDefinition(@NotNull List<Occurrence> occurrences) {
    int occurrenceOffset = Integer.MAX_VALUE;
    int occurrenceIndex = -1;
    int currentOccurrenceIndex = 0;

    for (Occurrence occurrence : occurrences) {
      ErlangArgumentDefinition argDef = occurrence.isRun() ? null :
                                        PsiTreeUtil.getParentOfType(occurrence.first(), ErlangArgumentDefinition.class);

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

  private static boolean checkIntroduceContext(@NotNull PsiFile file,
                                               @NotNull Editor editor,
                                               @Nullable PsiElement element) {
    if (!isValidIntroduceContext(element)) {
      showCannotPerformError(file.getProject(), editor);
      return false;
    }
    return true;
  }

  private static void showCannotPerformError(@NotNull Project project, @NotNull Editor editor) {
    showCannotPerformError(project, editor, "Cannot Perform Refactoring");
  }

  private static void showCannotPerformError(@NotNull Project project,
                                             @NotNull Editor editor,
                                             @NotNull String message) {
    CommonRefactoringUtil.showErrorHint(project, editor, message, "Cannot Perform Refactoring", "refactoring.extractVariable");
  }

  private static boolean isValidIntroduceContext(@Nullable PsiElement element) {
    return PsiTreeUtil.getParentOfType(element, ErlangClauseBody.class) != null;
  }

  @Nullable
  private static PsiElement findAnchor(@NotNull List<PsiElement> occurrences) {
    PsiElement anchor = occurrences.getFirst();
    next:
    do {
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
  private static List<Occurrence> getOccurrences(@NotNull Occurrence expression) {
    ErlangFunctionClause function = PsiTreeUtil.getParentOfType(expression.first(), ErlangFunctionClause.class);
    return ErlangRefactoringUtil.getOccurrences(expression, function);
  }

  @NotNull
  private static List<PsiElement> firstElements(@NotNull List<Occurrence> occurrences) {
    return ContainerUtil.map(occurrences, Occurrence::first);
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull PsiElement @NotNull [] elements, DataContext dataContext) {
  }

  private static class ErlangInplaceVariableIntroducer extends InplaceVariableIntroducer<PsiElement> {
    private final ErlangQVar myTarget;

    public ErlangInplaceVariableIntroducer(ErlangQVar target,
                                           Editor editor,
                                           Project project,
                                           @NotNull List<PsiElement> occurrences) {
      super(target, editor, project, "Introduce Variable", occurrences.toArray(new PsiElement[0]), null);
      myTarget = target;
    }

    @Override
    protected PsiElement checkLocalScope() {
      return myTarget.getContainingFile();
    }

    @Override
    protected void moveOffsetAfter(boolean success) {
      if (success) {
        reformatDeclaration();
      }
      super.moveOffsetAfter(success);
    }

    private void reformatDeclaration() {
      PsiDocumentManager.getInstance(myProject).commitDocument(myEditor.getDocument());
      ErlangAssignmentExpression declaration = PsiTreeUtil.getParentOfType(getVariable(), ErlangAssignmentExpression.class);
      if (declaration == null) return;
      PsiFile file = declaration.getContainingFile();
      TextRange range = declaration.getTextRange();
      WriteCommandAction.writeCommandAction(myProject, file).withName(getCommandName())
        .run(() -> CodeStyleManager.getInstance(myProject).reformatRange(file, range.getStartOffset(), range.getEndOffset()));
    }
  }
}
