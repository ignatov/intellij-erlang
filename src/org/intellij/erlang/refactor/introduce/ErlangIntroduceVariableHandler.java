package org.intellij.erlang.refactor.introduce;

import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pass;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.IntroduceTargetChooser;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.introduce.inplace.InplaceVariableIntroducer;
import com.intellij.refactoring.introduce.inplace.OccurrencesChooser;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.refactor.ErlangRefactoringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangIntroduceVariableHandler implements RefactoringActionHandler {
  @Override
  public void invoke(@NotNull Project project, @NotNull Editor editor, @NotNull PsiFile file, DataContext dataContext) {
    if (!CommonRefactoringUtil.checkReadOnlyStatus(file)) {
      return;
    }

    final SelectionModel selectionModel = editor.getSelectionModel();
    if (selectionModel.hasSelection()) {
      PsiElement element1 = file.findElementAt(selectionModel.getSelectionStart());
      PsiElement element2 = file.findElementAt(selectionModel.getSelectionEnd() - 1);
      if (element1 instanceof PsiWhiteSpace) {
        element1 = file.findElementAt(element1.getTextRange().getEndOffset());
      }
      if (element2 instanceof PsiWhiteSpace) {
        element2 = file.findElementAt(element2.getTextRange().getStartOffset() - 1);
      }

      if (element1 == null || element2 == null) {
        showCannotPerformError(project, editor);
        return;
      }

      ErlangExpression selectedExpression = getSelectedExpression(element1, element2);

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

  private static void smartIntroduce(@NotNull final Editor editor, @NotNull PsiFile file) {
    int offset = editor.getCaretModel().getOffset();
    PsiElement elementAtCaret = file.findElementAt(offset);
    if (!checkIntroduceContext(file, editor, elementAtCaret)) return;
    final List<ErlangExpression> expressions = new ArrayList<ErlangExpression>();
    while (elementAtCaret != null) {
      if (elementAtCaret instanceof ErlangClauseBody) {
        break;
      }
      else if (elementAtCaret instanceof ErlangExpression && !(elementAtCaret instanceof ErlangAssignmentExpression)) {
        boolean isQualifiedFunCall = elementAtCaret instanceof ErlangFunctionCallExpression &&
          elementAtCaret.getParent() instanceof ErlangGlobalFunctionCallExpression;
        if (!isQualifiedFunCall) {
          expressions.add((ErlangExpression) elementAtCaret);
        }
      }
      elementAtCaret = elementAtCaret.getParent();
    }
    if (expressions.size() == 1 || ApplicationManager.getApplication().isUnitTestMode()) {
      performOnElement(editor, expressions.get(0));
    }
    else if (expressions.size() > 1) {
      IntroduceTargetChooser.showChooser(editor, expressions,
        new Pass<ErlangExpression>() {
          @Override
          public void pass(@NotNull ErlangExpression expression) {
            performOnElement(editor, expression);
          }
        },
        new Function<ErlangExpression, String>() {
          public String fun(@NotNull ErlangExpression expression) {
            return expression.getText();
          }
        }
      );
    }
  }

  private static void performActionOnElementOccurrences(@NotNull final Editor editor, @NotNull final ErlangExpression expression) {
    if (editor.getSettings().isVariableInplaceRenameEnabled()) {
      OccurrencesChooser.simpleChooser(editor).showChooser(
        expression,
        getOccurrences(expression),
        new Pass<OccurrencesChooser.ReplaceChoice>() {
          @Override
          public void pass(OccurrencesChooser.ReplaceChoice replaceChoice) {
            performInplaceIntroduce(editor, expression, replaceChoice);
          }
        });
    }
  }

  private static void performOnElement(@NotNull Editor editor, @NotNull ErlangExpression expression) {
    performActionOnElementOccurrences(editor, expression);
  }

  private static void performInplaceIntroduce(@NotNull Editor editor, @NotNull ErlangExpression expression, @Nullable OccurrencesChooser.ReplaceChoice replaceChoice) {
    boolean replaceAll = replaceChoice == OccurrencesChooser.ReplaceChoice.ALL;
    List<PsiElement> occurrences = replaceAll ? getOccurrences(expression) : ContainerUtil.<PsiElement>list(expression);
    PsiElement declaration = performElement(editor, expression, occurrences);

    final ErlangQVar target = PsiTreeUtil.findChildOfType(declaration, ErlangQVar.class);
    if (target == null) {
      return;
    }

    editor.getCaretModel().moveToOffset(target.getTextRange().getStartOffset());
    final InplaceVariableIntroducer<PsiElement> introducer = new ErlangInplaceVariableIntroducer(target, editor, expression.getProject(), occurrences);
    introducer.performInplaceRefactoring(new LinkedHashSet<String>());
  }

  @Nullable
  private static PsiElement performElement(Editor editor, @NotNull ErlangExpression expression, @NotNull List<PsiElement> occurrences) {
    InitializerTextBuilder builder = new InitializerTextBuilder();
    expression.accept(builder);
    String newName = builder.result();
    String newText = newName + " = " + expression.getText();
    Project project = expression.getProject();
    PsiElement declaration = null;
    try {
      try {
        declaration = ErlangElementFactory.createExpressionFromText(project, newText);
      } catch (Exception e) {
        declaration = ErlangElementFactory.createExpressionFromText(project, "PlaceHolder" + " = " + expression.getText());
      }
    } catch (Exception e) { //
    }

    if (declaration == null) {
      showCannotPerformError(project, editor);
      return declaration;
    }

    declaration = performReplace(newName, declaration, expression, occurrences);
    declaration = CodeInsightUtilBase.forcePsiPostprocessAndRestoreElement(declaration);

    return declaration;
  }

  private static void modifyDeclaration(@NotNull PsiElement declaration) {
    PsiElement comma = ErlangElementFactory.createLeafFromText(declaration.getProject(), ",\n");
    final PsiElement newLineNode = PsiParserFacade.SERVICE.getInstance(declaration.getProject()).createWhiteSpaceFromText("\n");
    final PsiElement parent = declaration.getParent();
    PsiElement psiElement = parent.addAfter(comma, declaration);
    parent.addAfter(newLineNode, psiElement);
  }

  private static PsiElement performReplace(@NotNull final String newName,
                                           @NotNull final PsiElement declaration,
                                           @NotNull final ErlangExpression initializer,
                                           @NotNull final List<PsiElement> occurrences) {
    final Project project = declaration.getProject();
    return new WriteCommandAction<PsiElement>(project, "Extract variable", initializer.getContainingFile()) {
      protected void run(@NotNull final Result<PsiElement> result) throws Throwable {
        final PsiElement createdDeclaration = addDeclaration(declaration, occurrences);
        result.setResult(createdDeclaration);
        if (createdDeclaration != null) {
          modifyDeclaration(createdDeclaration);
        }
        PsiElement newExpression = ErlangElementFactory.createQVarFromText(project, newName);
        for (PsiElement occurrence : occurrences) {
          occurrence.replace(newExpression);
        }
      }
    }.execute().getResultObject();
  }

  private static PsiElement addDeclaration(@NotNull PsiElement declaration, @NotNull List<PsiElement> occurrences) {
    PsiElement anchor = findAnchor(occurrences);
    assert anchor != null;
    final PsiElement parent = anchor.getParent();
    return parent.addBefore(declaration, anchor);
  }

  private static boolean checkIntroduceContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element) {
    if (!isValidIntroduceContext(element)) {
      showCannotPerformError(file.getProject(), editor);
      return false;
    }
    return true;
  }

  private static void showCannotPerformError(@NotNull Project project, @NotNull Editor editor) {
    CommonRefactoringUtil.showErrorHint(project, editor, "Cannot perform refactoring", "Cannot Perform Refactoring", "refactoring.extractMethod");
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
      final ErlangCompositeElement clause = PsiTreeUtil.getParentOfType(anchor, ErlangClauseBody.class, ErlangTryExpressionsClause.class);

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
  private static List<PsiElement> getOccurrences(@NotNull final ErlangExpression expression) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(expression, ErlangFunction.class);
    return ErlangRefactoringUtil.getOccurrences(expression, function);
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull PsiElement[] elements, DataContext dataContext) {
  }

  private static class ErlangInplaceVariableIntroducer extends InplaceVariableIntroducer<PsiElement> {
    private final ErlangQVar myTarget;

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

  private static class InitializerTextBuilder extends PsiRecursiveElementVisitor {
    private final StringBuilder myResult = new StringBuilder();

    @Override
    public void visitWhiteSpace(@NotNull PsiWhiteSpace space) {
      myResult.append(space.getText().replace('\n', ' '));
    }

    @Override
    public void visitElement(@NotNull PsiElement element) {
      if (element instanceof ErlangNamedElement) {
        myResult.append(((ErlangNamedElement) element).getName());
        return;
      }
      else if (element instanceof ErlangQAtom) {
        myResult.append(element.getText());
        return;
      }
      else if (element instanceof ErlangFunctionCallExpression) {
        myResult.append(((ErlangFunctionCallExpression) element).getNameIdentifier().getText());
        return;
      }
      else if (element instanceof ErlangCaseExpression) {
        ErlangExpression expression = ((ErlangCaseExpression) element).getExpression();
        myResult.append("Case");
        if (expression != null) {
          InitializerTextBuilder b = new InitializerTextBuilder();
          expression.accept(b);
          myResult.append(b.result());
        }
        return;
      }
      else if (element instanceof ErlangFunExpression) {
        myResult.append("Fun");
        ErlangFunClauses funClauses1 = ((ErlangFunExpression) element).getFunClauses();
        List<ErlangFunClause> funClauses = funClauses1 != null ? funClauses1.getFunClauseList() : ContainerUtil.<ErlangFunClause>emptyList();
        ErlangFunClause firstItem = ContainerUtil.getFirstItem(funClauses);
        if (firstItem != null) {
          InitializerTextBuilder b = new InitializerTextBuilder();
          firstItem.getArgumentDefinitionList().accept(b);
          myResult.append(b.result());
        }
        return;
      }
      super.visitElement(element);
    }

    @NotNull
    public String result() {
      String s = StringUtil.toTitleCase(myResult.toString())
        .replaceAll("_", "")
        .replaceAll("\\?", "")
        .replaceAll(" ", "");
      return s.isEmpty() ? "PlaceHolder" : s;
    }
  }
}
