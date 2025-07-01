/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.google.common.base.CaseFormat;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.ObjectUtils;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.refactoring.ErlangRefactoringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class ErlangExtractFunctionHandler implements RefactoringActionHandler {
  private static final Logger LOGGER = Logger.getInstance(ErlangExtractFunctionHandler.class);

  @Override
  public void invoke(@NotNull final Project project,
                     @NotNull Editor editor,
                     @NotNull PsiFile file,
                     @Nullable DataContext dataContext) {
    if (!CommonRefactoringUtil.checkReadOnlyStatus(file)) return;

    SelectionModel selectionModel = editor.getSelectionModel();
    if (selectionModel.hasSelection()) {
      Pair<PsiElement, PsiElement> pair = ErlangRefactoringUtil.selectionToElements(file, selectionModel);

      if (pair.first == null || pair.second == null) {
        showCannotPerformError(project, editor);
        return;
      }

      List<ErlangExpression> selection = getSelectedExpressions(pair.first, pair.second);

      if (selection.isEmpty()) {
        showCannotPerformError(project, editor);
      }
      else {
        perform(selection);
      }
    }
    else {
      ErlangRefactoringUtil.smartIntroduce(editor, file,
                                           new ErlangRefactoringUtil.Extractor() {
                                             @Override
                                             public boolean checkContext(@NotNull PsiFile file,
                                                                         @NotNull Editor editor,
                                                                         @Nullable PsiElement element) {
                                               boolean ok = PsiTreeUtil.getParentOfType(element, ErlangClauseBody.class) != null;
                                               if (!ok) {
                                                 showCannotPerformError(project, editor);
                                               }
                                               return ok;
                                             }

                                             @Override
                                             public void process(@NotNull Editor editor,
                                                                 @NotNull ErlangExpression expression) {
                                               perform(new SmartList<>(expression));
                                             }
                                           });
    }
    selectionModel.removeSelection();
  }

  @NotNull
  private static List<ErlangExpression> getSelectedExpressions(@NotNull PsiElement first, @NotNull PsiElement second) {
    if (second instanceof LeafPsiElement) {
      ErlangAtom atom = PsiTreeUtil.getParentOfType(second, ErlangAtom.class, true);
      if (atom != null) {
        second = atom;
      }
      else {
        IElementType elementType = ((LeafPsiElement) second).getElementType();
        if (elementType == ErlangTypes.ERL_DOT || elementType == ErlangTypes.ERL_SEMI) {
          ASTNode node = FormatterUtil.getPreviousNonWhitespaceSibling((LeafPsiElement) second);
          second = node == null ? second : PsiTreeUtil.getDeepestLast(node.getPsi());
        }
      }
    }
    PsiElement commonParent = PsiTreeUtil.findCommonParent(first, second);

    if (commonParent != null && commonParent.getParent() instanceof ErlangQAtom) {
      commonParent = commonParent.getParent().getParent();
    }

    if (commonParent instanceof ErlangExpression) {
      if (ErlangPsiImplUtil.inLeftPartOfAssignment(commonParent, false)) return Collections.emptyList();
      return new SmartList<>((ErlangExpression) commonParent);
    }

    PsiElement e = first;
    while (e != null && e.getParent() != commonParent) e = e.getParent();

    if (e == null) return Collections.emptyList();
    if (e.getTextRange().getStartOffset() < first.getTextRange().getStartOffset()) return Collections.emptyList();

    List<ErlangExpression> res = new ArrayList<>();
    for (PsiElement i = e; i != null && i.getTextOffset() <= second.getTextOffset(); i = i.getNextSibling()) {
      if (i instanceof ErlangExpression) res.add((ErlangExpression) i);
    }

    return res;
  }

  private static void perform(@NotNull List<ErlangExpression> selection) {
    final ErlangExpression first = ContainerUtil.getFirstItem(selection);
    final ErlangFunction function = PsiTreeUtil.getParentOfType(first, ErlangFunction.class);
    final ErlangExpression last = selection.get(selection.size() - 1);
    assert first != null;
    assert function != null;
    assert last != null;

    Pair<List<ErlangNamedElement>, List<ErlangNamedElement>> analyze = analyze(selection);

    final Project project = first.getProject();
    List<ErlangNamedElement> inParams = analyze.first;
    List<ErlangNamedElement> outParams = analyze.second;

    String shorten = ErlangRefactoringUtil.shorten(last, "extracted");
    String name = CaseFormat.UPPER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, shorten);
    ErlangExtractFunctionDialog dialog = new ErlangExtractFunctionDialog(project, name, inParams);

    if (ApplicationManager.getApplication().isUnitTestMode() || dialog.showAndGet()) {
      String functionName = dialog.getFunctionName();
      String bindings = bindings(outParams);
      String bindingsEx = StringUtil.isEmpty(bindings) ? bindings : ",\n" + bindings;
      final String bindingsS = StringUtil.isEmpty(bindings) ? bindings : bindings + " = ";

      final String signature = generateSignature(functionName, inParams);
      final String functionText = signature + " ->\n" +
                                  StringUtil.join(selection, PsiElement::getText, ",\n") + bindingsEx + ".";

      try {
        PsiFile file = first.getContainingFile();
        WriteCommandAction.writeCommandAction(project, file).withName("Extract Function").run(() -> {
          ErlangFunction newFunction = ErlangElementFactory.createFunctionFromText(project, functionText);

          PsiElement functionParent = function.getParent();
          PsiElement added = functionParent.addAfter(newFunction, function);
          functionParent.addBefore(ErlangElementFactory.createLeafFromText(project, "\n"), added);
          functionParent.addAfter(ErlangElementFactory.createLeafFromText(project, "\n"), function);

          PsiElement parent = first.getParent();
          parent.addBefore(ErlangElementFactory.createExpressionFromText(project, bindingsS + signature), first);
          parent.deleteChildRange(first, last);
        });
      }
      catch (Throwable throwable) {
        LOGGER.warn(throwable);
      }
    }

    Disposer.dispose(dialog.getDisposable());
  }

  @NotNull
  static String generateSignature(@NotNull String functionName, @NotNull List<ErlangNamedElement> inParams) {
    return functionName + "(" + StringUtil.join(inParams, o -> ObjectUtils.notNull(o.getName(), "_"), ", ") + ")";
  }

  @NotNull
  private static String bindings(@NotNull List<ErlangNamedElement> out) {
    if (out.isEmpty()) return "";
    String join = StringUtil.join(out, o -> ObjectUtils.notNull(o.getName(), "_"), ", ");
    if (out.size() == 1) return join;
    return "{" + join + "}";
  }


  private static Pair<List<ErlangNamedElement>, List<ErlangNamedElement>> analyze(@NotNull List<? extends PsiElement> elements) {
    PsiElement first = elements.getFirst();
    PsiElement scope = PsiTreeUtil.getTopmostParentOfType(first, ErlangFunction.class);
    PsiElement lastElement = elements.getLast();
    final int lastElementEndOffset = lastElement.getTextOffset() + lastElement.getTextLength();
    final int firstElementStartOffset = first.getTextOffset();

    // find out params
    assert scope != null;
    final LocalSearchScope localSearchScope = new LocalSearchScope(scope);
    List<ErlangNamedElement> outDeclarations = ContainerUtil.filter(
      getSimpleDeclarations(elements),
      componentName -> {
        for (PsiReference usage : ReferencesSearch.search(componentName, localSearchScope, false).findAll()) {
          if (usage.getElement().getTextOffset() > lastElementEndOffset) {
            return true;
          }
        }
        return false;
      });

    // find params
    RefVisitor visitor = new RefVisitor();
    for (PsiElement element : elements) {
      element.accept(visitor);
    }
    Collection<ErlangNamedElement> names = visitor.getComponentNames();
    List<ErlangNamedElement> inComponentNames = ContainerUtil.filter(names, componentName -> {
      int offset = componentName.getTextOffset();
      return offset >= lastElementEndOffset || offset < firstElementStartOffset;
    });
    return Pair.create(inComponentNames, outDeclarations);
  }

  @NotNull
  private static Set<ErlangNamedElement> getSimpleDeclarations(@NotNull List<? extends PsiElement> children) {
    final Set<ErlangNamedElement> result = new LinkedHashSet<>();
    for (PsiElement child : children) {
      child.accept(new ErlangRecursiveVisitor() {
        @Override
        public void visitQVar(@NotNull ErlangQVar o) {
          result.add(o);
        }
      });
    }
    return result;
  }

  private static class RefVisitor extends PsiRecursiveElementVisitor {
    private final LinkedHashSet<ErlangNamedElement> myComponentNames = new LinkedHashSet<>();

    @NotNull
    public Collection<ErlangNamedElement> getComponentNames() {
      return myComponentNames;
    }

    @Override
    public void visitElement(@NotNull PsiElement element) {
      if (element instanceof ErlangQVar) {
        PsiReference reference = element.getReference();
        PsiElement resolve = reference.resolve();
        if (resolve instanceof ErlangNamedElement) {
          myComponentNames.add((ErlangNamedElement) resolve);
        }
        else {
          myComponentNames.add((ErlangNamedElement) element);
        }
      }
      super.visitElement(element);
    }
  }

  private static void showCannotPerformError(@NotNull Project project, @NotNull Editor editor) {
    CommonRefactoringUtil.showErrorHint(project, editor, "Cannot perform refactoring", "Cannot Perform Refactoring", "refactoring.extractMethod");
  }

  @Override
  public void invoke(@NotNull Project project, @NotNull PsiElement @NotNull [] elements, DataContext dataContext) {
  }
}
