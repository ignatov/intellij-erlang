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

package org.intellij.erlang.refactor;

import com.intellij.codeInsight.PsiEquivalenceUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.SelectionModel;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.Pass;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.refactoring.IntroduceTargetChooser;
import com.intellij.util.Function;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ErlangRefactoringUtil {
  private ErlangRefactoringUtil() {
  }

  @NotNull
  public static List<PsiElement> getOccurrences(@NotNull final PsiElement pattern, @Nullable final PsiElement context) {
    if (context == null) {
      return Collections.emptyList();
    }
    final List<PsiElement> occurrences = new ArrayList<PsiElement>();
    final PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      public void visitElement(@NotNull final PsiElement element) {
        if (PsiEquivalenceUtil.areElementsEquivalent(element, pattern)) {
          occurrences.add(element);
          return;
        }
        super.visitElement(element);
      }
    };
    context.accept(visitor);
    return occurrences;
  }

  @NotNull
  public static Pair<PsiElement, PsiElement> selectionToElements(@NotNull PsiFile file, @NotNull SelectionModel selectionModel) {
    PsiElement element1 = file.findElementAt(selectionModel.getSelectionStart());
    PsiElement element2 = file.findElementAt(selectionModel.getSelectionEnd() - 1);
    if (element1 instanceof PsiWhiteSpace) {
      element1 = file.findElementAt(element1.getTextRange().getEndOffset());
    }
    if (element2 instanceof PsiWhiteSpace) {
      element2 = file.findElementAt(element2.getTextRange().getStartOffset() - 1);
    }
    return Pair.create(element1, element2);
  }

  public static String shorten(@NotNull ErlangExpression o, @NotNull String defaultValue) {
    VariableTextBuilder visitor = new VariableTextBuilder();
    o.accept(visitor);
    return visitor.result(defaultValue);
  }

  @NotNull
  public static String shorten(@NotNull ErlangExpression o) { // maybe better to return List<String>
    VariableTextBuilder visitor = new VariableTextBuilder();
    o.accept(visitor);
    return visitor.result();
  }

  public static void smartIntroduce(@NotNull final Editor editor, 
                                    @NotNull PsiFile file, 
                                    @NotNull final Extractor extractor) {
    int offset = editor.getCaretModel().getOffset();
    PsiElement element = file.findElementAt(offset);
    if (!extractor.checkContext(file, editor, element)) return;
    List<ErlangExpression> expressions = new ArrayList<ErlangExpression>();
    while (element != null) {
      if (element instanceof ErlangClauseBody) {
        break;
      }
      else if (element instanceof ErlangExpression && !(element instanceof ErlangAssignmentExpression)) {
        boolean isQualifiedFunCall = element instanceof ErlangFunctionCallExpression &&
                                     element.getParent() instanceof ErlangGlobalFunctionCallExpression;
        if (!isQualifiedFunCall && !ErlangPsiImplUtil.inLeftPartOfAssignment(element, false)) {
          expressions.add((ErlangExpression) element);
        }
      }
      element = element.getParent();
    }
    if (expressions.size() == 1 || ApplicationManager.getApplication().isUnitTestMode()) {
      extractor.process(editor, expressions.get(0));
    }
    else if (expressions.size() > 1) {
      IntroduceTargetChooser.showChooser(editor, expressions,
        new Pass<ErlangExpression>() {
          @Override
          public void pass(@NotNull ErlangExpression expression) {
            extractor.process(editor, expression);
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

  public interface Extractor {
    boolean checkContext(@NotNull PsiFile file, @NotNull Editor editor, @Nullable PsiElement element);
    void process(@NotNull Editor editor, @NotNull ErlangExpression expression);
  }
}
