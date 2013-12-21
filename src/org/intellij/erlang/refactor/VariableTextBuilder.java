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

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiRecursiveElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class VariableTextBuilder extends PsiRecursiveElementVisitor {
  private StringBuilder myResult = new StringBuilder();

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
      myResult.append(StringUtil.capitalize(element.getText()));
      return;
    }
    else if (element instanceof ErlangFunctionCallExpression) {
      myResult.append(((ErlangFunctionCallExpression) element).getName());
      return;
    }
    else if (element instanceof ErlangCaseExpression) {
      ErlangExpression expression = ((ErlangCaseExpression) element).getExpression();
      myResult.append("Case");
      if (expression != null) {
        VariableTextBuilder b = new VariableTextBuilder();
        expression.accept(b);
        myResult.append(b.result());
      }
      return;
    }
    else if (element instanceof ErlangFunExpression) {
      myResult.append("Fun");
      ErlangFunClauses clauses = ((ErlangFunExpression) element).getFunClauses();
      List<ErlangFunClause> funClauses = clauses != null ? clauses.getFunClauseList() : ContainerUtil.<ErlangFunClause>emptyList();
      ErlangFunClause firstItem = ContainerUtil.getFirstItem(funClauses);
      if (firstItem != null) {
        VariableTextBuilder b = new VariableTextBuilder();
        firstItem.getArgumentDefinitionList().accept(b);
        myResult.append(b.result());
      }
      return;
    }
    else if (element instanceof ErlangListComprehension || element instanceof ErlangListExpression) {
      myResult.append("List");
      return;
    }
    else if (element instanceof ErlangTupleExpression || element instanceof ErlangRecordTuple) {
      myResult.append("Tuple");
      return;
    }
    else if (element instanceof ErlangMaxExpression) {
      if (((ErlangMaxExpression) element).getInteger() != null) {
        myResult.append("N");
      }
      if (((ErlangMaxExpression) element).getFloat() != null) {
        myResult.append("F");
      }
    }
    if (element instanceof ErlangStringLiteral) {
      myResult.append("Str");
    }
    super.visitElement(element);
  }

  @NotNull
  public String result() {
    return result("PlaceHolder");
  }

  @NotNull
  public String result(String defaultValue) {
    String s = StringUtil.toTitleCase(myResult.toString())
      .replaceAll("_", "")
      .replaceAll("\\?", "")
      .replaceAll("'", "")
      .replaceAll(" ", "");
    return StringUtil.notNullize(s, defaultValue);
  }
}
