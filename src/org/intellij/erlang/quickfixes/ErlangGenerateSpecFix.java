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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.ConstantNode;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;

public class ErlangGenerateSpecFix extends ErlangQuickFixBase {
  public static final String NAME = "Generate function spec";
  private static final String ANY_TYPE_STRING = "any()";

  @NotNull
  @Override
  public String getFamilyName() {
    return NAME;
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    var function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class, false);

    if (function != null) {
      var editor = FileEditorManager.getInstance(project).getSelectedTextEditor();

      if (editor != null) {
        generateSpec(editor, function);
      }
    }
  }

  private static @Nullable String getArgName(ErlangCompositeElement expression) {
    if (expression instanceof ErlangAssignmentExpression assignmentExpression) {
      var right = assignmentExpression.getRight();
      if (right instanceof ErlangMaxExpression) {
        expression = right;
      }
      else {
        var left = assignmentExpression.getLeft();
        if (left instanceof ErlangMaxExpression) expression = left;
      }
    }

    if (expression instanceof ErlangMaxExpression maxExpr) expression = maxExpr.getQVar();
    if (expression instanceof ErlangQVar qVar) return qVar.getName();

    if (expression instanceof ErlangRecordExpression) {
      var recordRef = ((ErlangRecordExpression) expression).getRecordRef();

      if (recordRef != null) {
        var RecordName = recordRef.getQAtom().getText();
        return RecordName.length() > 0
               ? RecordName.substring(0, 1).toUpperCase() + RecordName.substring(1)
               : null;
      }
    }
    return null;
  }

  private static List<ErlangExpression> getArgumentPatterns(ErlangFunction function, int argumentIdx) {
    var clauses = function.getFunctionClauseList();
    var argumentPatterns = new ArrayList<ErlangExpression>(clauses.size());

    for (var clause : clauses) {
      var argDefList = clause.getArgumentDefinitionList();
      var clauseArgs = argDefList.getArgumentDefinitionList();
      var argDef = argumentIdx < clauseArgs.size() ? clauseArgs.get(argumentIdx) : null;

      ContainerUtil.addIfNotNull(argumentPatterns, argDef != null ? argDef.getExpression() : null);
    }
    return argumentPatterns;
  }

  private static String computeArgumentDescriptionString(ErlangFunction function, int argumentIdx) {
    var argumentPatterns = getArgumentPatterns(function, argumentIdx);

    if (argumentPatterns.isEmpty()) return ANY_TYPE_STRING;

    var argTypes = new LinkedHashSet<String>();

    for (ErlangExpression expression : argumentPatterns) {
      if (expression instanceof ErlangAssignmentExpression assignmentExpr) {
        // Unwrap X = <something>, or <something> = X into X
        var left = assignmentExpr.getLeft();
        if (left instanceof ErlangMaxExpression) expression = left;
        var right = assignmentExpr.getRight();
        if (right instanceof ErlangMaxExpression) expression = right;
      }

      var erlangExpressionType = ErlangExpressionType.create(expression);
      argTypes.add(getTypeString(erlangExpressionType));
    }

    String typeString;

    if (!argTypes.contains(ANY_TYPE_STRING)) {
      typeString = StringUtil.join(argTypes, "|");
    }
    else {
      typeString = ANY_TYPE_STRING;
    }

    var argName = getArgName(argumentPatterns.get(0));

    return argName == null || argName.isEmpty()
           ? typeString
           : "%s::%s".formatted(argName, typeString);
  }

  /**
   * Add comma separated function args as template variables
   */
  private static void addArgsTypeSpecsAsTemplateFields(Template template, ErlangFunction function) {
    int arity = function.getArity();

    for (int i = 0; i < arity; i++) {
      if (i > 0) template.addTextSegment(", ");

      var argTypeText = computeArgumentDescriptionString(function, i);

      template.addVariable("ARG" + i, new ConstantNode(argTypeText), true);
    }
  }

  private static String getTypeString(ErlangExpressionType t) {
    return t == ErlangExpressionType.UNKNOWN
           ? ANY_TYPE_STRING
           : t.getName().toLowerCase() + "()";
  }

  private static ErlangExpressionType computeCommonType(List<ErlangExpression> expressions) {
    var types = ContainerUtil.map(expressions, ErlangExpressionType::create);

    //TODO compute common type
    return types.isEmpty()
           ? ErlangExpressionType.UNKNOWN
           : types.get(0);
  }

  private static ErlangExpressionType computeReturnType(ErlangFunction function) {
    var clauses = function.getFunctionClauseList();
    var lastExpressions = new ArrayList<ErlangExpression>(clauses.size());

    for (ErlangFunctionClause clause : clauses) {
      var clauseBody = clause.getClauseBody();

      if (clauseBody != null) {
        var lastExpressionInClause = ContainerUtil.getLastItem(clauseBody.getExpressionList());
        ContainerUtil.addIfNotNull(lastExpressions, lastExpressionInClause);
      }
    }
    return computeCommonType(lastExpressions);
  }

  private static Template createErlangSpecTemplate(ErlangFunction function, Project project) {
    var template = TemplateManager.getInstance(project).createTemplate("", "");
    template.setToReformat(true);

    template.addTextSegment("-spec " + function.getName() + "(");
    addArgsTypeSpecsAsTemplateFields(template, function);
    template.addTextSegment(") -> ");
    template.addVariable("RETURN", new ConstantNode(getTypeString(computeReturnType(function))), true);
    template.addTextSegment(".\n");

    template.addEndVariable();
    return template;
  }

  public static void generateSpec(@NotNull Editor editor, @NotNull ErlangFunction function) {
    var containingFile = function.getContainingFile();

    if (!(containingFile instanceof ErlangFile)) return;

    var project = Objects.requireNonNull(editor.getProject());
    var textOffset = function.getTextOffset();
    var template = createErlangSpecTemplate(function, project);

    editor.getCaretModel().moveToOffset(textOffset);
    TemplateManager.getInstance(project).startTemplate(editor, template);
  }
}
