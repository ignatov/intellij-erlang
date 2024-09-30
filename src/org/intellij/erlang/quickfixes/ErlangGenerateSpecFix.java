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
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.types.ErlType;
import org.intellij.erlang.types.ErlTypeUnion;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class ErlangGenerateSpecFix extends ErlangQuickFixBase {
  public static final String NAME = "Generate function spec";

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

  /**
   * Try and guess variable name used in the given expression, for spec argument naming purpose.
   *
   * @return The guess or null
   */
  private static @Nullable String guessArgumentName(ErlangCompositeElement expression) {
    var unwrapResult = PsiExprUtil.extractQVarFromAssignment((ErlangExpression) expression);

    if (unwrapResult.qVar != null) return unwrapResult.qVar.getName();

    if (unwrapResult.expression instanceof ErlangRecordExpression) {
      var recordRef = ((ErlangRecordExpression) expression).getRecordRef();

      if (recordRef != null) {
        var RecordName = recordRef.getQAtom().getText();
        return !RecordName.isEmpty()
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

    if (argumentPatterns.isEmpty()) return ErlType.ANY_TYPE.toString();

    var argType = new ErlTypeUnion(null);

    for (ErlangExpression expression : argumentPatterns) {
      var unwrapResult = PsiExprUtil.extractQVarFromAssignment(expression);
      var exprType = ErlType.fromExpression(unwrapResult.expression);

      argType.add(exprType);
    }

    String typeString = argType.toString();
    var argName = guessArgumentName(argumentPatterns.get(0));

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

  private static ErlType computeCommonType(List<ErlangExpression> expressions) {
    var types = expressions.stream().map(ErlType::fromExpression).toList();
    var unionType = new ErlTypeUnion(types);

    // Empty union by default converges to NONE_TYPE, but we want ANY_TYPE instead
    return unionType.isEmpty()
           ? ErlType.ANY_TYPE
           : types.get(0);
  }

  private static ErlType computeReturnType(ErlangFunction function) {
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
    template.addVariable("RETURN", new ConstantNode(computeReturnType(function).toString()), true);
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
