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

import com.intellij.codeInsight.template.TemplateBuilderFactory;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class ErlangGenerateSpecFix extends ErlangQuickFixBase {
  public static final String NAME = "Generate spec";
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

  public static void generateSpec(@NotNull Editor editor, @NotNull ErlangFunction function) {
    var containingFile = function.getContainingFile();

    if (!(containingFile instanceof ErlangFile file)) return;

    var project = function.getProject();
    var spec = createSpecTemplate(function, file, project);
    runSpecTemplateEditor(editor, spec);
  }

  private static void runSpecTemplateEditor(final Editor editor, final ErlangAttribute attr) {
    if (ApplicationManager.getApplication().isUnitTestMode()) return;

    ApplicationManager.getApplication().invokeLater(
      () -> CommandProcessor.getInstance().executeCommand(
        editor.getProject(),
        () -> ApplicationManager.getApplication().runWriteAction(() -> {
          var templateBuilder = TemplateBuilderFactory.getInstance().createTemplateBuilder(attr);
          var funType = PsiTreeUtil.findChildOfType(attr, ErlangFunType.class);
          var types = PsiTreeUtil.findChildrenOfType(funType, ErlangType.class);

          for (ErlangType type : types) {
            templateBuilder.replaceElement(type, type.getText());
          }

          templateBuilder.run(editor, false);
        }),
        "Generate Spec Template Editor",
        null));
  }

  private static ErlangAttribute createSpecTemplate(ErlangFunction function, ErlangFile file, Project project) {
    var text = MessageFormat.format("{0}({1}) -> {2}.",
                                    function.getName(),
                                    computeArgsTypeSpecs(function),
                                    getTypeString(computeReturnType(function)));
    var spec = file.addBefore(ErlangElementFactory.createSpecFromText(project, text), function);

    file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n"), function);
    return (ErlangAttribute) spec;
  }

  private static String computeArgsTypeSpecs(ErlangFunction function) {
    var argTypes = new StringBuilder();
    int arity = function.getArity();

    for (int i = 0; i < arity; i++) {
      argTypes.append(computeArgumentDescriptionString(function, i)).append(", ");
    }

    if (arity != 0) {
      argTypes.setLength(argTypes.length() - 2);
    }

    return argTypes.toString();
  }

  private static String computeArgumentDescriptionString(ErlangFunction function, int argumentIdx) {
    var argumentPatterns = getArgumentPatterns(function, argumentIdx);

    if (argumentPatterns.isEmpty()) return ANY_TYPE_STRING;

    var argTypes = new LinkedHashSet<String>();

    for (ErlangExpression expression : argumentPatterns) {
      if (expression instanceof ErlangAssignmentExpression assignmentExpression) {
        expression = assignmentExpression.getLeft();
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

    return argName != null ? argName + "::" + typeString : typeString;
  }

  private static String getTypeString(ErlangExpressionType t) {
    return t == ErlangExpressionType.UNKNOWN
           ? ANY_TYPE_STRING
           : t.getName().toLowerCase() + "()";
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

  private static ErlangExpressionType computeCommonType(List<ErlangExpression> expressions) {
    var types = ContainerUtil.map(expressions, ErlangExpressionType::create);

    //TODO compute common type
    return types.isEmpty()
           ? ErlangExpressionType.UNKNOWN
           : types.get(0);
  }

  private static @Nullable String getArgName(ErlangExpression expression) {
    if (expression instanceof ErlangMaxExpression) return expression.getText();

    if (expression instanceof ErlangAssignmentExpression) {
      var right = ((ErlangAssignmentExpression) expression).getRight();
      return right != null ? right.getText() : "";
    }

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

}
