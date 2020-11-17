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

import com.intellij.codeInsight.template.TemplateBuilder;
import com.intellij.codeInsight.template.TemplateBuilderFactory;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;

import java.util.*;

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
    ErlangFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class, false);
    if (function != null) {
      Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
      if (editor != null) {
        generateSpec(editor, function);
      }
    }
  }

  public static void generateSpec(@NotNull Editor editor, @NotNull ErlangFunction function) {
    PsiFile containingFile = function.getContainingFile();
    if (!(containingFile instanceof ErlangFile)) return;
    ErlangFile file = (ErlangFile) containingFile;
    Project project = function.getProject();
    ErlangAttribute spec = createSpecTemplate(function, file, project);
    runSpecTemplateEditor(editor, spec);
  }

  private static void runSpecTemplateEditor(final Editor editor, final ErlangAttribute attr) {
    if (ApplicationManager.getApplication().isUnitTestMode()) return;
    ApplicationManager.getApplication().invokeLater(() -> CommandProcessor.getInstance().executeCommand(editor.getProject(), () -> ApplicationManager.getApplication().runWriteAction(() -> {
      TemplateBuilder templateBuilder = TemplateBuilderFactory.getInstance().createTemplateBuilder(attr);
      ErlangFunType funType = PsiTreeUtil.findChildOfType(attr, ErlangFunType.class);
      Collection<ErlangType> types = PsiTreeUtil.findChildrenOfType(funType, ErlangType.class);
      for (ErlangType type : types) {
        templateBuilder.replaceElement(type, type.getText());
      }
      templateBuilder.run(editor, false);
    }), "Generate spec template editor", null));
  }

  private static ErlangAttribute createSpecTemplate(ErlangFunction function, ErlangFile file, Project project) {
    String text = function.getName() + '(' + computeArgsTypeSpecs(function) + ')' + " -> " + getTypeString(computeReturnType(function)) + '.';
    PsiElement spec = file.addBefore(ErlangElementFactory.createSpecFromText(project, text), function);
    file.addBefore(ErlangElementFactory.createLeafFromText(project, "\n"), function);
    return (ErlangAttribute) spec;
  }

  private static String computeArgsTypeSpecs(ErlangFunction function) {
    StringBuilder argTypes = new StringBuilder();
    int arity = function.getArity();
    for (int i = 0; i < arity; i++) {
      argTypes.append(computeArgDesString(function, i)).append(", ");
    }
    if (arity != 0) {
      argTypes.setLength(argTypes.length() - 2);
    }
    return argTypes.toString();
  }

  private static String computeArgDesString(ErlangFunction function, int argumentIdx) {
    List<ErlangExpression> argumentPatterns = getArgumentPatterns(function, argumentIdx);
    if (argumentPatterns.isEmpty()) return ANY_TYPE_STRING;
    HashSet<String> argTypes = new LinkedHashSet<>();
    for (ErlangExpression expression:argumentPatterns){
      if (expression instanceof ErlangAssignmentExpression)
        expression = ((ErlangAssignmentExpression) expression).getLeft();
      ErlangExpressionType erlangExpressionType = ErlangExpressionType.create(expression);
      argTypes.add(getTypeString(erlangExpressionType));
    }
    String typeString;
    if (!argTypes.contains(ANY_TYPE_STRING))
      typeString = StringUtil.join(argTypes, "|");
    else
      typeString = ANY_TYPE_STRING;
    String argName = getArgName(argumentPatterns.get(0));
    return argName != null ? argName + "::" + typeString : typeString;
  }

  public static String getTypeString(ErlangExpressionType t) {
    return t == ErlangExpressionType.UNKNOWN ? ANY_TYPE_STRING : t.getName().toLowerCase() + "()";
  }

  public static ErlangExpressionType computeReturnType(ErlangFunction function) {
    List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
    List<ErlangExpression> lastExpressions = new ArrayList<>(clauses.size());
    for (ErlangFunctionClause clause : clauses) {
      ErlangClauseBody clauseBody = clause.getClauseBody();
      if (clauseBody != null) {
        ErlangExpression lastExpressionInClause = ContainerUtil.getLastItem(clauseBody.getExpressionList());
        ContainerUtil.addIfNotNull(lastExpressions, lastExpressionInClause);
      }
    }
    return computeCommonType(lastExpressions);
  }


  private static List<ErlangExpression> getArgumentPatterns(ErlangFunction function, int argumentIdx) {
    List<ErlangFunctionClause> clauses = function.getFunctionClauseList();
    List<ErlangExpression> argumentPatterns = new ArrayList<>(clauses.size());
    for (ErlangFunctionClause clause : clauses) {
      ErlangArgumentDefinitionList argDefList = clause.getArgumentDefinitionList();
      List<ErlangArgumentDefinition> clauseArgs = argDefList.getArgumentDefinitionList();
      ErlangArgumentDefinition argDef = argumentIdx < clauseArgs.size() ? clauseArgs.get(argumentIdx) : null;
      ContainerUtil.addIfNotNull(argumentPatterns, argDef != null ? argDef.getExpression() : null);
    }
    return argumentPatterns;
  }

  private static ErlangExpressionType computeCommonType(List<ErlangExpression> expressions) {
    List<ErlangExpressionType> types = ContainerUtil.map(expressions, ErlangExpressionType::create);
    //TODO compute common type
    return types.isEmpty() ? ErlangExpressionType.UNKNOWN : types.get(0);
  }

  private static String getArgName(ErlangExpression expression) {
    if (expression instanceof ErlangMaxExpression) return expression.getText();
    if (expression instanceof ErlangAssignmentExpression){
      ErlangExpression right = ((ErlangAssignmentExpression) expression).getRight();
      return right != null ? right.getText() : "";
    }
    if (expression instanceof ErlangRecordExpression){
      ErlangRecordRef recordRef = ((ErlangRecordExpression) expression).getRecordRef();
      if (recordRef != null){
        String RecordName = recordRef.getQAtom().getText();
        return RecordName.length() > 0 ? RecordName.substring(0, 1).toUpperCase() + RecordName.substring(1) : null;
      }
    }
    return null;
  }

}
