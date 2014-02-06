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
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.refactor.VariableTextBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

//TODO add macro arguments count selection (it should be either -1 or same as arguments count on the call site)
public class ErlangIntroduceMacroQuickFix extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Introduce macro";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    PsiElement psiElement = getProblemElementMacroAware(descriptor);
    if (!(psiElement instanceof ErlangMacros)) return;
    PsiElement file = psiElement.getContainingFile();
    if (file instanceof ErlangFile) {
      insertMacroDefinition(project, (ErlangMacros) psiElement, (ErlangFile) file);
    }
  }

  private static void insertMacroDefinition(Project project, ErlangMacros macro, ErlangFile file) {
    Editor editor = PsiUtilBase.findEditor(file);
    String macroName = getMacroName(macro);

    if (editor == null || macroName == null) return;

    int offset = getFirstMacroDefinitionOffset(file);
    int topLevelOffset = getTopLevelElementOffset(macro, file);

    if (offset == -1 || topLevelOffset < offset) {
      offset = topLevelOffset;
      editor.getDocument().insertString(offset, "\n");
    }

    Template template = createMacroDefinitionTemplate(project, macroName, macro.getMacroCallArgumentList());
    editor.getCaretModel().moveToOffset(offset);
    TemplateManager.getInstance(project).startTemplate(editor, template);
  }

  private static int getTopLevelElementOffset(ErlangMacros macro, ErlangFile file) {
    PsiElement element = macro;

    while (element != null && element.getParent() != file) {
      element = element.getParent();
    }

    return element == null ? 0 : element.getTextRange().getStartOffset();
  }

  @Nullable
  private static String getMacroName(ErlangMacros macro) {
    ErlangMacrosName macroNamePsi = macro.getMacrosName();
    return macroNamePsi != null ? ErlangPsiImplUtil.getMacroName(macroNamePsi) : null;
  }

  private static int getFirstMacroDefinitionOffset(ErlangFile file) {
    PsiElement elementBefore = PsiTreeUtil.getChildOfType(file, ErlangMacrosDefinition.class);

    return null == elementBefore ? -1 : elementBefore.getTextRange().getStartOffset();
  }

  private static Template createMacroDefinitionTemplate(Project project, String macroName, @Nullable ErlangMacroCallArgumentList argumentList) {
    TemplateManager templateManager = TemplateManager.getInstance(project);
    Template template = templateManager.createTemplate("", "");
    template.setToReformat(true);
    template.addTextSegment("-define(" + macroName);
    if (argumentList != null) {
      addMacroArgumentsToTemplate(template, argumentList.getArgumentList().getExpressionList());
    }
    template.addTextSegment(", ");
    template.addVariable("macro_body", new ConstantNode("macro_body"), true);
    template.addTextSegment(").");
    template.addEndVariable();
    template.addTextSegment("\n");
    return template;
  }

  private static void addMacroArgumentsToTemplate(Template template, List<ErlangExpression> args) {
    template.addTextSegment("(");
    if (!args.isEmpty()) {
      int argumentNumber = 0;
      while (true) {
        addMacroArgumentToTemplate(template, args.get(argumentNumber), argumentNumber);
        argumentNumber++;
        if (argumentNumber == args.size()) break;
        template.addTextSegment(", ");
      }
    }
    template.addTextSegment(")");
  }

  private static void addMacroArgumentToTemplate(Template template, ErlangExpression argument, int argumentNumber) {
    VariableTextBuilder varNameBuilder = new VariableTextBuilder();
    varNameBuilder.visitElement(argument);
    String variableName = varNameBuilder.result();
    template.addVariable("arg_" + argumentNumber, new ConstantNode(variableName), true);
  }
}
