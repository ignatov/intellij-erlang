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
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacros;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangIntroduceMacroQuickFix extends ErlangQuickFixBase {

  @NotNull
  @Override
  public String getFamilyName() {
    return "Introduce macro";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    PsiElement psiElement = descriptor.getPsiElement();

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

    Template template = createMacroDefinitionTemplate(project, macroName);
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
    return macroNamePsi != null ? macroNamePsi.getText() : null;
  }

  private static int getFirstMacroDefinitionOffset(ErlangFile file) {
    PsiElement elementBefore = PsiTreeUtil.getChildOfType(file, ErlangMacrosDefinition.class);

    return null == elementBefore ? -1 : elementBefore.getTextRange().getStartOffset();
  }

  private static Template createMacroDefinitionTemplate(Project project, String macroName) {
    TemplateManager templateManager = TemplateManager.getInstance(project);

    Template template = templateManager.createTemplate("", "");

    template.setToReformat(true);
    template.addTextSegment("-define(" + macroName + ", ");
    template.addVariable("macro_body", new ConstantNode("macro_body"), true);
    template.addTextSegment(").");
    template.addEndVariable();
    template.addTextSegment("\n");

    return template;
  }
}
