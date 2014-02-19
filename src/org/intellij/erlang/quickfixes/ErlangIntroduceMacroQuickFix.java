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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.ListPopup;
import com.intellij.openapi.ui.popup.ListPopupStep;
import com.intellij.openapi.ui.popup.PopupStep;
import com.intellij.openapi.ui.popup.util.BaseListPopupStep;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.refactoring.VariableTextBuilder;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

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
    insertMacroDefinition(project, (ErlangMacros) psiElement);
  }

  private static void insertMacroDefinition(@NotNull Project project, @NotNull ErlangMacros macro) {
    ErlangMacroCallArgumentList macroCallArgumentList = macro.getMacroCallArgumentList();
    if (macroCallArgumentList == null || ApplicationManager.getApplication().isUnitTestMode()) {
      doInsertMacroDefinition(project, macro, macroCallArgumentList);
    }
    else {
      showAritySelectionPopup(project, macro, macroCallArgumentList);
    }
  }

  private static void showAritySelectionPopup(@NotNull Project project, @NotNull ErlangMacros macro,
                                              @NotNull ErlangMacroCallArgumentList macroCallArgumentList) {
    ListPopupStep<String> popupStep = createAritySelectionPopupStep(project, macro, macroCallArgumentList);
    Editor editor = PsiUtilBase.findEditor(macro);
    if (popupStep == null || editor == null) return;
    ListPopup listPopup = JBPopupFactory.getInstance().createListPopup(popupStep);
    listPopup.showInBestPositionFor(editor);
  }

  @Nullable
  private static ListPopupStep<String> createAritySelectionPopupStep(@NotNull final Project project, @NotNull final ErlangMacros macro,
                                                                     @NotNull final ErlangMacroCallArgumentList macroCallArgumentList) {
    String macroName = getMacroName(macro);
    if (macroName == null) return null;

    final String callSiteArityMacro = "?" + macroName + "/" + macroCallArgumentList.getArgumentList().getExpressionList().size();
    final String noArityMacro = "?" + macroName;
    return new BaseListPopupStep<String>("Which macro to introduce?",
      new String[]{callSiteArityMacro, noArityMacro}) {
      @Override
      public PopupStep onChosen(String selectedValue, boolean finalChoice) {
        if (!finalChoice) return this;
        if (callSiteArityMacro == selectedValue) {
          doInsertMacroDefinitionInWriteAction(project, macro, macroCallArgumentList);
        }
        else if (noArityMacro == selectedValue) {
          doInsertMacroDefinitionInWriteAction(project, macro, null);
        }
        return FINAL_CHOICE;
      }
    };
  }

  private static void doInsertMacroDefinitionInWriteAction(@NotNull final Project project, @NotNull final ErlangMacros macro,
                                                           @Nullable final ErlangMacroCallArgumentList macroCallArgumentList) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        doInsertMacroDefinition(project, macro, macroCallArgumentList);
      }
    });
  }

  private static void doInsertMacroDefinition(@NotNull Project project, @NotNull ErlangMacros macro,
                                              @Nullable ErlangMacroCallArgumentList macroCallArgumentList) {
    PsiFile containingFile = macro.getContainingFile();
    Editor editor = containingFile != null ? PsiUtilBase.findEditor(containingFile) : null;
    ErlangFile file = containingFile instanceof ErlangFile ? (ErlangFile) containingFile : null;
    String macroName = getMacroName(macro);
    if (editor == null || file == null || macroName == null) return;

    int offset = getFirstMacroDefinitionOffset(file);
    int topLevelOffset = getTopLevelElementOffset(macro, file);
    if (offset == -1 || topLevelOffset < offset) {
      offset = topLevelOffset;
      editor.getDocument().insertString(offset, "\n");
    }

    Template template = createMacroDefinitionTemplate(project, macroName, macroCallArgumentList);
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
