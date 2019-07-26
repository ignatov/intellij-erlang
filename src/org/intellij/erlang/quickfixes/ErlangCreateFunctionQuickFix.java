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

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.ConstantNode;
import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangCreateFunctionQuickFix extends LocalQuickFixBase implements IntentionAction {
  public static final String FUNCTION_BODY_DEFAULT_TEXT = "erlang:error(not_implemented).";
  private final FunctionTextProvider myFunctionText;

  public ErlangCreateFunctionQuickFix(@NotNull String fixMessage, @NotNull String name, int arity) {
    this(fixMessage, new DefaultFunctionTextProvider(name, arity));
  }

  public ErlangCreateFunctionQuickFix(@NotNull String fixMessage, @NotNull FunctionTextProvider provider) {
    super(fixMessage, "Erlang");
    myFunctionText = provider;
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
  }

  @Nls
  @NotNull
  @Override
  public String getText() {
    return super.getName();
  }

  @Override
  public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file) {
    return true;
  }

  @Override
  public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
    int textOffset = file.getLastChild().getTextRange().getEndOffset();
    TemplateManager templateManager = TemplateManager.getInstance(project);
    Template template = templateManager.createTemplate("", "");
    template.setToReformat(true);
    template.addTextSegment("\n\n");
    template.addTextSegment(myFunctionText.getName() + "(");

    List<String> arguments = myFunctionText.getArguments();
    int arity = arguments.size();
    for (int i = 0; i < arity; i++) {
      String name = arguments.get(i);
      template.addVariable("param" + i, new ConstantNode(name), true);
      if (i != arity - 1) template.addTextSegment(", ");
    }
    template.addTextSegment(") ->\n");
    template.addEndVariable();
    template.addTextSegment(FUNCTION_BODY_DEFAULT_TEXT);

    editor.getCaretModel().moveToOffset(textOffset);
    templateManager.startTemplate(editor, template);
  }

  @Override
  public boolean startInWriteAction() {
    return true;
  }

  public interface FunctionTextProvider {
    @NotNull
    String getName();

    @NotNull
    List<String> getArguments();
  }

  private static class DefaultFunctionTextProvider implements FunctionTextProvider {
    private final String myName;
    private final int myArity;

    public DefaultFunctionTextProvider(@NotNull String name, int arity) {
      myName = name;
      assert arity >= 0;
      myArity = arity;
    }

    @NotNull
    public String getName() {
      return myName;
    }

    @NotNull
    public List<String> getArguments() {
      List<String> arguments = new ArrayList<>(myArity);
      for (int i = 0; i < myArity; i++) {
        arguments.add("_Arg" + i);
      }
      return arguments;
    }
  }
}