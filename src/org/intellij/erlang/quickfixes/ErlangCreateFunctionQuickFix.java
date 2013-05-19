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
import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.refactor.introduce.ErlangIntroduceVariableHandler;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangCreateFunctionQuickFix extends LocalQuickFixBase {
  private final String myName;
  private final int myArity;

  public ErlangCreateFunctionQuickFix(@NotNull String name, int arity) {
    super("Create Function '" + name + "/" + arity + "'", "Erlang");
    myName = name;
    myArity = arity;
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    //noinspection unchecked
    PsiElement call = PsiTreeUtil.getContextOfType(
      descriptor.getPsiElement(),
      false,
      ErlangFunctionCallExpression.class,
      ErlangSpecFun.class,
      ErlangFunctionWithArity.class,
      ErlangExportFunction.class
    );
    if (call != null) {
      //noinspection unchecked
      PsiElement topmost = PsiTreeUtil.getParentOfType(call,
        ErlangFunction.class, ErlangRecordDefinition.class, ErlangMacros.class, ErlangAttribute.class, ErlangModule.class);

      boolean fromExport = call instanceof ErlangExportFunction;
      if (fromExport) {
        topmost = call.getContainingFile().getLastChild();
      }

      if (topmost != null) {
        Editor editor = PsiUtilBase.findEditor(topmost);
        if (editor == null) return;
        List<String> placeHolders = new ArrayList<String>(myArity);
        for (int i = 0; i < myArity; i++) placeHolders.add("_Arg" + i);
        if (call instanceof ErlangFunctionCallExpression) {
          List<ErlangExpression> exrList = ((ErlangFunctionCallExpression) call).getArgumentList().getExpressionList();
          placeHolders = ContainerUtil.map(exrList, new Function<ErlangExpression, String>() {
            @Override
            public String fun(ErlangExpression erlangExpression) {
              return shorten(erlangExpression);
            }
          });
        }

        boolean addBelow = topmost instanceof ErlangAttribute || fromExport;

        TemplateManager templateManager = TemplateManager.getInstance(project);
        Template template = templateManager.createTemplate("", "");
        template.setToReformat(true);
        template.addTextSegment(addBelow ? "\n" + (fromExport ? "\n" : "") : "");
        template.addTextSegment(myName + "(");
        int size = placeHolders.size();
        for (int i = 0; i < placeHolders.size(); i++) {
          String name = placeHolders.get(i);
          template.addVariable("param" + i, new ConstantNode(name), true);
          if (i != size - 1) template.addTextSegment(", ");
        }
        template.addTextSegment(") ->\n");
        template.addEndVariable();
        template.addTextSegment("erlang:error(not_implemented)");
        template.addTextSegment("." + (fromExport ? "" : "\n") + (addBelow ? "" : "\n"));

        editor.getCaretModel().moveToOffset(addBelow ?
          topmost.getTextRange().getEndOffset() :
          topmost.getTextOffset());
        templateManager.startTemplate(editor, template);
      }
    }
  }

  private static String shorten(ErlangExpression o) { // maybe better to return List<String>
    ErlangIntroduceVariableHandler.VariableTextBuilder visitor = new ErlangIntroduceVariableHandler.VariableTextBuilder();
    o.accept(visitor);
    return visitor.result();
  }
}
