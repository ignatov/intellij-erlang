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

package org.intellij.erlang.inspection;

import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.ConstantNode;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.LocalQuickFixBase;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangFunctionReferenceImpl;
import org.intellij.erlang.refactor.introduce.ErlangIntroduceVariableHandler;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangUnresolvedFunctionInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReferenceImpl && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;

          String name = r.getName();
          int arity = r.getArity();

          if (ErlangBifTable.isBif("erlang", name, arity)) return;

          String signature = r.getSignature();

          PsiElement parent = o.getParent();
          if (parent instanceof ErlangGlobalFunctionCallExpression) {
            ErlangModuleRef moduleRef = ((ErlangGlobalFunctionCallExpression) parent).getModuleRef();
            if (moduleRef != null) {
              if (moduleRef.getQAtom().getMacros() != null) return;
              String moduleName = moduleRef.getText();
              if (ErlangBifTable.isBif(moduleName, name, arity)) return;
              signature = moduleName + ":" + signature;
            }
          }

          LocalQuickFix[] qfs = parent instanceof ErlangGenericFunctionCallExpression || parent instanceof ErlangGlobalFunctionCallExpression ?
            new LocalQuickFix[]{} :
            new LocalQuickFix[]{new ErlangCreateFunctionQuickFixBase(name, arity)};

          problemsHolder.registerProblem(o.getNameIdentifier(), "Unresolved function " + "'" + signature + "'", qfs);
        }
      }

      @Override
      public void visitSpecFun(@NotNull ErlangSpecFun o) {
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReferenceImpl && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;

          LocalQuickFix[] qfs = PsiTreeUtil.getNextSiblingOfType(o, ErlangModuleRef.class) != null ?
            new LocalQuickFix[]{} :
            new LocalQuickFix[]{new ErlangCreateFunctionQuickFixBase(r.getName(), r.getArity())};

          problemsHolder.registerProblem(o.getQAtom(), "Unresolved function " + "'" + r.getSignature() + "'", qfs);
        }
      }

      @Override
      public void visitFunctionWithArity(@NotNull ErlangFunctionWithArity o) {
        PsiReference reference = o.getReference();
        if (reference instanceof ErlangFunctionReferenceImpl && reference.resolve() == null) {
          if (o.getQAtom().getMacros() != null) return;
          ErlangFunctionReferenceImpl r = (ErlangFunctionReferenceImpl) reference;

          LocalQuickFix[] qfs = PsiTreeUtil.getNextSiblingOfType(o, ErlangModuleRef.class) != null ?
            new LocalQuickFix[]{} :
            new LocalQuickFix[]{new ErlangCreateFunctionQuickFixBase(r.getName(), r.getArity())};

          problemsHolder.registerProblem(o.getQAtom(), "Unresolved function " + "'" + r.getSignature() + "'", qfs);
        }
      }
    });
  }

  private static class ErlangCreateFunctionQuickFixBase extends LocalQuickFixBase {
    private final String myName;
    private final int myArity;

    protected ErlangCreateFunctionQuickFixBase(@NotNull String name, int arity) {
      super("Create Function '" + name + "/" + arity + "'", "Erlang");
      myName = name;
      myArity = arity;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      //noinspection unchecked
      ErlangCompositeElement call = PsiTreeUtil.getParentOfType(
        descriptor.getPsiElement(),
        ErlangFunctionCallExpression.class,
        ErlangSpecFun.class,
        ErlangFunctionWithArity.class

      );
      if (call != null) {
        //noinspection unchecked
        ErlangCompositeElement topmost = PsiTreeUtil.getParentOfType(call,
          ErlangFunction.class, ErlangRecordDefinition.class, ErlangMacros.class, ErlangAttribute.class);
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

          boolean addBelow = topmost instanceof ErlangAttribute;

          TemplateManager templateManager = TemplateManager.getInstance(project);
          Template template = templateManager.createTemplate("", "");
          template.setToReformat(true);
          template.addTextSegment(addBelow ? "\n" : "");
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
          template.addTextSegment(".\n" + (addBelow ? "" : "\n"));

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
}
