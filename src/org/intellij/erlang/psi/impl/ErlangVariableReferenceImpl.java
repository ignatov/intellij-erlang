package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.getModule;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.inDefinition;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.isLeftPartOfAssignment;

/**
 * @author ignatov
 */
public class ErlangVariableReferenceImpl extends PsiReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  @Override
  public PsiElement resolve() {
//    if (PsiTreeUtil.getParentOfType(myElement, ErlangArgumentDefinition.class) != null) return null;
    ErlangVarProcessor processor = new ErlangVarProcessor(myElement.getText(), myElement);
    ErlangListComprehension lc = PsiTreeUtil.getParentOfType(myElement, ErlangListComprehension.class);
    ErlangCompositeElement place = lc != null ? lc.getLcExprs() : myElement;
    ResolveUtil.treeWalkUp(place, processor);
    ErlangQVar processorResult = processor.getResult();
    if (processorResult != null) return processorResult;

    ErlangModule module = getModule(myElement.getContainingFile());
    if (module == null) return null;
    module.processDeclarations(processor, ResolveState.initial(), module, module);
//    ResolveUtil.treeWalkUp(module.getArgumentDefinition(), processor);

    return processor.getResult();
  }


  @NotNull
  @Override
  public Object[] getVariants() {
    if (PsiTreeUtil.getParentOfType(myElement, ErlangArgumentDefinition.class) != null) return new Object[]{};

    final List<LookupElement> result = new ArrayList<LookupElement>();

    final ErlangFunctionClause clause = PsiTreeUtil.getParentOfType(myElement, ErlangFunctionClause.class);
    BaseScopeProcessor processor = new BaseScopeProcessor() {
      @Override
      public boolean execute(@NotNull PsiElement psiElement, ResolveState resolveState) {
        if (!psiElement.equals(myElement) && psiElement instanceof ErlangQVar && !psiElement.getText().equals("_")) {
          if (PsiTreeUtil.isAncestor(clause, psiElement, false) && (inDefinition(psiElement) || isLeftPartOfAssignment(psiElement))) {
            result.add(LookupElementBuilder.create((PsiNamedElement) psiElement).setIcon(ErlangIcons.VARIABLE));
          }
        }
        return true;
      }
    };
    ResolveUtil.treeWalkUp(myElement, processor);

    // todo: support for functions completion when item under caret is empty
    result.addAll(ErlangPsiImplUtil.getFunctionLookupElements(myElement.getContainingFile(), false));

    return ArrayUtil.toObjectArray(result);
  }

  @Override
  public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
    myElement.replace(ErlangElementFactory.createQVarFromText(myElement.getProject(), newElementName));
    return myElement;
  }
}
