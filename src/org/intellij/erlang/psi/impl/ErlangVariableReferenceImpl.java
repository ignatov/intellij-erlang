package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

/**
 * @author ignatov
 */
public class ErlangVariableReferenceImpl extends PsiReferenceBase<ErlangQVar> {
  public ErlangVariableReferenceImpl(@NotNull ErlangQVar element, TextRange range) {
    super(element, range);
  }

  @Override
  public PsiElement resolve() {
    ErlangVarProcessor processor = new ErlangVarProcessor(myElement.getText(), myElement);
    ErlangListComprehension lc = PsiTreeUtil.getParentOfType(myElement, ErlangListComprehension.class);
    ErlangCompositeElement place = lc != null ? lc.getLcExprs() : myElement;
    ResolveUtil.treeWalkUp(place, processor);
    ErlangQVar result = processor.getResult();
    if (result != null) return result;

    ErlangModule module = getModule(myElement.getContainingFile());
    if (module == null) return null;
    module.processDeclarations(processor, ResolveState.initial(), module, module);

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
