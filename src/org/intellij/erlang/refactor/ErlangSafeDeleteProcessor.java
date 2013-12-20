package org.intellij.erlang.refactor;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.safeDelete.NonCodeUsageSearchInfo;
import com.intellij.refactoring.safeDelete.SafeDeleteProcessorDelegateBase;
import com.intellij.refactoring.safeDelete.usageInfo.SafeDeleteReferenceSimpleDeleteUsageInfo;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangExportFunction;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangSpecification;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

public class ErlangSafeDeleteProcessor extends SafeDeleteProcessorDelegateBase {
  @Nullable
  @Override
  public Collection<? extends PsiElement> getElementsToSearch(PsiElement element, @Nullable Module module, Collection<PsiElement> allElementsToDelete) {
    return allElementsToDelete;
  }

  @Override
  public boolean handlesElement(PsiElement element) {
    return element instanceof ErlangFunction;
  }

  @Nullable
  @Override
  public NonCodeUsageSearchInfo findUsages(PsiElement element, PsiElement[] allElementsToDelete, List<UsageInfo> result) {
    if (element instanceof ErlangFunction) {
      Collection<PsiReference> all = ReferencesSearch.search(element).findAll();
      for (PsiReference ref : all) {
        PsiElement refElement = ref.getElement();
        if (PsiTreeUtil.getParentOfType(refElement, ErlangSpecification.class) != null) continue;
        final boolean inExport = PsiTreeUtil.getParentOfType(refElement, ErlangExportFunction.class) != null;
        result.add(new SafeDeleteReferenceSimpleDeleteUsageInfo(refElement, element, inExport) {
          @Override
          public void deleteElement() throws IncorrectOperationException {
            PsiElement e = getElement();
            if (isSafeDelete() && inExport && e != null) {
              PsiElement parent = e.getParent();
              PsiElement prev = PsiTreeUtil.prevVisibleLeaf(parent);
              if (isComma(prev)) {
                prev.delete();
              }
              else {
                PsiElement next = PsiTreeUtil.nextVisibleLeaf(parent);
                if (isComma(next)) {
                  next.delete();
                }
              }
              parent.delete();
            }
            else {
              super.deleteElement();
            }
          }

          @Contract("null -> false")
          private boolean isComma(@Nullable PsiElement leaf) {
            return leaf instanceof LeafPsiElement && ((LeafPsiElement) leaf).getElementType() == ErlangTypes.ERL_COMMA;
          }
        });
      }
    }
    return null;
  }

  @Nullable
  @Override
  public Collection<PsiElement> getAdditionalElementsToDelete(PsiElement element, Collection<PsiElement> allElementsToDelete, boolean askUser) {
    if (element instanceof ErlangFunction) {
      ErlangSpecification specification = ErlangPsiImplUtil.getSpecification((ErlangFunction) element);
      if (specification != null) {
        return ContainerUtil.newSmartList(specification.getParent());
      }
    }
    return null;
  }

  @Nullable
  @Override
  public Collection<String> findConflicts(PsiElement element, PsiElement[] allElementsToDelete) {
    return null;
  }

  @Nullable
  @Override
  public UsageInfo[] preprocessUsages(Project project, UsageInfo[] usages) {
    return usages;
  }

  @Override public void prepareForDeletion(PsiElement element) throws IncorrectOperationException { }
  @Override public boolean isToSearchInComments(PsiElement element) { return false; }
  @Override public void setToSearchInComments(PsiElement element, boolean enabled) { }
  @Override public boolean isToSearchForTextOccurrences(PsiElement element) { return false; }
  @Override public void setToSearchForTextOccurrences(PsiElement element, boolean enabled) { }
}
