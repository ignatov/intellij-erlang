package org.intellij.erlang;

import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangDescriptionProvider implements ElementDescriptionProvider {
  @Override
  public String getElementDescription(@NotNull PsiElement psiElement, @NotNull ElementDescriptionLocation location) {
    if (location == UsageViewNodeTextLocation.INSTANCE && psiElement instanceof ErlangNamedElement) {
      return getElementDescription(psiElement, UsageViewTypeLocation.INSTANCE) + " " +
        "'" + getElementDescription(psiElement, UsageViewShortNameLocation.INSTANCE) + "'";
    }
    if ((location == UsageViewShortNameLocation.INSTANCE || location == UsageViewLongNameLocation.INSTANCE) && psiElement instanceof ErlangNamedElement) {
      return ((ErlangNamedElement) psiElement).getName();
    }
    if (location == UsageViewTypeLocation.INSTANCE) {
      if (psiElement instanceof ErlangModule) return "Module";
      if (psiElement instanceof ErlangFunction) return "Function";
      if (psiElement instanceof ErlangRecordDefinition) return "Record";
      if (psiElement instanceof ErlangQVar) return "Variable";
    }
    return null;
  }
}
