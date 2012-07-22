package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author ignatov
 */
public class ErlangRecordReferenceImpl<T extends ErlangQAtom> extends ErlangAtomBasedReferenceImpl<T> {
  public ErlangRecordReferenceImpl(@NotNull T element, TextRange range, String name) {
    super(element, range, name);
  }

  @Override
  public PsiElement resolve() {
    PsiFile containingFile = myElement.getContainingFile();
    if (containingFile instanceof ErlangFile) {
      ErlangRecordDefinition record = ((ErlangFile) containingFile).getRecord(myReferenceName);
      if (record != null) return record;

      List<ErlangRecordDefinition> fromIncludes = ErlangPsiImplUtil.getErlangRecordFromIncludes(containingFile, false, myReferenceName);
      return ContainerUtil.getFirstItem(fromIncludes);
    }
    return null;
  }

  @NotNull
  @Override
  public Object[] getVariants() {
    return ArrayUtil.toObjectArray(ErlangPsiImplUtil.getRecordLookupElements(myElement.getContainingFile()));
  }
}
