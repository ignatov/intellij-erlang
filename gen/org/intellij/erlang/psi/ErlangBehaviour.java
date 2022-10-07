// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import org.intellij.erlang.stubs.ErlangBehaviourStub;

public interface ErlangBehaviour extends ErlangMetaAttribute, StubBasedPsiElement<ErlangBehaviourStub> {

  @Nullable
  ErlangModuleRef getModuleRef();

  @Nullable
  PsiElement getParLeft();

  @Nullable
  PsiElement getParRight();

  @NotNull String getName();

}
