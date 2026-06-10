// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface ErlangAttribute extends ErlangCompositeElement {

  @Nullable
  ErlangAtomAttribute getAtomAttribute();

  @Nullable
  ErlangBehaviour getBehaviour();

  @Nullable
  ErlangCallbackSpec getCallbackSpec();

  @Nullable
  ErlangElseAtomAttribute getElseAtomAttribute();

  @Nullable
  ErlangExport getExport();

  @Nullable
  ErlangExportRecordAttribute getExportRecordAttribute();

  @Nullable
  ErlangExportTypeAttribute getExportTypeAttribute();

  @Nullable
  ErlangImportDirective getImportDirective();

  @Nullable
  ErlangImportRecordAttribute getImportRecordAttribute();

  @Nullable
  ErlangModule getModule();

  @Nullable
  ErlangOptionalCallbacks getOptionalCallbacks();

  @Nullable
  ErlangSpecification getSpecification();

  @NotNull
  PsiElement getOpMinus();

}
