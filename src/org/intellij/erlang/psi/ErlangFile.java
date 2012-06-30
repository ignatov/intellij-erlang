package org.intellij.erlang.psi;

import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ErlangFile extends PsiFile {
  @NotNull
  List<ErlangRule> getRules();

  @NotNull
  List<ErlangAttribute> getAttributes();

  @NotNull
  List<ErlangFunction> getFunctions();

  @Nullable
  ErlangFunction getFunction(String name, int argsCount);

  @NotNull
  List<ErlangRecordDefinition> getRecords();

  @Nullable
  ErlangRecordDefinition getRecord(String name);
}
