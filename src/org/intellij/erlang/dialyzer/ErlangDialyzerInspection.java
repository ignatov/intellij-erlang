package org.intellij.erlang.dialyzer;

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.inspection.ErlangInspectionBase;

public class ErlangDialyzerInspection extends ErlangInspectionBase {
  public static final String INSPECTION_SHORT_NAME = "ErlangDialyzerInspection";
  public static final Key<ErlangDialyzerInspection> KEY = Key.create(INSPECTION_SHORT_NAME);

  @Override
  protected void checkFile(PsiFile file, ProblemsHolder problemsHolder) {
  }
}
