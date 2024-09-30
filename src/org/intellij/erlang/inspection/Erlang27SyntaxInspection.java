package org.intellij.erlang.inspection;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangStringLiteral;
import org.intellij.erlang.psi.ErlangVisitor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

public class Erlang27SyntaxInspection extends ErlangInspectionBase {
  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    ErlangSdkRelease release = ErlangSdkType.getRelease(file);
    return release == null || ErlangSdkRelease.V_27_0.isNewerThan(release);
  }

  @NotNull
  @Override
  public ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder,
                                          @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
        String text = o.getText();
        if (text.startsWith("\"\"\"") || text.endsWith("\"\"\"")) {
          holder.registerProblem(o, "Triple quotes are only supported in Erlang 27 and newer versions");
        }
      }
    };
  }
}