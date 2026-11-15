package org.intellij.erlang.resolve;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangNamedFunVariableResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testProbe() {
    myFixture.configureByText("a.erl",
      "-module(a).\n" +
      "foo() ->\n" +
      "  fun\n" +
      "    NamedFun\n" +
      "    (1, _) -> NamedFun(0, 0);\n" +
      "    NamedFun\n" +
      "    (0, NamedFun) ->\n" +
      "      NamedFun(1, 0);\n" +
      "    NamedFun(0, _) ->\n" +
      "      NamedFun(1, 1)\n" +
      "  end.\n");
    List<ErlangQVar> vars = new ArrayList<>();
    for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class)) {
      if ("NamedFun".equals(v.getText())) vars.add(v);
    }
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < vars.size(); i++) {
      PsiPolyVariantReference ref = (PsiPolyVariantReference) vars.get(i).getReference();
      sb.append("#").append(i + 1).append(" resolve=").append(vars.indexOf(ref.resolve()) + 1).append(" multi=");
      for (ResolveResult r : ref.multiResolve(false)) sb.append(vars.indexOf(r.getElement()) + 1).append(",");
      sb.append("\n");
    }
    System.out.println("PROBE\n" + sb);
    fail(sb.toString());
  }
}
