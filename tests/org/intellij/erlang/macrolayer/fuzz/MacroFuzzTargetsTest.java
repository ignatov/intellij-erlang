package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.psi.PsiFile;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Family 4: every kind of target and reference host the platform gets at an offset (macro uses, bare macro references,
 * names made by macros, header targets, predefined and undefined macros) through presentation, description,
 * documentation, find usages, rename and navigation. Case 0 is a fixed module with every predefined macro; the others are
 * generated.
 */
public class MacroFuzzTargetsTest extends MacroFuzzTestBase {
  static final String PREDEFINED_CASE = """
    -module(m).
    -include("h1.hrl").
    -export([f/0, g/1]).
    -define(C, 1).
    -define(ADD(A, B), A + B).
    -define(GEN(F), F() -> ok.).
    ?GEN(made_fn).
    f() -> {?MODULE, ?LINE, ?FILE, ?MACHINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?MODULE_STRING, ?OTP_RELEASE,
            ?FEATURE_AVAILABLE(maybe_expr), ?FEATURE_ENABLED(maybe_expr), ?UNDEFINED_ONE, ?C, ?ADD(1, 2), ?H0, made_fn()}.
    g(X) -> ?ADD(X, ?C).
    """;

  public void testTargetsAtOffsets() throws Throwable {
    runCases("targets", 3, (index, rnd, caseId) -> {
      Map<String, String> files;
      if (index == 0) {
        files = new LinkedHashMap<>();
        files.put("h1.hrl", "-define(H0, header_value).\n-define(HF(N), N() -> 1.).\n");
        files.put("m.erl", PREDEFINED_CASE);
      }
      else {
        files = new MacroCaseGenerator(rnd).project();
      }
      FuzzWorld w = world(files, false);
      try {
        boolean smoke = !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
        for (String path : List.of("m.erl", "h1.hrl")) {
          PsiFile f = w.psi(path);
          if (f == null) continue;
          open(f.getVirtualFile());
          warm(f);
          TargetProbe probe = new TargetProbe(rec, myFixture.getEditor(), myFixture.getFile());
          for (int offset : TargetProbe.offsets(myFixture.getFile(), rnd, smoke ? 10 : 60)) {
            probe.probeOffset(offset, false);
            probe.probeOffset(offset, true);
          }
          if (smoke) break;
        }
      }
      finally {
        w.dispose();
      }
    });
  }
}
