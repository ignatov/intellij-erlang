package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.psi.PsiFile;

import java.util.*;

/**
 * Shapes of scale and of partial knowledge: an include that does not resolve next to macros of the file itself, a file
 * with thousands of macro uses and a huge macro body, a file that includes many headers, a header included by many
 * files, an empty file, a file of only {@code ?}, deep nesting, non-ASCII text, CRLF line ends, a header opened by itself.
 * Each goes through the warm-up, highlighting, editor actions and the targets at some offsets; an action slower than
 * {@code MACRO_FUZZ_SLOW_MS} (10 s) is reported (timing never fails the run).
 */
public class MacroFuzzScaleTest extends MacroFuzzTestBase {
  static final String[] SHAPES = {"missing-include", "many-uses", "huge-body", "many-headers", "many-includers", "empty", "only-?",
    "deep-nesting", "non-ascii", "crlf", "header-alone"};

  private static boolean smoke() {
    return !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
  }

  static Map<String, String> shape(String name, Random rnd, int scale) {
    Map<String, String> f = new LinkedHashMap<>();
    StringBuilder sb = new StringBuilder("-module(m).\n-export([f/1]).\n");
    switch (name) {
      case "missing-include" -> {
        sb.append("-include(\"missing.hrl\").\n-include_lib(\"nolib/include/x.hrl\").\n");
        sb.append("-define(TAIL(X), , X]).\n-define(ARROW, ->).\n-define(LISTS, lists:).\n-define(ADD(A, B), A + B).\n");
        sb.append("f(L) -> {[1 ?TAIL(2), ?LISTS reverse(L), ?ADD(1, ?MISSING_ONE), ?FROM_MISSING(L)}.\n");
        sb.append("g() ?ARROW ok.\n");
      }
      case "many-uses" -> {
        sb.append("-define(C, 1).\n-define(F(X), {X, ?C}).\n-define(G(X, Y), [X | Y]).\nf(X) ->\n  [");
        for (int i = 0; i < scale * 300; i++) sb.append(i > 0 ? ",\n   " : "").append(i % 3 == 0 ? "?C" : i % 3 == 1 ? "?F(X)" : "?G(X, ?C)");
        sb.append("].\n");
      }
      case "huge-body" -> {
        sb.append("-define(BIG(X), [");
        for (int i = 0; i < scale * 1500; i++) sb.append(i > 0 ? ", " : "").append("{X, ").append(i).append('}');
        sb.append("]).\nf(X) -> ?BIG(X).\n");
      }
      case "many-headers" -> {
        int n = scale * 30;
        for (int i = 0; i < n; i++) {
          f.put("inc/h" + i + ".hrl", "-define(H" + i + ", " + i + ").\n-define(HF" + i + "(X), {X, ?H" + i + "}).\n" +
                                      (i > 0 ? "-define(HC" + i + ", ?H" + (i - 1) + ").\n" : ""));
          sb.append("-include(\"inc/h").append(i).append(".hrl\").\n");
        }
        sb.append("f(X) -> [");
        for (int i = 0; i < n; i++) sb.append(i > 0 ? ", " : "").append("?HF").append(i).append("(X)").append(i > 0 ? ", ?HC" + i : "");
        sb.append("].\n");
      }
      case "many-includers" -> {
        f.put("shared.hrl", "-define(SH(V), V = 42).\n-define(SHC, shared_value).\n-define(SHF(F), F() -> ?SHC.).\n");
        int n = scale * 30;
        for (int i = 0; i < n; i++) {
          f.put("inc_" + i + ".erl", "-module(inc_" + i + ").\n-include(\"shared.hrl\").\n-export([g/0]).\ng() -> ?SH(Z), {Z, ?SHC}.\n");
        }
        sb.append("-include(\"shared.hrl\").\nf(X) -> ?SH(Y), {X, Y, ?SHC}.\n?SHF(made).\n");
      }
      case "empty" -> {
        f.put("m.erl", "");
        return f;
      }
      case "only-?" -> {
        f.put("m.erl", "?");
        return f;
      }
      case "deep-nesting" -> {
        int depth = scale * 40;
        sb.append("-define(N0(X), X).\n");
        for (int i = 1; i < depth; i++) sb.append("-define(N").append(i).append("(X), ?N").append(i - 1).append("({X})).\n");
        sb.append("f(X) -> ?N").append(depth - 1).append("(X).\n");
        sb.append("g(X) -> ").append("(".repeat(depth)).append("?N1(X)").append(")".repeat(depth)).append(".\n");
      }
      case "non-ascii" -> {
        sb.append("%% Комментарий ünïcödé 日本語\n-define(ÄTOM, 'ünïcödé атом').\n-define(S, \"строка ✓\").\n");
        sb.append("f(X) -> {?S, 'ünïcödé атом', X, ?MODULE, \"日本語\"}.\n");
      }
      case "crlf" -> {
        sb.append("-define(C, 1).\n-define(F(X),\n  {X,\n   ?C}).\nf(X) ->\n  ?F(X).\n");
        f.put("m.erl", sb.toString().replace("\n", "\r\n"));
        return f;
      }
      default -> {
        // a header opened by itself: macros of other headers, variables in bodies, a made function
        f.put("base.hrl", "-define(BASE, 1).\n");
        f.put("alone.hrl", "-include(\"base.hrl\").\n-define(USE, ?BASE + 1).\n-define(BIND(V), V = ?USE).\n" +
                           "-define(GEN(F), F() -> ?USE.).\n-record(alone_rec, {a = ?BASE}).\n");
        f.put("m.erl", sb.append("-include(\"alone.hrl\").\nf(X) -> ?BIND(Y), {X, Y}.\n?GEN(made).\n").toString());
        return f;
      }
    }
    f.put("m.erl", sb.toString());
    return f;
  }

  public void testScaleShapes() throws Throwable {
    EditorActionProbe.enableInspections(myFixture);
    runCases("scale", SHAPES.length, (index, rnd, caseId) -> {
      String name = SHAPES[index % SHAPES.length];
      int scale = smoke() ? 1 : 1 + index / SHAPES.length % 4;
      Map<String, String> files = shape(name, rnd, scale);
      FuzzWorld w = world(files, false);
      try {
        rec.guard("shape " + name + " x" + scale, () -> {});
        String main = name.equals("header-alone") ? "alone.hrl" : "m.erl";
        PsiFile f = w.psi(main);
        open(f.getVirtualFile());
        warm(myFixture.getFile());
        EditorActionProbe probe = new EditorActionProbe(rec, myFixture);
        rec.guard("highlight", () -> myFixture.doHighlighting());
        TargetProbe targets = new TargetProbe(rec, myFixture.getEditor(), myFixture.getFile());
        targets.searchedTargets = 2;
        List<Integer> offsets = TargetProbe.offsets(myFixture.getFile(), rnd, 5);
        Collections.shuffle(offsets, rnd);
        for (int offset : offsets.subList(0, Math.min(offsets.size(), smoke() ? 3 : 25))) {
          myFixture.getEditor().getCaretModel().moveToOffset(offset);
          probe.run(probe.random(rnd, true), rnd, "@" + offset + " ");
          targets.probeOffset(offset, rnd.nextBoolean());
        }
        if (name.equals("many-includers") || name.equals("header-alone")) {
          // the header by itself: find usages and rename in its body search the includers
          PsiFile h = w.psi(name.equals("many-includers") ? "shared.hrl" : "alone.hrl");
          open(h.getVirtualFile());
          TargetProbe ht = new TargetProbe(rec, myFixture.getEditor(), myFixture.getFile());
          ht.searchedTargets = 3;
          for (int offset : TargetProbe.offsets(myFixture.getFile(), rnd, 3).subList(0, 4)) ht.probeOffset(offset, true);
        }
      }
      finally {
        w.dispose();
      }
    });
  }
}
