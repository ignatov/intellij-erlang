package org.intellij.erlang.spikeprobe;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.parser.macro.ErlangMacroSubstitution;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;

/** Verifier 2: typing speed, modes off and fragments interleaved in one JVM. PROBE_OUT/label/typing.txt. */
public class Verifier2TypingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang/lib/";

  @Override protected LightProjectDescriptor getProjectDescriptor() { return descriptor(); }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  @Override
  protected void runTestRunnable(com.intellij.util.ThrowableRunnable<Throwable> testRunnable) throws Throwable {
    if (env("PROBE_OUT") == null) return;
    super.runTestRunnable(testRunnable);
  }

  private long typeOnce(String text, int at) {
    Document d = myFixture.getEditor().getDocument();
    WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(text));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    myFixture.getFile().getNode().getFirstChildNode();
    myFixture.getEditor().getCaretModel().moveToOffset(at);
    String typed = "        foo(Bar),\n        baz(Qux, 1),\n        ok,\n          ";
    typed = (typed + typed).substring(0, 50);
    long t0 = System.nanoTime();
    for (char c : typed.toCharArray()) {
      myFixture.type(c);
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    }
    return (System.nanoTime() - t0) / 1_000_000;
  }

  public void testTypingInterleaved() throws Exception {
    StringBuilder out = new StringBuilder();
    String[][] files = {
      {OTP + "dialyzer-6.0.3/src/erl_types.erl", "t_inf_lists_strict(L1, L2) ->\n"},
      {OTP + "xmerl-2.2/src/xmerl_scan.erl", "    ets:delete(Rules),\n"}};
    ErlangMacroSubstitution.Mode[] modes = {ErlangMacroSubstitution.Mode.OFF, ErlangMacroSubstitution.Mode.FRAGMENTS};
    int n = 0;
    for (String[] f : files) {
      String text = read(Paths.get(f[0]));
      int at = text.indexOf(f[1]) + f[1].length();
      Map<String, List<Long>> res = new LinkedHashMap<>();
      for (int rep = 0; rep < 5; rep++) {
        for (int k = 0; k < 2; k++) {
          ErlangMacroSubstitution.Mode mode = modes[(rep + k) % 2];
          com.intellij.openapi.Disposable disp = Disposer.newDisposable();
          ErlangMacroSubstitution.setModeForTests(mode, disp);
          try {
            myFixture.configureByText("t" + (n++) + "_" + Paths.get(f[0]).getFileName(), text);
            int[] c0 = treeCounts(myFixture.getFile());
            typeOnce(text, at);
            long ms = typeOnce(text, at);
            res.computeIfAbsent(mode.name(), x -> new ArrayList<>()).add(ms);
            out.append(Paths.get(f[0]).getFileName()).append(" rep ").append(rep).append(' ').append(mode).append(": ").append(ms)
              .append(" ms / 50 keys; calls=").append(c0[2]).append(" foreign=").append(c0[1]).append('\n');
          }
          finally {
            Disposer.dispose(disp);
          }
        }
      }
      for (Map.Entry<String, List<Long>> e : res.entrySet()) {
        List<Long> s = new ArrayList<>(e.getValue());
        Collections.sort(s);
        out.append(Paths.get(f[0]).getFileName()).append(' ').append(e.getKey()).append(" median per keystroke: ")
          .append(s.get(s.size() / 2) / 50.0).append(" ms, all ").append(e.getValue()).append('\n');
      }
    }
    Path dir = Paths.get(env("PROBE_OUT"), label());
    Files.createDirectories(dir);
    Files.writeString(dir.resolve("typing.txt"), out.toString());
  }
}
