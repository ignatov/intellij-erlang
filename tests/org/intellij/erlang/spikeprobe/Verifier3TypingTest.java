package org.intellij.erlang.spikeprobe;

import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;

/** Verifier 3: typing and highlighting times; same source on master and on the layer (one JVM per mode). */
public class Verifier3TypingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang/lib/";
  private static final String EJ = "/Users/ignatov/factory-work/macro-research/count/corpus/ejabberd/src/ejabberd_commands_doc.erl";
  private int myN;

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

  private void enableAll() {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  private static double median(List<Double> v) {
    List<Double> s = new ArrayList<>(v);
    Collections.sort(s);
    return s.get(s.size() / 2);
  }

  public void testTiming() throws Exception {
    enableAll();
    StringBuilder out = new StringBuilder();
    String[][] files = {
      {OTP + "dialyzer-6.0.3/src/erl_types.erl", "t_inf_lists_strict(L1, L2) ->\n"},
      {OTP + "xmerl-2.2/src/xmerl_scan.erl", "    ets:delete(Rules),\n"}};
    for (String[] f : files) {
      String text = read(Paths.get(f[0]));
      int at = text.indexOf(f[1]) + f[1].length();
      List<Double> commitOnly = new ArrayList<>(), withHl = new ArrayList<>();
      for (int rep = 0; rep < 5; rep++) {
        myFixture.configureByText("t" + (myN++) + "_" + Paths.get(f[0]).getFileName(), text);
        myFixture.doHighlighting();
        Document d = myFixture.getEditor().getDocument();
        myFixture.getEditor().getCaretModel().moveToOffset(at);
        String typed = "        foo(Bar),\n        baz(Qux, 1),\n        ok,\n          ".substring(0, 40);
        long t0 = System.nanoTime();
        for (char c : typed.toCharArray()) { myFixture.type(c); PsiDocumentManager.getInstance(getProject()).commitAllDocuments(); }
        commitOnly.add((System.nanoTime() - t0) / 1e6 / typed.length());
        String typed2 = "x1(Y),\n";
        t0 = System.nanoTime();
        for (char c : typed2.toCharArray()) { myFixture.type(c); PsiDocumentManager.getInstance(getProject()).commitAllDocuments(); myFixture.doHighlighting(); }
        withHl.add((System.nanoTime() - t0) / 1e6 / typed2.length());
        WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(""));
      }
      String n = Paths.get(f[0]).getFileName().toString();
      out.append(n).append(" commitOnly median ms/key ").append(String.format("%.1f", median(commitOnly))).append(" all ").append(commitOnly).append('\n');
      out.append(n).append(" keystroke+highlight median ms/key ").append(String.format("%.1f", median(withHl))).append(" all ").append(withHl).append('\n');
    }
    String ej = read(Paths.get(EJ));
    List<Double> full = new ArrayList<>();
    for (int rep = 0; rep < 5; rep++) {
      myFixture.configureByText("ej" + (myN++) + "_ejabberd_commands_doc.erl", ej);
      long t0 = System.nanoTime();
      myFixture.doHighlighting();
      full.add((System.nanoTime() - t0) / 1e6);
    }
    out.append("ejabberd_commands_doc full highlighting median ms ").append(String.format("%.0f", median(full))).append(" all ").append(full).append('\n');
    Path dir = Paths.get(env("PROBE_OUT"), label(), "typing");
    Files.createDirectories(dir);
    Files.writeString(dir.resolve("timing-" + System.currentTimeMillis() + ".txt"), out.toString());
  }
}
