package org.intellij.erlang.macrolayer;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: the cost of the separate expansion-error pass on erl_types.erl and xmerl_scan.erl: the forms it checks with
 * and without the check of plain forms, the copy builds, and the time of a pass from scratch (copy built) and of a
 * rerun (copy cached). Runs only when {@code RED_COST} is set; output lines "RED-COST ...".
 */
public class ErlangMacroLayerRedPassCostTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangMacroLayerRedPassCostTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor() {
      @Override
      public com.intellij.openapi.projectRoots.Sdk getSdk() {
        return org.intellij.erlang.sdk.ErlangSdkType.createMockSdk(ErlangMacroLayerSoundnessTest.OTP_ROOT,
                                                                   org.intellij.erlang.sdk.ErlangSdkRelease.V_29_0);
      }
    };
  }

  private static void out(String s) {
    System.out.println("RED-COST " + s);
  }

  private void copyApp(String appDir) throws Exception {
    String name = appDir.replaceAll("-[0-9.]+$", "");
    Path app = Paths.get(ErlangMacroLayerSoundnessTest.OTP, appDir);
    for (String dir : List.of("src", "include", "ebin")) {
      Path d = app.resolve(dir);
      if (!Files.isDirectory(d)) continue;
      try (Stream<Path> s = Files.walk(d)) {
        for (Path p : s.filter(Files::isRegularFile).toList()) {
          String fn = p.getFileName().toString();
          if (fn.endsWith(".hrl") || fn.endsWith(".erl") || fn.endsWith(".app")) {
            myFixture.addFileToProject("apps/" + name + "/" + app.relativize(p), read(p));
          }
        }
      }
    }
    myFixture.addFileToProject("apps/" + name + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}]}.\n");
  }

  public void testCost() throws Exception {
    if (env("RED_COST") == null) return;
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable()); // no refusal in the quiet time after the edits
    copyApp("kernel-11.0.4");
    copyApp("stdlib-8.1");
    copyApp("dialyzer-6.0.3");
    copyApp("xmerl-2.2");
    int rounds = Integer.parseInt(Objects.requireNonNullElse(env("RED_COST_ROUNDS"), "5"));
    for (String rel : List.of("apps/dialyzer/src/erl_types.erl", "apps/xmerl/src/xmerl_scan.erl")) {
      VirtualFile vf = myFixture.findFileInTempDir(rel);
      PsiFile file = PsiManager.getInstance(getProject()).findFile(vf);
      ErlangFile ef = (ErlangFile)file;
      int[] forms = inBackground(() -> {
        ErlangMacroLayer.Table t = ErlangMacroLayer.table(ef);
        int use = 0, need = 0, all = 0;
        for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
          if (c instanceof com.intellij.psi.PsiWhiteSpace || c instanceof com.intellij.psi.PsiComment) continue;
          all++;
          if (ErlangMacroLayer.formUsesMacro(t, c)) use++;
          if (ErlangMacroLayer.formNeedsCopy(t, c)) need++;
        }
        return new int[]{all, use, need};
      });
      out(vf.getName() + " forms=" + forms[0] + " with a macro use=" + forms[1] + " needing the copy (main gate)=" + forms[2]);
      for (boolean plain : new boolean[]{false, true}) {
        com.intellij.openapi.Disposable d = com.intellij.openapi.util.Disposer.newDisposable();
        try {
          ErlangMacroLayerExpansionErrors.setChecksPlainFormsForTests(plain, d);
          List<Double> scratch = new ArrayList<>(), rerun = new ArrayList<>();
          long builds = 0;
          int problems = 0;
          for (int i = 0; i < rounds; i++) {
            // an edit at the end of the file: a new table and a new copy, as after typing
            com.intellij.openapi.editor.Document doc = com.intellij.psi.PsiDocumentManager.getInstance(getProject()).getDocument(file);
            com.intellij.openapi.command.WriteCommandAction.runWriteCommandAction(getProject(), () -> doc.insertString(doc.getTextLength(), "\n"));
            com.intellij.psi.PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
            ErlangMacroLayerLog.reset();
            long t0 = System.nanoTime();
            List<ErlangMacroLayerExpansionErrors.Problem> p = inBackground(() -> ErlangMacroLayerExpansionErrors.problems(ef));
            scratch.add((System.nanoTime() - t0) / 1e6);
            builds += builds().stream().filter(e -> e.file().equals(vf.getName())).count();
            problems = p == null ? -1 : p.size();
            long t1 = System.nanoTime();
            inBackground(() -> ErlangMacroLayerExpansionErrors.problems(ef));
            rerun.add((System.nanoTime() - t1) / 1e6);
          }
          out(vf.getName() + " plain-check=" + plain + " errors=" + problems + " builds=" + builds + "/" + rounds +
              " pass from scratch ms: " + stats(scratch) + " | rerun ms: " + stats(rerun) + " | load " + load());
        }
        finally {
          com.intellij.openapi.util.Disposer.dispose(d);
        }
      }
    }
  }
}
