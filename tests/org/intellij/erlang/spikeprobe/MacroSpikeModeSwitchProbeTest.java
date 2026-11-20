package org.intellij.erlang.spikeprobe;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubTree;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;

/**
 * Prober of the macro spike (spike branch only): the stub index is built in one mode and the AST is loaded in another,
 * as after a change of the switch with an existing index (the stub version does not depend on the mode).
 */
public class MacroSpikeModeSwitchProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  public void testIndexFromOtherMode() throws Exception {
    String out = env("PROBE_OUT");
    if (out == null) return;
    StringBuilder log = new StringBuilder();
    String text = read(Paths.get("testData/macroSpikeProbes/def.erl"));
    for (ErlangMacroLayer.Mode[] pair : new ErlangMacroLayer.Mode[][]{
      {ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.Mode.LAYER},
      {ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.Mode.OFF}}) {
      com.intellij.openapi.Disposable d1 = Disposer.newDisposable();
      ErlangMacroLayer.setModeForTests(pair[0], d1);
      PsiFile file = myFixture.addFileToProject("switch-" + pair[0] + "/def.erl", text);
      String[] stub = new String[1];
      ReadAction.run(() -> {
        PsiFileImpl psi = (PsiFileImpl)PsiManager.getInstance(getProject()).findFile(file.getVirtualFile());
        psi.clearCaches();
        StubTree t = psi.getStubTree();
        stub[0] = t == null ? "null" : DebugUtil.stubTreeToString(t.getRoot());
      });
      Disposer.dispose(d1);
      com.intellij.openapi.Disposable d2 = Disposer.newDisposable();
      ErlangMacroLayer.setModeForTests(pair[1], d2);
      log.append("index built in ").append(pair[0]).append(", AST loaded in ").append(pair[1]).append('\n');
      log.append("  stub from index: ").append(stub[0].replace('\n', ' ')).append('\n');
      List<Failure> f = guarded(() -> ReadAction.run(() -> {
        PsiFileImpl psi = (PsiFileImpl)PsiManager.getInstance(getProject()).findFile(file.getVirtualFile());
        List<ErlangFunction> fns = ((ErlangFile)psi).getFunctions();
        log.append("  functions via stub: ").append(fns.size()).append('\n');
        psi.getNode().getFirstChildNode();
        for (ErlangFunction fn : ((ErlangFile)psi).getFunctions()) fn.getName();
        log.append("  functions after AST load: ").append(((ErlangFile)psi).getFunctions().size()).append('\n');
      }));
      for (Failure x : f) log.append("  FAILURE ").append(x.line()).append('\n');
      Disposer.dispose(d2);
    }
    Files.createDirectories(Paths.get(out, label(), "features"));
    Files.writeString(Paths.get(out, label(), "features", "testIndexFromOtherMode.txt"), log.toString());
  }
}
