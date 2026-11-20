package org.intellij.erlang.macrolayer;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiModificationTracker;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/** Spike 4, step 1: why did switching between 20 highlighted files rebuild 19 copies without an edit? */
public class ErlangMacroLayerSwitchDiagTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static void out(String s) {
    System.out.println("LAYER-DIAG " + s);
  }

  public void testSwitch20() throws Exception {
    String list = System.getenv("OPEN20_FILES");
    if (list == null) return;
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    List<VirtualFile> vfs = new ArrayList<>();
    int n = 0;
    for (String l : Files.readAllLines(Paths.get(list))) {
      if (l.isBlank()) continue;
      PsiFile f = myFixture.addFileToProject("open" + (n++) + "/" + Paths.get(l.trim()).getFileName(), Files.readString(Paths.get(l.trim())));
      vfs.add(f.getVirtualFile());
    }
    List<String> changes = new ArrayList<>();
    PsiManager.getInstance(getProject()).addPsiTreeChangeListener(new PsiTreeChangeAdapter() {
      @Override
      public void childrenChanged(@org.jetbrains.annotations.NotNull PsiTreeChangeEvent event) {
        changes.add("childrenChanged " + (event.getFile() == null ? null : event.getFile().getName()) + " physical=" + (event.getFile() != null && event.getFile().isPhysical()));
      }

      @Override
      public void propertyChanged(@org.jetbrains.annotations.NotNull PsiTreeChangeEvent event) {
        changes.add("propertyChanged " + event.getPropertyName() + " " + (event.getFile() == null ? event.getElement() : event.getFile().getName()));
      }

      @Override
      public void childReplaced(@org.jetbrains.annotations.NotNull PsiTreeChangeEvent event) {
        changes.add("childReplaced " + (event.getFile() == null ? null : event.getFile().getName()));
      }
    }, myFixture.getTestRootDisposable());
    PsiModificationTracker tracker = PsiModificationTracker.getInstance(getProject());
    List<String> bumps = new ArrayList<>();
    getProject().getMessageBus().connect(myFixture.getTestRootDisposable()).subscribe(PsiModificationTracker.TOPIC, () -> {
      StringBuilder sb = new StringBuilder();
      int k = 0;
      for (StackTraceElement f : new Throwable().getStackTrace()) {
        String c = f.getClassName();
        if (c.startsWith("java.") || c.startsWith("jdk.") || c.contains("MessageBus") || c.contains("PsiModificationTracker")) continue;
        sb.append(c.substring(c.lastIndexOf('.') + 1)).append('.').append(f.getMethodName()).append(" < ");
        if (++k == 12) break;
      }
      bumps.add(sb.toString());
    });
    for (VirtualFile v : vfs) {
      long c0 = tracker.getModificationCount();
      int b0 = bumps.size();
      myFixture.configureFromExistingVirtualFile(v);
      long c1 = tracker.getModificationCount();
      int b1 = bumps.size();
      myFixture.doHighlighting();
      long c2 = tracker.getModificationCount();
      out("first " + v.getName() + " psiCount configure " + c0 + "->" + c1 + " highlight ->" + c2 +
          " bumps configure=" + bumps.subList(b0, b1) + " highlight=" + bumps.subList(b1, bumps.size()));
    }
    for (int round = 0; round < 2; round++) {
      int before = ErlangMacroLayerLog.events().size();
      for (VirtualFile v : vfs.subList(1, vfs.size())) {
        long c0 = tracker.getModificationCount();
        int e0 = changes.size();
        myFixture.configureFromExistingVirtualFile(v);
        long c1 = tracker.getModificationCount();
        int e1 = changes.size();
        myFixture.doHighlighting();
        long c2 = tracker.getModificationCount();
        out("round=" + round + " " + v.getName() + " psiCount configure " + c0 + "->" + c1 + " highlight ->" + c2 +
            " events configure=" + changes.subList(e0, e1) + " highlight=" + changes.subList(e1, changes.size()));
      }
      List<ErlangMacroLayerLog.Event> ev = ErlangMacroLayerLog.events();
      out("round=" + round + " builds=" + (ev.size() - before));
      for (ErlangMacroLayerLog.Event e : ev.subList(before, ev.size())) out("round=" + round + " " + e.line());
    }
  }
}
