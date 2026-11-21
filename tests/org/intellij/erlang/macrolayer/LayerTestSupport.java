package org.intellij.erlang.macrolayer;

import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.Callable;

/** Spike 4: helpers of the measurement tests. */
final class LayerTestSupport {
  private LayerTestSupport() {
  }

  static String env(String name) {
    String v = System.getenv(name);
    return v == null || v.isBlank() ? null : v.trim();
  }

  static LightProjectDescriptor descriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  static String read(Path file) throws IOException {
    byte[] bytes = Files.readAllBytes(file);
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text.replace("\r\n", "\n").replace('\r', '\n');
  }

  static void enableAll(CodeInsightTestFixture fixture) {
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    fixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
  }

  static String load() {
    double[] l = new double[1];
    l[0] = ManagementFactory.getOperatingSystemMXBean().getSystemLoadAverage();
    return String.format(Locale.ROOT, "%.1f", l[0]);
  }

  static double median(List<Double> v) {
    List<Double> s = new ArrayList<>(v);
    Collections.sort(s);
    return s.isEmpty() ? Double.NaN : s.get(s.size() / 2);
  }

  static String stats(List<Double> v) {
    List<Double> s = new ArrayList<>(v);
    Collections.sort(s);
    if (s.isEmpty()) return "n=0";
    return String.format(Locale.ROOT, "median=%.1f min=%.1f max=%.1f n=%d all=%s", median(v), s.get(0), s.get(s.size() - 1),
                         s.size(), v.stream().map(d -> String.format(Locale.ROOT, "%.0f", d)).toList());
  }

  /**
   * Runs in a pooled thread inside a read action (like highlighting and navigation in the IDE). Round 5: under a
   * progress indicator, as background work in the IDE runs (a caller without one cannot be cancelled, and the layer
   * gives it master's answer instead of building a copy).
   */
  static <T> T inBackground(Callable<T> c) throws Exception {
    return inBackgroundWithIndicator(c);
  }

  /** Round 5: a pooled thread, a plain read action, no progress indicator: a caller that cannot be cancelled. */
  static <T> T inBackgroundWithoutIndicator(Callable<T> c) throws Exception {
    return ApplicationManager.getApplication().executeOnPooledThread(() -> ReadAction.compute(c::call)).get();
  }

  /**
   * Round 5: runs in a pooled thread inside a read action under a progress indicator that is not the daemon's (like the
   * target-element search or find usages in the IDE).
   */
  static <T> T inBackgroundWithIndicator(Callable<T> c) throws Exception {
    return ApplicationManager.getApplication().executeOnPooledThread(
      () -> ProgressManager.getInstance().runProcess(() -> ReadAction.compute(() -> {
        try {
          return c.call();
        }
        catch (RuntimeException e) {
          throw e;
        }
        catch (Exception e) {
          throw new RuntimeException(e);
        }
      }), new EmptyProgressIndicator())).get();
  }

  /** Resolves every reference of the file in a background read action; returns the number resolved. */
  static int resolveAll(PsiFile file) throws Exception {
    return inBackground(() -> {
      int n = 0;
      for (PsiElement e : PsiTreeUtil.collectElements(file, x -> x.getReferences().length > 0)) {
        for (PsiReference r : e.getReferences()) if (r.resolve() != null) n++;
      }
      return n;
    });
  }

  /** Product copy builds in the log (the measurement tests' direct builds are left out). */
  static List<ErlangMacroLayerLog.Event> builds() {
    List<ErlangMacroLayerLog.Event> r = new ArrayList<>();
    for (ErlangMacroLayerLog.Event e : ErlangMacroLayerLog.events()) {
      if (e.kind().equals("BUILD") && !e.trigger().equals("direct")) r.add(e);
    }
    return r;
  }

  static long count(String kind) {
    return ErlangMacroLayerLog.events().stream().filter(e -> e.kind().equals(kind)).count();
  }

  static List<String> uiThreadBuilds() {
    List<String> r = new ArrayList<>();
    for (ErlangMacroLayerLog.Event e : builds()) if (e.edt()) r.add(e.line());
    return r;
  }
}
