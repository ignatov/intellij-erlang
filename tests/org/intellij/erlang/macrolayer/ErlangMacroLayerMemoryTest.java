package org.intellij.erlang.macrolayer;

import com.intellij.codeInspection.InspectionProfileEntry;
import com.intellij.codeInspection.LocalInspectionEP;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import javax.management.ObjectName;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;

/**
 * Round 6 (next), item 1c: what the layer keeps after many files are highlighted, and after they are closed. Runs only
 * when {@code S6_MEMORY} is set. {@code S6_MODE}: {@code layer} (default) or {@code off}. {@code S6_FILES}: how many
 * .erl files (default 100) of the OTP apps xmerl, ssh and inets, copied into the project with the include files of the
 * libraries they use; the OTP 29 installation of {@code MACRO_LAYER_OTP_ROOT} is the SDK (nothing runs without it, or without
 * {@code S6_MEMORY}). Every Erlang inspection is on. A class histogram (after a
 * full GC) counts the layer's objects; the heap in use after that GC is printed too.
 */
public class ErlangMacroLayerMemoryTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String OTP = LayerTestSupport.otpRoot();
  private final Set<String> myCopied = new HashSet<>();

  public ErlangMacroLayerMemoryTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return LayerTestSupport.otpOrMockSdk(System.getenv("S6_MEMORY") != null);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  private static void out(String s) {
    System.out.println("S6-MEMORY " + s);
  }

  private static Path findApp(String name) throws Exception {
    try (Stream<Path> s = Files.list(Paths.get(OTP, "lib"))) {
      return s.filter(p -> p.getFileName().toString().startsWith(name + "-")).findFirst().orElse(null);
    }
  }

  /** Copies an app (src and include; only include for a library) with an .app file and a rebar.config. */
  private void copyApp(String name, boolean full, List<String> erl) throws Exception {
    if (!myCopied.add(name)) return;
    Path app = findApp(name);
    if (app == null) return;
    String root = "apps/" + name;
    Set<String> libs = new TreeSet<>();
    for (String dir : full ? List.of("src", "include") : List.of("include")) {
      Path d = app.resolve(dir);
      if (!Files.isDirectory(d)) continue;
      try (Stream<Path> s = Files.walk(d)) {
        for (Path p : s.filter(Files::isRegularFile).sorted().toList()) {
          String fn = p.getFileName().toString();
          if (!fn.endsWith(".erl") && !fn.endsWith(".hrl")) continue;
          String text = LayerTestSupport.read(p);
          String at = root + "/" + app.relativize(p);
          myFixture.addFileToProject(at, text);
          if (fn.endsWith(".erl") && full) erl.add(at);
          java.util.regex.Matcher m = java.util.regex.Pattern.compile("-include_lib\\(\"([a-z_]+)/").matcher(text);
          while (m.find()) libs.add(m.group(1));
        }
      }
    }
    myFixture.addFileToProject(root + "/ebin/" + name + ".app", "{application, " + name + ", [{vsn, \"1\"}]}.\n");
    myFixture.addFileToProject(root + "/rebar.config", "{erl_opts, [{i, \"include\"}, {i, \"src\"}]}.\n");
    for (String lib : libs) if (!lib.equals(name)) copyApp(lib, false, erl);
  }

  /** Instance counts and bytes of the classes of interest, from a class histogram (the histogram runs a full GC). */
  private static Map<String, long[]> histogram() throws Exception {
    String h = (String)ManagementFactory.getPlatformMBeanServer().invoke(
      new ObjectName("com.sun.management:type=DiagnosticCommand"), "gcClassHistogram", new Object[]{null}, new String[]{String[].class.getName()});
    Map<String, long[]> r = new TreeMap<>();
    for (String line : h.split("\n")) {
      String[] c = line.trim().split("\\s+");
      if (c.length < 4 || !c[0].endsWith(":")) continue;
      String cls = c[3];
      if (cls.startsWith("org.intellij.erlang.macrolayer.") || cls.startsWith("org.intellij.erlang.parser.macro.") ||
          cls.equals("org.intellij.erlang.psi.impl.ErlangFileImpl") || cls.equals("com.intellij.psi.impl.source.tree.CompositeElement") ||
          cls.equals("com.intellij.psi.impl.source.tree.LeafPsiElement") || cls.equals("com.intellij.psi.impl.source.tree.CompositeElement[]")) {
        r.put(cls.replace("org.intellij.erlang.macrolayer.", "").replace("org.intellij.erlang.parser.macro.", ""),
              new long[]{Long.parseLong(c[1]), Long.parseLong(c[2])});
      }
    }
    return r;
  }

  private void report(String when) throws Exception {
    Map<String, long[]> h = histogram();
    long used = ManagementFactory.getMemoryMXBean().getHeapMemoryUsage().getUsed();
    out(when + " heap_used_mb=" + used / (1024 * 1024) + " strong_copies=" + ErlangMacroLayerRetention.getInstance(getProject()).strongCount());
    for (Map.Entry<String, long[]> e : h.entrySet()) {
      out(when + " " + e.getKey() + " instances=" + e.getValue()[0] + " kb=" + e.getValue()[1] / 1024);
    }
  }

  public void testManyFiles() throws Exception {
    if (System.getenv("S6_MEMORY") == null || OTP == null) return;
    String mode = Objects.requireNonNullElse(System.getenv("S6_MODE"), "layer");
    ErlangMacroLayer.setModeForTests("off".equals(mode) ? ErlangMacroLayer.Mode.OFF : ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    int n = Integer.parseInt(Objects.requireNonNullElse(System.getenv("S6_FILES"), "100"));
    List<String> erl = new ArrayList<>();
    for (String app : List.of("xmerl", "ssh", "inets")) copyApp(app, true, erl);
    List<String> chosen = erl.subList(0, Math.min(n, erl.size()));
    List<InspectionProfileEntry> tools = new ArrayList<>();
    for (LocalInspectionEP ep : LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.") || ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new InspectionProfileEntry[0]));
    report("mode=" + mode + " before");
    List<VirtualFile> opened = new ArrayList<>();
    long t0 = System.currentTimeMillis();
    int builds0 = ErlangMacroLayer.EXPANSIONS_BUILT.get();
    for (String path : chosen) {
      PsiFile f = myFixture.getPsiManager().findFile(myFixture.findFileInTempDir(path));
      assertNotNull(path, f);
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
      opened.add(f.getVirtualFile());
    }
    out("mode=" + mode + " files=" + chosen.size() + " highlighted in " + (System.currentTimeMillis() - t0) + " ms, builds=" +
        (ErlangMacroLayer.EXPANSIONS_BUILT.get() - builds0) + " open editors=" + FileEditorManager.getInstance(getProject()).getOpenFiles().length);
    report("mode=" + mode + " open");
    for (VirtualFile v : opened) FileEditorManager.getInstance(getProject()).closeFile(v);
    report("mode=" + mode + " closed");
  }
}
