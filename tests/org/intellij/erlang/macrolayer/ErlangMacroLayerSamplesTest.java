package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * D4: the owner's samples project (set {@code S6_SAMPLES} to its folder; nothing runs without it). Every .erl and .hrl
 * of {@code src/}, {@code include/} and {@code libs/} is copied into the project, OTP 29.1 is the SDK, every Erlang
 * inspection is on. A file with a {@code %% erlc: L:C: message} comment is wrong: the plugin must show an error on
 * that line. Every other file is valid: the plugin must show no error. Prints one line per file; fails on a mismatch
 * unless the file is listed in {@code S6_SAMPLES_KNOWN} (comma separated).
 */
public class ErlangMacroLayerSamplesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang";
  private static final Pattern ERLC = Pattern.compile("%%\\s*erlc:\\s*(?:\\S+?\\.erl:)?(\\d+):(\\d+):\\s*(.*)");

  public ErlangMacroLayerSamplesTest() {
    super(true);
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk(System.getenv("S6_SAMPLES") != null ? OTP : "testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  public void testSamples() throws Exception {
    String root = System.getenv("S6_SAMPLES");
    if (root == null) return;
    Set<String> known = new HashSet<>(Arrays.asList(Objects.requireNonNullElse(System.getenv("S6_SAMPLES_KNOWN"), "").split(",")));
    Path base = Paths.get(root);
    List<String> erl = new ArrayList<>();
    for (String dir : List.of("src", "include", "libs")) {
      try (Stream<Path> s = Files.walk(base.resolve(dir))) {
        for (Path p : s.filter(Files::isRegularFile).sorted().toList()) {
          String rel = base.relativize(p).toString();
          if (!rel.endsWith(".erl") && !rel.endsWith(".hrl") && !rel.endsWith(".app.src")) continue;
          myFixture.addFileToProject(rel, LayerTestSupport.read(p));
          if (rel.endsWith(".erl") && !rel.equals("src/main.erl")) erl.add(rel);
        }
      }
    }
    LayerTestSupport.enableAll(myFixture);
    List<String> mismatches = new ArrayList<>();
    for (String rel : erl) {
      PsiFile f = myFixture.getPsiManager().findFile(myFixture.findFileInTempDir(rel));
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      String text = f.getText();
      List<String> errors = new ArrayList<>();
      Set<Integer> errorLines = new TreeSet<>();
      for (HighlightInfo h : myFixture.doHighlighting()) {
        if (h.getSeverity() != HighlightSeverity.ERROR) continue;
        int line = StringUtil.offsetToLineNumber(text, h.getStartOffset()) + 1;
        errorLines.add(line);
        errors.add(line + ": " + h.getDescription());
      }
      List<Integer> expected = new ArrayList<>();
      Matcher m = ERLC.matcher(text);
      while (m.find()) expected.add(Integer.parseInt(m.group(1)));
      String verdict;
      if (expected.isEmpty()) verdict = errors.isEmpty() ? "OK" : "FALSE ERROR";
      else verdict = expected.stream().allMatch(errorLines::contains) ? "OK" : "MISSED (erlc line " + expected + ")";
      System.out.println("S6-SAMPLE " + verdict + " " + rel + " " + errors);
      if (!verdict.equals("OK") && !known.contains(rel)) mismatches.add(rel + ": " + verdict + " " + errors);
    }
    assertEquals(List.of(), mismatches);
  }
}
