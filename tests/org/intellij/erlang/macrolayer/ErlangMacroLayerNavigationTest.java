package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 5: navigation into and out of macros, on testData/macroLayer/nav (erlc compiles s.erl, i.erl and i2.erl).
 * s.erl uses same-file macros; i.erl and i2.erl use the macros of h.hrl. Navigation is computed on a pooled thread in
 * a read action, as the IDE does; every target is reported as file:line:text of its navigation element.
 */
public class ErlangMacroLayerNavigationTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final Map<String, PsiFile> myFiles = new LinkedHashMap<>();

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
    ErlangMacroLayerLog.enableForTests(true);
    ErlangMacroLayerLog.reset();
    for (String n : List.of("h.hrl", "i.erl", "i2.erl", "s.erl")) {
      myFiles.put(n, myFixture.addFileToProject("nav/" + n, read(Paths.get("testData/macroLayer/nav", n))));
    }
    for (PsiFile f : myFiles.values()) {
      myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
      myFixture.doHighlighting();
    }
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      assertEquals("a copy was built on the UI thread", List.of(), uiThreadBuilds());
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      ErlangMacroLayerLog.enableForTests(false);
      super.tearDown();
    }
  }

  static String where(PsiElement t) {
    if (t == null) return "-";
    PsiElement n = t.getNavigationElement();
    PsiFile f = n.getContainingFile();
    if (f == null) return "?" + t;
    assertNull("an element of a copy left the layer: " + n, f.getUserData(ErlangMacroLayer.COPY_OF));
    assertTrue("not physical: " + n, f.isPhysical());
    int line = StringUtil.offsetToLineNumber(f.getText(), n.getTextOffset()) + 1;
    String text = n.getText();
    if (text.length() > 24) text = text.substring(0, 24) + "..";
    return f.getName() + ":" + line + ":" + text.replace('\n', ' ');
  }

  /** Go to declaration at marker+shift: the references at the offset and the go-to handlers. */
  private String navigate(String file, String marker, int shift) throws Exception {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    int offset = at + shift;
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    Editor editor = myFixture.getEditor();
    return inBackground(() -> {
      Set<String> r = new TreeSet<>();
      PsiReference ref = f.findReferenceAt(offset);
      if (ref instanceof PsiPolyVariantReference pr) for (ResolveResult rr : pr.multiResolve(false)) r.add(where(rr.getElement()));
      if (ref != null) r.add(where(ref.resolve()));
      PsiElement leaf = f.findElementAt(offset);
      for (GotoDeclarationHandler h : GotoDeclarationHandler.EP_NAME.getExtensionList()) {
        PsiElement[] t = h.getGotoDeclarationTargets(leaf, offset, editor);
        if (t != null) for (PsiElement e : t) r.add(where(e));
      }
      r.remove("-");
      return r.isEmpty() ? "NOTHING" : String.join(" ", r);
    });
  }

  // ------------------------------------------------------------------------------------------------ (c) made by a macro

  public void testRecordMadeByMacro() throws Exception {
    assertEquals("s.erl:11:mr", navigate("s.erl", "#mr{", 1));
    assertEquals("h.hrl:9:hmr", navigate("i.erl", "#hmr{", 1));
  }

  public void testRecordFieldMadeByMacro() throws Exception {
    assertEquals("s.erl:11:mf", navigate("s.erl", "mf = 1", 0));
    assertEquals("h.hrl:9:hmf", navigate("i.erl", "hmf = 1", 0));
  }

  public void testTypeMadeByMacro() throws Exception {
    assertEquals("s.erl:12:mt", navigate("s.erl", "mt().\n", 0));
    assertEquals("h.hrl:10:hmt", navigate("i.erl", "hmt().\n", 0));
  }

  // ------------------------------------------------------------------------------------------------ (d) inside a body

  /**
   * A call in the body of a header macro names a function of the includer. Each use resolves it in its own file;
   * the two includers define it differently, so go-to offers both.
   */
  public void testCallInHeaderMacroBodyGoesToEveryIncluder() throws Exception {
    assertEquals("i.erl:8:h_target(X) -> X. i2.erl:8:h_target(X) -> X.", navigate("h.hrl", "h_target(X)).", 0));
  }

  /** The body of H_FRAG is not an expression (a clause separator and a clause): the uses' copies resolve it. */
  public void testCallInFragmentHeaderMacroBody() throws Exception {
    assertEquals("i.erl:9:h_frag_target() -> i. i2.erl:9:h_frag_target() -> i2.", navigate("h.hrl", "h_frag_target())", 0));
  }

  /** Master already resolves names in same-file bodies; the layer adds nothing there. */
  public void testSameFileBodiesKeepMastersAnswer() throws Exception {
    assertEquals("s.erl:19:target(X) -> X.", navigate("s.erl", "target(X)).", 0));
    assertEquals("s.erl:20:frag_target() -> ok.", navigate("s.erl", "frag_target())", 0));
    assertEquals("h.hrl:1:-record(hr, {hf}).", navigate("h.hrl", "#hr{hf = V}", 1));
  }

  // ------------------------------------------------------------------------------------------------ (e) find usages

  /** The element find usages starts from: the target of the reference at marker+shift, or the named element there. */
  private PsiElement startAt(String file, String marker, int shift) throws Exception {
    PsiFile f = myFiles.get(file);
    int at = f.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    return inBackground(() -> {
      PsiReference ref = f.findReferenceAt(at + shift);
      PsiElement t = ref == null ? null : ref.resolve();
      if (t != null) return t;
      return com.intellij.psi.util.PsiTreeUtil.getParentOfType(f.findElementAt(at + shift), PsiNamedElement.class, false);
    });
  }

  /** Find usages as the IDE runs it: the platform's handler, the search on a pooled thread. */
  private String usages(PsiElement t) throws Exception {
    com.intellij.find.findUsages.FindUsagesManager manager =
      ((com.intellij.find.impl.FindManagerImpl)com.intellij.find.FindManager.getInstance(getProject())).getFindUsagesManager();
    com.intellij.find.findUsages.FindUsagesHandler handler =
      manager.getFindUsagesHandler(t, com.intellij.find.findUsages.FindUsagesHandlerFactory.OperationMode.USAGES_WITH_DEFAULT_OPTIONS);
    if (handler == null) return "NO HANDLER";
    com.intellij.find.findUsages.FindUsagesOptions options = handler.getFindUsagesOptions(null);
    List<com.intellij.usageView.UsageInfo> found = waitForTask(com.intellij.openapi.application.ApplicationManager.getApplication().executeOnPooledThread(() -> {
      com.intellij.util.CommonProcessors.CollectProcessor<com.intellij.usageView.UsageInfo> p =
        new com.intellij.util.CommonProcessors.CollectProcessor<>(Collections.synchronizedList(new ArrayList<>()));
      for (PsiElement e : handler.getPrimaryElements()) handler.processElementUsages(e, p, options);
      return new ArrayList<>(p.getResults());
    }));
    return inBackground(() -> {
      Set<String> r = new TreeSet<>();
      for (com.intellij.usageView.UsageInfo u : found) {
        PsiElement e = u.getElement();
        if (e == null) continue;
        PsiFile f = e.getContainingFile();
        com.intellij.openapi.util.TextRange range = u.getRangeInElement();
        int start = e.getTextRange().getStartOffset() + (range == null ? 0 : range.getStartOffset());
        int end = range == null ? e.getTextRange().getEndOffset() : e.getTextRange().getStartOffset() + range.getEndOffset();
        r.add(f.getName() + ":" + (StringUtil.offsetToLineNumber(f.getText(), start) + 1) + ":" + f.getText().substring(start, end));
      }
      return r.isEmpty() ? "NONE" : String.join(" ", r);
    });
  }

  public void testUsagesOfAFunctionCalledInAHeaderMacroBody() throws Exception {
    String u = usages(startAt("i.erl", "h_target(X) -> X.", 0));
    System.out.println("LAYER-NAV usages h_target: " + u);
    assertEquals("h.hrl:3:h_target i.erl:14:h_target", u);
    String f = usages(startAt("i.erl", "h_frag_target() -> i.", 0));
    System.out.println("LAYER-NAV usages h_frag_target: " + f);
    assertEquals("h.hrl:11:h_frag_target", f);
  }

  public void testUsagesOfNamesMadeByMacros() throws Exception {
    String[][] cases = {
      {"s.erl", "#mr{", "1", "s.erl:23:mr"},
      {"i.erl", "#hmr{", "1", "i.erl:12:hmr i2.erl:12:hmr"},
      {"s.erl", "mf = 1", "0", "s.erl:23:mf"},
      {"i.erl", "hmf = 1", "0", "i.erl:12:hmf i2.erl:12:hmf"},
      {"s.erl", "mt().\n", "0", "s.erl:29:mt"},
      {"i.erl", "hmt().\n", "0", "i.erl:18:hmt i2.erl:18:hmt"},
      {"i.erl", "hfixed()}", "0", "i.erl:13:hfixed i.erl:3:hfixed i2.erl:13:hfixed i2.erl:3:hfixed"},
      {"s.erl", "fixed()}", "0", "s.erl:24:fixed s.erl:2:fixed"},
    };
    StringBuilder failures = new StringBuilder();
    for (String[] c : cases) {
      String u = usages(startAt(c[0], c[1], Integer.parseInt(c[2])));
      System.out.println("LAYER-NAV usages " + c[1].trim() + ": " + u);
      if (!u.equals(c[3])) failures.append(c[1].trim()).append(": expected ").append(c[3]).append(" got ").append(u).append('\n');
    }
    assertEquals("", failures.toString());
  }

  /** #1486: the module of the issue; usages of {@code hello()} made by {@code ?DEF(hello).} include its export. */
  public void testUsagesOfFunctionNamedByMacroArgumentIncludeExport() throws Exception {
    PsiFile greet = myFixture.addFileToProject("nav/greet.erl", read(Paths.get("testData/macroLayer/nav", "greet.erl")));
    myFiles.put("greet.erl", greet);
    myFixture.configureFromExistingVirtualFile(greet.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("greet.erl:11:hello greet.erl:2:hello", usages(startAt("greet.erl", "hello(),", 0)));
  }

  /** #1486: the export entry {@code hello/0} goes to {@code hello} in {@code ?DEF(hello).}, not to the call. */
  public void testExportOfFunctionNamedByMacroArgumentAfterFragmentMacro() throws Exception {
    PsiFile greet = myFixture.addFileToProject("nav/greet.erl", read(Paths.get("testData/macroLayer/nav", "greet.erl")));
    myFiles.put("greet.erl", greet);
    myFixture.configureFromExistingVirtualFile(greet.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("greet.erl:18:hello", navigate("greet.erl", "hello/0", 0));
  }

  /** #1604: a call in a later clause, after a fragment macro in an earlier one, goes to {@code hello} in {@code ?DEF(hello).}. */
  public void testCallInLaterClauseAfterFragmentMacro() throws Exception {
    PsiFile greet = myFixture.addFileToProject("nav/greet2.erl", read(Paths.get("testData/macroLayer/nav", "greet2.erl")));
    myFiles.put("greet2.erl", greet);
    myFixture.configureFromExistingVirtualFile(greet.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("greet2.erl:15:hello", navigate("greet2.erl", "hello().", 0));
  }

  /** #1604: usages of {@code hello/0} made by {@code ?DEF(hello).} include the call in the later clause. */
  public void testUsagesOfFunctionNamedByMacroArgumentIncludeCallInLaterClause() throws Exception {
    PsiFile greet = myFixture.addFileToProject("nav/greet2.erl", read(Paths.get("testData/macroLayer/nav", "greet2.erl")));
    myFiles.put("greet2.erl", greet);
    myFixture.configureFromExistingVirtualFile(greet.getVirtualFile());
    myFixture.doHighlighting();
    assertEquals("greet2.erl:11:hello greet2.erl:2:hello", usages(startAt("greet2.erl", "hello/0", 0)));
  }
}
