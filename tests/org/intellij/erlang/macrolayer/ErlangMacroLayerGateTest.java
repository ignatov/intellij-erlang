package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.DebugUtil;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.stubs.StubTree;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.usageView.UsageViewTypeLocation;
import com.intellij.util.indexing.FileBasedIndex;
import com.intellij.util.indexing.ID;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/** Spike 3: the gate (G1 to G4) and the invariants (I1, I2) of the macro expansion layer. */
public class ErlangMacroLayerGateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R29/", ErlangSdkRelease.V_29_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    // spike 4: these gate checks resolve on the test (UI) thread; let it build, as round three did
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private static void out(String s) {
    System.out.println("LAYER-GATE " + s);
  }

  private PsiReference refAt(PsiFile file, String marker, int shift) {
    int at = file.getText().indexOf(marker);
    assertTrue(marker, at >= 0);
    PsiReference ref = file.findReferenceAt(at + shift);
    assertNotNull("reference at " + marker, ref);
    return ref;
  }

  private static String where(PsiElement e) {
    if (e == null) return "null";
    PsiFile f = e.getContainingFile();
    return e.getClass().getSimpleName() + " '" + e.getText() + "' in " + (f == null ? "?" : f.getName()) + "@" +
           e.getTextRange().getStartOffset() + " physical=" + e.isPhysical() + " copy=" + (f != null && f.getUserData(ErlangMacroLayer.COPY_OF) != null);
  }

  // ---------------------------------------------------------------------------------------------- G1

  public void testG1WholeFileCopyHasModuleContext() {
    myFixture.addFileToProject("inc.hrl", "-record(hr, {a}).\nhfun() -> ok.\n");
    myFixture.addFileToProject("other.erl", "-module(other).\n-export([ext/0]).\next() -> ok.\n");
    PsiFile file = myFixture.configureByText("g1.erl", """
      -module(g1).
      -include("inc.hrl").
      -export([f/0]).
      -record(r, {f}).
      -define(BODY(X), X, local(), #r{f = 1}, #hr{a = 2}, hfun(), other:ext()).
      local() -> ok.
      f() -> ?BODY(ok).
      """);
    ErlangMacroLayer.Copy copy = ErlangMacroLayer.getCopy(file);
    assertNotNull(copy);
    assertNotNull(copy.copyFile);
    String text = copy.copyFile.getText();
    out("G1 copy form: " + text.substring(text.indexOf("f() ->")));
    String[][] checks = {{"local()", "0", "ErlangFunction", "g1.erl"}, {"#r{", "1", "ErlangRecordDefinition", "g1.erl"},
      {"#hr{", "1", "ErlangRecordDefinition", "inc.hrl"}, {"hfun()", "0", "ErlangFunction", "inc.hrl"},
      {"ext()", "0", "ErlangFunction", "other.erl"}};
    int from = text.indexOf("f() ->");
    for (String[] c : checks) {
      int at = text.indexOf(c[0], from) + Integer.parseInt(c[1]);
      PsiReference ref = copy.copyFile.findReferenceAt(at);
      assertNotNull(c[0], ref);
      PsiElement target = ref.resolve();
      out("G1 " + c[0] + " -> " + where(target));
      assertNotNull(c[0], target);
      assertTrue(c[0] + " " + target.getClass(), target.getClass().getSimpleName().startsWith(c[2]));
      assertEquals(c[0], c[3], target.getContainingFile().getName());
    }
  }

  // ---------------------------------------------------------------------------------------------- G2

  public void testG2ValueGoesToTheDefine() {
    PsiFile file = myFixture.configureByText("pat.erl", """
      -module(pat).
      -export([f/1]).
      -define(PAT, {ok, Value}).
      f(?PAT) -> Value;
      f(_) -> none.
      """);
    PsiReference ref = refAt(file, "Value;", 1);
    PsiElement target = ref.resolve();
    out("G2 Value -> " + where(target));
    assertNotNull(target);
    assertSame(file, target.getContainingFile());
    assertEquals("Value", target.getText());
    assertEquals(file.getText().indexOf("Value}"), target.getTextRange().getStartOffset());
    myFixture.getEditor().getCaretModel().moveToOffset(file.getText().indexOf("Value;") + 1);
    PsiElement goTo = GotoDeclarationAction.findTargetElement(getProject(), myFixture.getEditor(), myFixture.getCaretOffset());
    out("G2 Value goto -> " + where(goTo));
    assertNotNull(goTo);
    assertEquals(file.getText().indexOf("Value}"), goTo.getTextOffset());
  }

  public void testG2HelloGoesToTheArgument() {
    PsiFile file = myFixture.configureByText("def.erl", """
      -module(def).
      -export([f/0, hello/0]).
      -define(DEF(N), N() -> ok).
      ?DEF(hello).
      f() -> hello().
      """);
    int arg = file.getText().indexOf("?DEF(hello)") + 5;
    PsiElement target = refAt(file, "hello()", 1).resolve();
    out("G2 hello() -> " + where(target));
    assertNotNull(target);
    assertEquals("hello", target.getText());
    assertEquals(arg, target.getTextRange().getStartOffset());
    PsiElement export = refAt(file, "hello/0", 1).resolve();
    out("G2 hello/0 -> " + where(export));
    assertNotNull(export);
    assertEquals(arg, export.getTextRange().getStartOffset());
  }

  // ---------------------------------------------------------------------------------------------- G3

  public void testG3FreshInsideOneWriteCommand() {
    PsiFile file = myFixture.configureByText("fresh.erl", """
      -module(fresh).
      -export([f/1]).
      -define(PAT, {ok, Value}).
      f(?PAT) -> Value.
      """);
    List<String> log = new ArrayList<>();
    WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      PsiElement before = refAt(file, "Value.", 1).resolve();
      log.add("before: " + where(before));
      ErlangQVar bodyVar = PsiTreeUtil.getParentOfType(file.findElementAt(file.getText().indexOf("Value}")), ErlangQVar.class);
      assertNotNull(bodyVar);
      bodyVar.replace(ErlangElementFactory.createQVarFromText(getProject(), "Other"));
      PsiElement afterBodyEdit = refAt(file, "Value.", 1).resolve();
      log.add("after the -define body became Other: " + where(afterBodyEdit));
      ErlangQVar useVar = PsiTreeUtil.getParentOfType(file.findElementAt(file.getText().indexOf("Value.")), ErlangQVar.class);
      useVar.replace(ErlangElementFactory.createQVarFromText(getProject(), "Other"));
      PsiElement afterUseEdit = refAt(file, "Other.", 1).resolve();
      log.add("after the use became Other: " + where(afterUseEdit));
      assertNotNull(before);
      assertNull(afterBodyEdit);
      assertNotNull(afterUseEdit);
      assertEquals(file.getText().indexOf("Other}"), afterUseEdit.getTextRange().getStartOffset());
    });
    for (String s : log) out("G3 " + s);
  }

  // ---------------------------------------------------------------------------------------------- G4

  public void testG4TypingNeverBuildsAnExpansion() {
    // round 5: highlighting runs the Erlang inspections, as in the IDE (the name of a macro call no longer asks the layer)
    LayerTestSupport.enableAll(myFixture);
    myFixture.configureByText("type.erl", """
      -module(type).
      -export([f/1]).
      -define(PAT, {ok, Value}).
      -define(DEF(N), N() -> ok).
      ?DEF(hello).
      f(?PAT) -> <caret>Value.
      """);
    int built = ErlangMacroLayer.EXPANSIONS_BUILT.get();
    for (char c : "X = 1, ".toCharArray()) {
      myFixture.type(c);
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    }
    // round 5: the new form goes after f/1, so that f/1 keeps its use of Value, which highlighting asks the layer for (the
    // name of the macro call ?DEF(more), which was the only question when the form split f/1, no longer asks)
    myFixture.getEditor().getCaretModel().moveToOffset(myFixture.getEditor().getDocument().getTextLength());
    myFixture.type("\n?DEF(more).\n");
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    PsiFile file = myFixture.getFile();
    file.getNode().getFirstChildNode();
    ReadAction.run(() -> ((PsiFileImpl)file).calcStubTree());
    int afterTyping = ErlangMacroLayer.EXPANSIONS_BUILT.get();
    out("G4 builds during typing, reparse and stub build: " + (afterTyping - built));
    assertEquals(built, afterTyping);
    myFixture.doHighlighting();
    int afterHighlighting = ErlangMacroLayer.EXPANSIONS_BUILT.get();
    out("G4 builds during highlighting: " + (afterHighlighting - afterTyping));
    assertTrue(afterHighlighting > afterTyping);
  }

  // ---------------------------------------------------------------------------------------------- I1, I2

  private static List<Path> fixtures() throws Exception {
    List<Path> result = new ArrayList<>();
    for (String dir : List.of("testData/macroSpikeProbes", "testData/verifier2")) {
      try (var s = Files.list(Paths.get(dir))) {
        s.filter(p -> p.toString().endsWith(".erl")).sorted().forEach(result::add);
      }
    }
    String list = System.getenv("MACRO_LAYER_I1_FILES");
    if (list != null && !list.isEmpty()) {
      for (String line : Files.readAllLines(Paths.get(list))) if (!line.isBlank()) result.add(Paths.get(line.trim()));
    }
    return result;
  }

  private static String read(Path p) throws Exception {
    byte[] bytes = Files.readAllBytes(p);
    String text = new String(bytes, StandardCharsets.UTF_8);
    if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
    return text.replace("\r\n", "\n").replace('\r', '\n');
  }

  @SuppressWarnings("unchecked")
  private static ID<String, Void> id(String name) {
    return ID.findByName(name);
  }

  private String snapshot(PsiFile file) {
    return ReadAction.compute(() -> {
      StringBuilder sb = new StringBuilder();
      sb.append(DebugUtil.psiToString(file, true, false));
      StubTree stub = ((PsiFileImpl)file).calcStubTree();
      sb.append("\n--- stub\n").append(DebugUtil.stubTreeToString(stub.getRoot()));
      VirtualFile vf = file.getVirtualFile();
      FileBasedIndex index = FileBasedIndex.getInstance();
      sb.append("\n--- module index ").append(new TreeMap<>(index.getFileData(id("ErlangModuleIndex"), vf, getProject())).keySet());
      sb.append("\n--- atom index ").append(new TreeSet<>(index.getFileData(id("erlang.atom.index"), vf, getProject()).keySet()));
      return sb.toString();
    });
  }

  public void testI1MainTreeStubsAndIndicesUnchanged() throws Exception {
    int n = 0, expanded = 0;
    for (Path p : fixtures()) {
      String text = read(p);
      String name = "i1_" + (n++) + "_" + p.getFileName();
      com.intellij.openapi.Disposable off = Disposer.newDisposable();
      ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, off);
      PsiFile file = myFixture.addFileToProject("i1/" + name, text);
      String master = snapshot(file);
      Disposer.dispose(off);
      myFixture.configureFromExistingVirtualFile(file.getVirtualFile());
      myFixture.doHighlighting();
      ErlangMacroLayer.Copy copy = ReadAction.compute(() -> ErlangMacroLayer.getCopy(file));
      if (copy != null && copy.copyFile != null) expanded++;
      String layer = snapshot(file);
      assertEquals("I1 " + p, master, layer);
    }
    out("I1 files=" + n + " with an expanded copy=" + expanded + " identical=" + n);
  }

  public void testI2NothingFromTheCopyLeaves() throws Exception {
    int refs = 0, resolved = 0, usages = 0, n = 0;
    int violations = ErlangMacroLayer.GUARD_VIOLATIONS.get();
    for (Path p : fixtures()) {
      if (!p.toString().startsWith("testData")) continue;
      PsiFile file = myFixture.addFileToProject("i2/" + (n++) + "_" + p.getFileName(), read(p));
      myFixture.configureFromExistingVirtualFile(file.getVirtualFile());
      myFixture.doHighlighting();
      for (PsiElement e : PsiTreeUtil.collectElements(file, x -> true)) {
        for (PsiReference ref : e.getReferences()) {
          refs++;
          PsiElement target = ref.resolve();
          if (target == null) continue;
          resolved++;
          assertNoCopy("resolve of " + ref.getCanonicalText(), target);
          if (ref instanceof PsiPolyVariantReference poly) {
            for (ResolveResult rr : poly.multiResolve(false)) assertNoCopy("multiResolve", rr.getElement());
          }
        }
        if (e instanceof PsiNamedElement && (e instanceof ErlangQVar || e instanceof ErlangFunction || e instanceof ErlangRecordDefinition)) {
          for (PsiReference u : ReferencesSearch.search(e).findAll()) {
            usages++;
            assertNoCopy("usage", u.getElement());
          }
        }
      }
    }
    out("I2 files=" + n + " references=" + refs + " resolved=" + resolved + " usages=" + usages +
        " guard violations=" + (ErlangMacroLayer.GUARD_VIOLATIONS.get() - violations));
    assertEquals(violations, ErlangMacroLayer.GUARD_VIOLATIONS.get());
  }

  private static void assertNoCopy(String what, @NotNull PsiElement e) {
    PsiFile f = e.getContainingFile();
    assertTrue(what + ": " + where(e), e.isPhysical() && f != null && f.getUserData(ErlangMacroLayer.COPY_OF) == null);
  }

  // ---------------------------------------------------------------------------------------------- entry points

  public void testModeFollowsTheSwitch() {
    assertEquals(ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.getMode());
    Disposable off = Disposer.newDisposable();
    try {
      ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.OFF, off);
      assertEquals(ErlangMacroLayer.Mode.OFF, ErlangMacroLayer.getMode());
    }
    finally {
      Disposer.dispose(off);
    }
    assertEquals(ErlangMacroLayer.Mode.LAYER, ErlangMacroLayer.getMode());
  }

  public void testQuietMillisForTests() {
    ErlangMacroLayer.setQuietMillisForTests(321, getTestRootDisposable());
    assertEquals(321, ErlangMacroLayer.quietMillis());
  }

  public void testMacroUsesInText() {
    assertTrue(ErlangMacroLayer.usesKnownMacro("f() -> ?'my macro'(1).", Set.of("my macro")));
    assertTrue(ErlangMacroLayer.usesKnownMacro("f() -> ? M.", Set.of("M")));
    // ??X stringifies the argument X of a macro: it is not a use of the macro X
    assertFalse(ErlangMacroLayer.usesKnownMacro("-define(S(X), ??X).", Set.of("X")));
    assertFalse(ErlangMacroLayer.usesKnownMacro("f() -> ?'other'.", Set.of("my macro")));
    assertEquals(Set.of("q q", "B"), ErlangMacroLayer.macroNamesIn("??A ?'q q' ?B ?C ?", Set.of("A", "q q", "B")));
    assertEquals(Set.of(), ErlangMacroLayer.macroNamesIn("??A ? ", Set.of("A")));
  }

  public void testExplicitTableAndFormUse() throws Exception {
    PsiFile file = myFixture.configureByText("tbl.erl", """
      -module(tbl).
      -export([f/0, g/0]).
      -define(K, 1).
      f() -> ?K.
      g() -> ok.
      """);
    ErlangFunction f = ((ErlangFile)file).getFunction("f", 0);
    ErlangFunction g = ((ErlangFile)file).getFunction("g", 0);
    assertNotNull(f);
    assertNotNull(g);
    List<Object> r = LayerTestSupport.inBackground(() -> {
      ErlangMacroLayer.Table t = ErlangMacroLayer.table((ErlangFile)file, true);
      assertNotNull(t);
      return List.of(t.refusal == null, t.names.contains("K"), ErlangMacroLayer.formUsesMacroForTests(t, f),
                     ErlangMacroLayer.formUsesMacroForTests(t, g), ErlangMacroLayer.formUsesMacroForTests(t, null));
    });
    assertEquals(List.of(true, true, true, false, false), r);
  }

  public void testFormUseWithMissingIncludeAtEndOfFile() throws Exception {
    PsiFile file = myFixture.configureByText("mi.erl", """
      -module(mi).
      -include("missing_header.hrl").
      -define(K, 1).
      g() -> ?FROM_HEADER(1).
      f() -> ?K
      """);
    ErlangFunction f = ((ErlangFile)file).getFunction("f", 0);
    ErlangFunction g = ((ErlangFile)file).getFunction("g", 0);
    assertNotNull(f);
    assertNotNull(g);
    List<Object> r = LayerTestSupport.inBackground(() -> {
      ErlangMacroLayer.Table t = ErlangMacroLayer.table((ErlangFile)file, true);
      assertNotNull(t);
      return List.of(t.refusal == null, t.missingInclude, ErlangMacroLayer.formUsesMacroForTests(t, ErlangMacroLayer.topForm(f)),
                     ErlangMacroLayer.formUsesMacroForTests(t, ErlangMacroLayer.topForm(g)));
    });
    // the last form uses a macro the file sees and runs to the end of the file; the one before uses a macro that may
    // come from the missing header, which leaves it to master
    assertEquals(List.of(true, true, true, false), r);
  }

  public void testExitGuard() throws Exception {
    PsiFile file = myFixture.configureByText("ex.erl", """
      -module(ex).
      -export([f/0]).
      -define(K, local()).
      local() -> ok.
      f() -> ?K.
      """);
    ErlangMacroLayer.Copy copy = LayerTestSupport.inBackground(() -> ErlangMacroLayer.getCopy(file));
    assertNotNull(copy);
    assertNotNull(copy.copyFile);
    PsiElement real = ((ErlangFile)file).getFunction("local", 0);
    assertNotNull(real);
    PsiElement inCopy = copy.copyFile.getFunction("local", 0);
    assertNotNull(inCopy);
    assertNull(ErlangMacroLayer.exit(null, file));
    assertSame(real, ErlangMacroLayer.exit(real, file));
    int violations = ErlangMacroLayer.GUARD_VIOLATIONS.get();
    try {
      ErlangMacroLayer.exit(inCopy, file);
      fail("an element of the copy left the layer");
    }
    catch (AssertionError e) {
      assertTrue(e.getMessage(), e.getMessage().startsWith("macro layer: an element of an expanded copy left the layer"));
    }
    assertEquals(violations + 1, ErlangMacroLayer.GUARD_VIOLATIONS.get());
  }

  public void testIsMacroParameter() {
    PsiFile file = myFixture.configureByText("par.erl", """
      -module(par).
      -define(M(A, B), {A, B, C}).
      -define(N, C).
      """);
    List<ErlangMacrosDefinition> defs = new ArrayList<>(PsiTreeUtil.findChildrenOfType(file, ErlangMacrosDefinition.class));
    assertEquals(2, defs.size());
    assertTrue(ErlangMacroLayer.isMacroParameter(defs.get(0), "A"));
    assertTrue(ErlangMacroLayer.isMacroParameter(defs.get(0), "B"));
    assertFalse(ErlangMacroLayer.isMacroParameter(defs.get(0), "C"));
    assertFalse(ErlangMacroLayer.isMacroParameter(defs.get(1), "C"));
  }

  public void testNoLayerDescriptionOfAPlainName() {
    PsiFile file = myFixture.configureByText("desc.erl", """
      -module(desc).
      -export([f/0]).
      -define(K, 1).
      f() -> {plain, ?K}.
      """);
    PsiElement leaf = file.findElementAt(file.getText().indexOf("plain"));
    assertNotNull(leaf);
    assertEquals("plain", leaf.getText());
    assertFalse(ErlangMacroLayer.isNameMadeByMacro(leaf));
    assertNull(ErlangMacroLayer.describeLayerTarget(leaf, UsageViewTypeLocation.INSTANCE));
  }

  public void testRenameTargetOfMadeNameIsUnknownWithoutCopyOnUiThread() throws Exception {
    ErlangMacroLayer.setEdtBuildForTests(false, getTestRootDisposable());
    PsiFile file = myFixture.configureByText("rt.erl", """
      -module(rt).
      -export([made/0]).
      -define(MK(N), N() -> ok).
      ?MK(made).
      """);
    int at = file.getText().indexOf("made).");
    ErlangQAtom q = PsiTreeUtil.getParentOfType(file.findElementAt(at), ErlangQAtom.class);
    assertNotNull(q);
    assertTrue(ErlangMacroLayer.isNameMadeByMacro(PsiTreeUtil.firstChild(q)));
    // on the UI thread no copy is built, so the layer cannot tell yet what to rename
    assertNull(ErlangMacroLayer.renameTargetOfMacroToken(q));
    assertNotNull(LayerTestSupport.inBackground(() -> ErlangMacroLayer.getCopy(file)));
    // with the copy there, the name's own token
    PsiElement target = ErlangMacroLayer.renameTargetOfMacroToken(q);
    assertNotNull(target);
    assertNotSame(q, target);
    assertEquals("made", target.getText());
    assertEquals(at, target.getTextRange().getStartOffset());
  }

  private static PsiErrorElement lastError(PsiFile file) {
    List<PsiErrorElement> errors = new ArrayList<>(PsiTreeUtil.findChildrenOfType(file, PsiErrorElement.class));
    assertFalse(errors.isEmpty());
    return errors.get(errors.size() - 1);
  }

  public void testSyntaxErrorAtEndOfExpandedFormIsShown() throws Exception {
    // erlc: "syntax error before:" at the end of the file, the last form has no dot
    PsiFile file = myFixture.configureByText("eof.erl", "-module(eof).\n-export([f/0]).\n-define(X, 1).\nf() -> ?X\n");
    PsiErrorElement error = lastError(file);
    assertEquals(file.getText().indexOf("?X\n") + 2, error.getTextRange().getStartOffset());
    ErlangMacroLayer.Copy copy = LayerTestSupport.inBackground(() -> ErlangMacroLayer.getCopy(file));
    assertNotNull(copy);
    assertNotNull(copy.copyFile);
    ErlangMacroExpander.Form last = copy.expansion.forms.get(copy.expansion.forms.size() - 1);
    assertEquals(ErlangMacroExpander.FormStatus.EXPANDED, last.status);
    assertTrue(copy.isBroken(last));
    assertFalse(copy.isClean(last));
    assertFalse(LayerTestSupport.inBackground(() -> ErlangMacroLayer.isErrorExplainedByExpansion(error)));
  }
}
