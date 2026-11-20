package org.intellij.erlang.spikeprobe;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.command.undo.UndoManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.ex.IdeDocumentHistory;
import com.intellij.openapi.fileEditor.impl.text.TextEditorProvider;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.safeDelete.SafeDeleteHandler;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.usageView.UsageInfo;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.refactoring.ErlangInlineVariableHandler;
import org.intellij.erlang.refactoring.introduce.ErlangIntroduceVariableHandler;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;

/** Verifier 2: new fixtures (testData/verifier2). Writes PROBE_OUT/label/v2features/testName.txt; never fails on a finding. */
public class Verifier2ProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final StringBuilder myLog = new StringBuilder();
  private Path myDir;

  @Override protected LightProjectDescriptor getProjectDescriptor() { return descriptor(); }
  @Override protected String getTestDataPath() { return "testData/verifier2/"; }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    String out = env("PROBE_OUT");
    myDir = out == null ? null : Paths.get(out, label());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      if (myDir != null) {
        Files.createDirectories(myDir.resolve("v2features"));
        Files.writeString(myDir.resolve("v2features").resolve(getName() + ".txt"), myLog.toString());
      }
    }
    finally {
      super.tearDown();
    }
  }

  @Override
  protected void runTestRunnable(com.intellij.util.ThrowableRunnable<Throwable> testRunnable) throws Throwable {
    if (env("PROBE_OUT") == null) return;
    super.runTestRunnable(testRunnable);
  }

  private void log(String s) { myLog.append(s).append('\n'); }
  private void logFailures(String what, List<Failure> failures) { for (Failure f : failures) log("  FAILURE " + what + ": " + f.line()); }
  private String fixture(String name) throws Exception { return read(Paths.get(getTestDataPath(), name)); }

  private void open(String name, String marker, int shift) throws Exception {
    String text = fixture(name);
    int at = marker == null ? 0 : text.indexOf(marker);
    if (at < 0) throw new IllegalArgumentException(marker);
    int caret = marker == null ? 0 : at + shift;
    myFixture.configureByText(name, text.substring(0, caret) + "<caret>" + text.substring(caret));
  }

  private int offsetOf(String marker, int shift) {
    int at = myFixture.getEditor().getDocument().getText().indexOf(marker);
    if (at < 0) throw new IllegalArgumentException(marker);
    return at + shift;
  }

  private void saveEdit(String tag) throws Exception {
    Files.createDirectories(myDir.resolve("v2edits"));
    Files.writeString(myDir.resolve("v2edits").resolve(getName() + "-" + tag + ".erl.txt"), myFixture.getEditor().getDocument().getText());
  }

  private void checkStructure(String what) {
    List<Failure> f = guarded(() -> {
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      PsiFile file = myFixture.getFile();
      if (!file.getText().equals(myFixture.getEditor().getDocument().getText())) throw new AssertionError("PSI text != document");
      PsiTestUtil.checkFileStructure(file);
    });
    log("  structure after " + what + ": " + (f.isEmpty() ? "consistent" : "INCONSISTENT"));
    logFailures(what, f);
  }

  private static int refusals() {
    try {
      Class<?> c = Class.forName("org.intellij.erlang.parser.macro.ErlangMacroSubstitutionRenameRefusal");
      return ((java.util.concurrent.atomic.AtomicInteger)c.getField("REFUSALS").get(null)).get();
    }
    catch (Throwable t) {
      return -1;
    }
  }

  private void logDiff(String before, String after) {
    String[] a = before.split("\n", -1), b = after.split("\n", -1);
    if (a.length != b.length) { log("  line count " + a.length + " -> " + b.length); return; }
    for (int i = 0; i < a.length; i++) if (!a[i].equals(b[i])) log("  line " + (i + 1) + ": '" + a[i] + "' -> '" + b[i] + "'");
  }

  /** Rename through the handler (as the user does) and, on a fresh copy, through RenameProcessor directly. */
  private void rename(String file, String marker, int shift, String newName) throws Exception {
    for (String how : new String[]{"handler", "processor"}) {
      open(file, marker, shift);
      String before = myFixture.getEditor().getDocument().getText();
      log("rename [" + how + "] in " + file + " at '" + marker + "'+" + shift + " to " + newName);
      PsiElement[] target = new PsiElement[1];
      guarded(() -> target[0] = myFixture.getElementAtCaret());
      log("  element at caret: " + describe(target[0]));
      int r0 = refusals();
      List<Failure> f = guarded(() -> {
        if (how.equals("handler")) myFixture.renameElementAtCaretUsingHandler(newName);
        else myFixture.renameElementAtCaret(newName);
      });
      logFailures("rename", f);
      String after = myFixture.getEditor().getDocument().getText();
      log("  refusals: " + (refusals() - r0) + "; changed: " + !after.equals(before));
      logDiff(before, after);
      saveEdit(how);
      checkStructure("rename " + how);
    }
  }

  private void navigate(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("navigate in " + file + " at '" + marker + "'+" + shift);
    int offset = myFixture.getCaretOffset();
    List<Failure> f = guarded(() -> {
      PsiReference ref = myFixture.getFile().findReferenceAt(offset);
      log("  reference: " + (ref == null ? "none" : ref.getClass().getSimpleName() + " in " + describe(ref.getElement())));
      if (ref instanceof PsiPolyVariantReference poly) for (ResolveResult rr : poly.multiResolve(false)) log("  multiResolve: " + describe(rr.getElement()));
      else if (ref != null) log("  resolve: " + describe(ref.resolve()));
      PsiElement target = GotoDeclarationAction.findTargetElement(getProject(), myFixture.getEditor(), offset);
      log("  goto target: " + describe(target));
      if (target != null) {
        PsiElement nav = target.getNavigationElement();
        int o = nav.getTextOffset();
        String t = myFixture.getEditor().getDocument().getText();
        log("  navigation element: " + describe(nav) + " textOffset=" + o + " text there: '" + clip(t.substring(Math.max(0, Math.min(o, t.length())), Math.min(t.length(), Math.max(0, o) + 25)), 25) + "'");
      }
    });
    logFailures("navigate", f);
  }

  private void usages(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("find usages in " + file + " at '" + marker + "'+" + shift);
    List<Failure> f = guarded(() -> {
      PsiElement e = myFixture.getElementAtCaret();
      log("  element: " + describe(e));
      Collection<UsageInfo> usages = myFixture.findUsages(e);
      List<String> lines = new ArrayList<>();
      for (UsageInfo u : usages) {
        TextRange r = u.getSegment() == null ? null : TextRange.create(u.getSegment());
        lines.add("  usage: " + describe(u.getElement()) + " segment=" + r + " text='" + (r == null ? "?" : myFixture.getEditor().getDocument().getText().substring(r.getStartOffset(), r.getEndOffset())) + "'");
      }
      Collections.sort(lines);
      log("  usages: " + usages.size());
      for (String l : lines) log(l);
    });
    logFailures("usages", f);
  }

  // ------------------------------------------------------------------------------------------------ highlighting

  private static final String[] FIXTURES = {"v1_twice.erl", "v2_bind.erl", "v3_nest.erl", "v4_nonexpr.erl", "v5_decl.erl", "v6_unused.erl", "v7_multi_a.erl"};

  private void enableAll() {
    List<com.intellij.codeInspection.InspectionProfileEntry> tools = new ArrayList<>();
    for (com.intellij.codeInspection.LocalInspectionEP ep : com.intellij.codeInspection.LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new com.intellij.codeInspection.InspectionProfileEntry[0]));
  }

  public void testHighlight() throws Exception {
    enableAll();
    for (String name : FIXTURES) {
      open(name, null, 0);
      int[] c = treeCounts(myFixture.getFile());
      log("highlight " + name + " (tree errors=" + c[0] + " foreign=" + c[1] + " calls=" + c[2] + ")");
      List<Failure> f = guarded(() -> {
        List<String> lines = new ArrayList<>();
        for (HighlightInfo i : myFixture.doHighlighting()) {
          if (i.getDescription() == null) continue;
          String covered = myFixture.getEditor().getDocument().getText().substring(i.getStartOffset(), i.getEndOffset());
          lines.add("  " + i.getSeverity().getName() + " [" + i.getInspectionToolId() + "] line " +
                    (myFixture.getEditor().getDocument().getLineNumber(i.getStartOffset()) + 1) + " '" + clip(covered, 40) + "': " + clip(i.getDescription(), 140));
        }
        Collections.sort(lines);
        for (String l : lines) log(l);
      });
      logFailures("highlight", f);
    }
  }

  // ------------------------------------------------------------------------------------------------ 1. twice
  public void testV1RenameBeeFromBinding() throws Exception { rename("v1_twice.erl", "Bee = ", 1, "Bea"); }
  public void testV1RenameBeeFromArg() throws Exception { rename("v1_twice.erl", "?TWICE(Bee)", 8, "Bea"); }
  public void testV1UsagesBee() throws Exception { usages("v1_twice.erl", "Bee = ", 1); }
  public void testV1RenameVarFromArg() throws Exception { rename("v1_twice.erl", "?CL(h, Var)", 8, "Vv"); }
  public void testV1UsagesVar() throws Exception { usages("v1_twice.erl", "?CL(h, Var)", 8); }
  public void testV1GotoVar() throws Exception { navigate("v1_twice.erl", "?CL(h, Var)", 8); }
  public void testV1GotoBeeFromArg() throws Exception { navigate("v1_twice.erl", "?TWICE(Bee)", 8); }
  public void testV1RenameG() throws Exception { rename("v1_twice.erl", "g/1", 0, "gg"); }

  // ------------------------------------------------------------------------------------------------ 2. bindings
  public void testV2RenameYyFromUse() throws Exception { rename("v2_bind.erl", "Yy + 1", 0, "Why"); }
  public void testV2RenameZzFromUse() throws Exception { rename("v2_bind.erl", "Zz + 1", 0, "Zed"); }
  public void testV2GotoZzFromUse() throws Exception { navigate("v2_bind.erl", "Zz + 1", 0); }
  public void testV2UsagesZz() throws Exception { usages("v2_bind.erl", "?LET(Zz", 6); }
  public void testV2RenameQInB() throws Exception { rename("v2_bind.erl", "b(Q)", 2, "Qq"); }
  public void testV2RenameQInC() throws Exception { rename("v2_bind.erl", "c(X, Q)", 5, "Qq"); }
  public void testV2UsagesQInC() throws Exception { usages("v2_bind.erl", "c(X, Q)", 5); }

  // ------------------------------------------------------------------------------------------------ 3. nesting
  public void testV3RenameWww() throws Exception { rename("v3_nest.erl", "?WRAP(Www)", 7, "Wv"); }
  public void testV3UsagesWww() throws Exception { usages("v3_nest.erl", "?WRAP(Www)", 7); }
  public void testV3GotoWrapInArg() throws Exception { navigate("v3_nest.erl", "?WRAP(Www)", 2); }
  public void testV3GotoInnerInArg() throws Exception { navigate("v3_nest.erl", "?INNER(helper", 2); }
  public void testV3GotoInnerInOuterBody() throws Exception { navigate("v3_nest.erl", "?INNER(Arg)", 2); }
  public void testV3GotoHelperInNestedArg() throws Exception { navigate("v3_nest.erl", "?INNER(helper", 8); }
  public void testV3RenameHelper() throws Exception { rename("v3_nest.erl", "helper() -> 1", 0, "aid"); }
  public void testV3UsagesHelper() throws Exception { usages("v3_nest.erl", "helper() -> 1", 0); }
  public void testV3RenameN() throws Exception { rename("v3_nest.erl", "n(_) -> 0", 0, "nn"); }
  public void testV3RenameInnerMacro() throws Exception { rename("v3_nest.erl", "INNER(X)", 0, "INC"); }

  // ------------------------------------------------------------------------------------------------ 4. non-expression arguments
  public void testV4RenameXInGuardArg() throws Exception { rename("v4_nonexpr.erl", "is_integer(X)", 11, "Y"); }
  public void testV4GotoXInGuardArg() throws Exception { navigate("v4_nonexpr.erl", "is_integer(X)", 11); }
  public void testV4GotoGFromExport() throws Exception { navigate("v4_nonexpr.erl", "g/1", 0); }
  public void testV4RenameGFromClauseArg() throws Exception { rename("v4_nonexpr.erl", "?CLAUSE(g(1)", 8, "gee"); }
  public void testV4RenameGFromReal() throws Exception { rename("v4_nonexpr.erl", "g(_) -> other", 0, "gee"); }

  // ------------------------------------------------------------------------------------------------ 5. declarations made by macros
  public void testV5GotoRecord() throws Exception { navigate("v5_decl.erl", "#vrec", 2); }
  public void testV5RenameRecordFromUse() throws Exception { rename("v5_decl.erl", "#vrec", 2, "wrec"); }
  public void testV5UsagesRecord() throws Exception { usages("v5_decl.erl", "?REC(vrec)", 6); }
  public void testV5GotoNameFunFromExport() throws Exception { navigate("v5_decl.erl", "name_fun/0", 0); }
  public void testV5RenameNameFunFromExport() throws Exception { rename("v5_decl.erl", "name_fun/0", 0, "nf"); }
  public void testV5RenameNameFunFromArg() throws Exception { rename("v5_decl.erl", "?FUN(name_fun)", 6, "nf"); }
  public void testV5RenameRealFromDef() throws Exception { rename("v5_decl.erl", "real(X) -> X", 0, "actual"); }
  public void testV5UsagesReal() throws Exception { usages("v5_decl.erl", "real(X) -> X", 0); }
  public void testV5RenameFieldFromUse() throws Exception { rename("v5_decl.erl", "a = 2", 0, "aa"); }

  // ------------------------------------------------------------------------------------------------ 6. unused, safe delete, introduce, inline, fixes
  private void safeDelete(String marker, int shift) throws Exception {
    open("v6_unused.erl", marker, shift);
    String before = myFixture.getEditor().getDocument().getText();
    log("safe delete at '" + marker + "'+" + shift);
    List<Failure> f = guarded(() -> {
      PsiElement e = myFixture.getElementAtCaret();
      log("  element: " + describe(e));
      SafeDeleteHandler.invoke(getProject(), new PsiElement[]{e}, false);
    });
    logFailures("safe delete", f);
    String after = myFixture.getEditor().getDocument().getText();
    log("  changed: " + !after.equals(before));
    logDiff(before, after);
    saveEdit("after");
    checkStructure("safe delete");
  }

  public void testV6SafeDeleteOnlyViaMacro() throws Exception { safeDelete("only_via_macro(X) ->", 0); }
  public void testV6SafeDeleteOnlyInArg() throws Exception { safeDelete("only_in_arg(X) ->", 0); }

  public void testV6IntroduceVariableInArg() throws Exception {
    String text = fixture("v6_unused.erl");
    int s = text.indexOf("N * 3"), e = s + 5;
    myFixture.configureByText("v6_unused.erl", text.substring(0, s) + "<selection>" + text.substring(s, e) + "</selection>" + text.substring(e));
    log("introduce variable on 'N * 3' inside ?FRAG(...)");
    List<Failure> f = guarded(() -> new ErlangIntroduceVariableHandler(ErlangIntroduceVariableHandler.ReplaceStrategy.SINGLE)
      .invoke(getProject(), myFixture.getEditor(), myFixture.getFile(), null));
    logFailures("introduce", f);
    logDiff(text, myFixture.getEditor().getDocument().getText());
    saveEdit("after");
    checkStructure("introduce variable");
  }

  public void testV6InlineVariableUsedInArg() throws Exception {
    open("v6_unused.erl", "Vv = N", 0);
    String before = myFixture.getEditor().getDocument().getText();
    log("inline Vv (used only inside ?ARG(Vv))");
    List<Failure> f = guarded(() -> {
      PsiElement e = myFixture.getElementAtCaret();
      log("  element: " + describe(e));
      new ErlangInlineVariableHandler().inlineElement(getProject(), myFixture.getEditor(), e);
    });
    logFailures("inline", f);
    logDiff(before, myFixture.getEditor().getDocument().getText());
    saveEdit("after");
    checkStructure("inline");
  }

  private void fixesAt(String marker, int shift, String prefix) throws Exception {
    enableAll();
    open("v6_unused.erl", marker, shift);
    String before = myFixture.getEditor().getDocument().getText();
    log("intentions/fixes at '" + marker + "'+" + shift);
    List<Failure> f = guarded(() -> {
      List<IntentionAction> all = myFixture.getAvailableIntentions();
      List<String> names = new ArrayList<>();
      for (IntentionAction a : all) names.add(a.getText());
      Collections.sort(names);
      log("  available: " + names);
      for (IntentionAction a : all) {
        if (a.getText().startsWith(prefix)) { log("  launching: " + a.getText()); myFixture.launchAction(a); break; }
      }
    });
    logFailures("fix", f);
    logDiff(before, myFixture.getEditor().getDocument().getText());
    saveEdit("after");
    checkStructure("fix");
  }

  public void testV6CreateFunctionFixInArg() throws Exception { fixesAt("missing_fn(N)", 2, "Create"); }
  public void testV6UnusedVariableFixInArg() throws Exception { fixesAt("Unused = N", 2, "Rename"); }
  public void testV6UsagesOnlyViaMacro() throws Exception { usages("v6_unused.erl", "only_via_macro(X) ->", 0); }
  public void testV6UsagesUsedOnlyInBodyMacro() throws Exception { usages("v6_unused.erl", "USED_ONLY_IN_BODY, 42", 0); }

  // ------------------------------------------------------------------------------------------------ 7. stale tree
  public void testV7UndoRedoRenameArg() throws Exception {
    open("v1_twice.erl", "Bee = ", 1);
    String before = myFixture.getEditor().getDocument().getText();
    List<Failure> f = guarded(() -> myFixture.renameElementAtCaret("Bea"));
    logFailures("rename", f);
    String renamed = myFixture.getEditor().getDocument().getText();
    log("rename Bee -> Bea, changed: " + !renamed.equals(before));
    logDiff(before, renamed);
    checkStructure("rename");
    com.intellij.openapi.fileEditor.FileEditor fe = TextEditorProvider.getInstance().getTextEditor(myFixture.getEditor());
    UndoManager um = UndoManager.getInstance(getProject());
    log("undo available: " + um.isUndoAvailable(fe));
    List<Failure> u = guarded(() -> um.undo(fe));
    logFailures("undo", u);
    String undone = myFixture.getEditor().getDocument().getText();
    log("after undo equals original: " + undone.equals(before));
    checkStructure("undo");
    log("redo available: " + um.isRedoAvailable(fe));
    List<Failure> r = guarded(() -> um.redo(fe));
    logFailures("redo", r);
    log("after redo equals renamed: " + myFixture.getEditor().getDocument().getText().equals(renamed));
    checkStructure("redo");
    PsiElement e = myFixture.getFile().findElementAt(offsetOf("?TWICE(", 7));
    log("element in the argument after redo: " + describe(e));
  }

  private void multi(String file, String marker, int shift, String how) throws Exception {
    PsiFile a = myFixture.addFileToProject("v7_multi_a.erl", fixture("v7_multi_a.erl"));
    String bText = fixture("v7_multi_b.erl");
    if (file.equals("b")) {
      int at = bText.indexOf(marker) + shift;
      myFixture.configureByText("v7_multi_b.erl", bText.substring(0, at) + "<caret>" + bText.substring(at));
    }
    else {
      myFixture.addFileToProject("v7_multi_b.erl", bText);
      String aText = fixture("v7_multi_a.erl");
      int at = aText.indexOf(marker) + shift;
      myFixture.configureFromExistingVirtualFile(a.getVirtualFile());
      myFixture.getEditor().getCaretModel().moveToOffset(at);
    }
    log("multi-file rename [" + how + "] from " + file + " at '" + marker + "'+" + shift + " to common");
    int r0 = refusals();
    List<Failure> f = guarded(() -> {
      if (how.equals("handler")) myFixture.renameElementAtCaretUsingHandler("common");
      else myFixture.renameElementAtCaret("common");
    });
    logFailures("rename", f);
    log("  refusals: " + (refusals() - r0));
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    for (String n : new String[]{"v7_multi_a.erl", "v7_multi_b.erl"}) {
      PsiFile pf = PsiManager.getInstance(getProject()).findFile(myFixture.getTempDirFixture().getFile(n));
      String now = PsiDocumentManager.getInstance(getProject()).getDocument(pf).getText();
      log("  " + n + " changed: " + !now.equals(fixture(n)));
      logDiff(fixture(n), now);
      Files.createDirectories(myDir.resolve("v2edits"));
      Files.writeString(myDir.resolve("v2edits").resolve(getName() + "-" + n + ".txt"), now);
      List<Failure> g = guarded(() -> PsiTestUtil.checkFileStructure(pf));
      log("  structure of " + n + ": " + (g.isEmpty() ? "consistent" : "INCONSISTENT"));
      logFailures("structure " + n, g);
    }
  }

  public void testV7MultiFileRenameFromCallHandler() throws Exception { multi("b", "shared()", 1, "handler"); }
  public void testV7MultiFileRenameFromCallProcessor() throws Exception { multi("b", "shared()", 1, "processor"); }
  public void testV7MultiFileRenameFromArgHandler() throws Exception { multi("a", "?DEF(shared)", 6, "handler"); }
  public void testV7MultiFileRenameFromExportProcessor() throws Exception { multi("a", "shared/0", 1, "processor"); }

  /** A PSI edit in a write command, then a read of the substituted copies in the same command, before any commit. */
  public void testV7WriteThenReadSameCommand() throws Exception {
    open("v1_twice.erl", "?TWICE(Bee)", 8);
    PsiFile file = myFixture.getFile();
    int at = offsetOf("?TWICE(Bee)", 8);
    List<Failure> f = guarded(() -> WriteCommandAction.runWriteCommandAction(getProject(), () -> {
      PsiReference ref = file.findReferenceAt(at);
      log("reference in argument: " + (ref == null ? "none" : describe(ref.getElement())));
      if (ref != null) ref.handleElementRename("Bea");
      List<String> vars = new ArrayList<>();
      for (ErlangQVar v : PsiTreeUtil.collectElementsOfType(file, ErlangQVar.class)) {
        vars.add(v.getName() + "@" + v.getTextRange() + " text='" + v.getText() + "'");
      }
      log("vars right after the edit, same command: " + vars);
      PsiElement leaf = file.findElementAt(at);
      log("leaf at argument: " + describe(leaf));
      PsiReference ref2 = file.findReferenceAt(at);
      log("resolve right after: " + (ref2 == null ? "no ref" : describe(ref2.resolve())));
    }));
    logFailures("write-then-read", f);
    List<String> vars = new ArrayList<>();
    for (ErlangQVar v : PsiTreeUtil.collectElementsOfType(myFixture.getFile(), ErlangQVar.class)) vars.add(v.getName() + "@" + v.getTextRange());
    log("vars after the command: " + vars);
    log("--- text\n" + myFixture.getEditor().getDocument().getText() + "---");
    saveEdit("after");
    checkStructure("write then read");
  }
  // ------------------------------------------------------------------------------------------------ 8. failing master tests
  private void masterRename(String name) throws Exception {
    String text = read(Paths.get("testData/rename", name + ".erl"));
    String expected = read(Paths.get("testData/rename", name + "-after.erl"));
    myFixture.configureByText(name + ".erl", text);
    String before = myFixture.getEditor().getDocument().getText();
    log("master rename test " + name + " to uid");
    int r0 = refusals();
    List<Failure> f = guarded(() -> myFixture.renameElementAtCaret("uid"));
    logFailures("rename", f);
    String after = myFixture.getEditor().getDocument().getText();
    log("  refusals: " + (refusals() - r0) + "; changed: " + !after.equals(before) + "; equals expected: " + after.trim().equals(expected.trim()));
    logDiff(before, after);
    saveEdit("after");
    checkStructure("rename");
  }

  public void testV8TypedMacroRecordField() throws Exception { masterRename("typedMacroRecordField"); }
  public void testV8TypedMacroRecordFieldFromUsage() throws Exception { masterRename("typedMacroRecordFieldFromUsage"); }
  public void testV8MacroNamedTypedRecordFieldFromUsage() throws Exception { masterRename("macroNamedTypedRecordFieldFromUsage"); }
}
