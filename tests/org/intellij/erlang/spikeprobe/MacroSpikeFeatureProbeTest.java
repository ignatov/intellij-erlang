package org.intellij.erlang.spikeprobe;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupManager;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.lang.LanguageStructureViewBuilder;
import com.intellij.lang.documentation.DocumentationProvider;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.lang.folding.LanguageFolding;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.utils.parameterInfo.MockCreateParameterInfoContext;
import com.intellij.usageView.UsageInfo;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangParameterInfoHandler;
import org.intellij.erlang.documentation.ErlangDocumentationProvider;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.TimeUnit;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;

/**
 * Prober of the macro spike: editor features on small fixtures (testData/macroSpikeProbes). Every test writes what it
 * sees to PROBE_OUT/label/features/testName.txt and the edited texts to PROBE_OUT/label/edits/; it never fails on a
 * finding. No-op without PROBE_OUT.
 */
public class MacroSpikeFeatureProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final StringBuilder myLog = new StringBuilder();
  private Path myDir;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/macroSpikeProbes/";
  }

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
        Files.createDirectories(myDir.resolve("features"));
        Files.writeString(myDir.resolve("features").resolve(getName() + ".txt"), myLog.toString());
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

  private void log(String s) {
    myLog.append(s).append('\n');
  }

  private void logFailures(String what, List<Failure> failures) {
    for (Failure f : failures) log("  FAILURE " + what + ": " + f.line());
  }

  private String fixture(String name) throws Exception {
    return read(Paths.get(getTestDataPath(), name));
  }

  /** Opens the fixture with a caret placed before the first occurrence of marker plus shift. */
  private void open(String name, String marker, int shift) throws Exception {
    String text = fixture(name);
    int at = marker == null ? 0 : text.indexOf(marker);
    if (at < 0) throw new IllegalArgumentException(marker);
    int caret = marker == null ? 0 : at + shift;
    myFixture.configureByText(name, text.substring(0, caret) + "<caret>" + text.substring(caret));
  }

  private void saveEdit(String tag) throws Exception {
    Files.createDirectories(myDir.resolve("edits"));
    String name = getName() + "-" + tag;
    String text = myFixture.getEditor().getDocument().getText();
    Files.writeString(myDir.resolve("edits").resolve(name + ".erl.txt"), text);
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

  // ------------------------------------------------------------------------------------------------ 2. rename

  private void rename(String file, String marker, int shift, String newName) throws Exception {
    open(file, marker, shift);
    String before = myFixture.getEditor().getDocument().getText();
    log("rename in " + file + " at '" + marker + "'+" + shift + " to " + newName);
    PsiElement[] target = new PsiElement[1];
    guarded(() -> target[0] = myFixture.getElementAtCaret());
    log("  element at caret: " + describe(target[0]));
    List<Failure> f = guarded(() -> myFixture.renameElementAtCaret(newName));
    logFailures("rename", f);
    String after = myFixture.getEditor().getDocument().getText();
    log("  changed: " + !after.equals(before));
    log("--- text after\n" + after + "---");
    saveEdit("after");
    checkStructure("rename");
  }

  public void testRenameFunctionMadeByMacroFromCall() throws Exception { rename("def.erl", "hello(),", 2, "greet"); }

  public void testRenameFunctionMadeByMacroFromExport() throws Exception { rename("def.erl", "hello/0", 1, "greet"); }

  public void testRenameFunctionMadeByMacroFromArgument() throws Exception { rename("def.erl", "?DEF(hello)", 6, "greet"); }

  public void testRenameFunctionMadeByClauseMacro() throws Exception { rename("def.erl", "m(1).", 0, "mm"); }

  public void testRenameFunctionUsedInMacroArguments() throws Exception { rename("calls.erl", "helper() -> ok", 2, "assist"); }

  public void testRenameFunctionUsedInMacroArgumentsFromUse() throws Exception { rename("calls.erl", "?CLAUSES(helper", 10, "assist"); }

  public void testRenameVariableFromMacroBody() throws Exception { rename("pat.erl", "Value;", 2, "Result"); }

  public void testRenameMacroParameter() throws Exception { rename("def.erl", "DEF(N)", 4, "Name"); }

  public void testRenameMacroFromDefinition() throws Exception { rename("sep.erl", "SEP, ;", 1, "SEMI"); }

  public void testRenameMacroFromUse() throws Exception { rename("sep.erl", "?SEP\n", 2, "SEMI"); }

  public void testRenameClauseMacroFromUse() throws Exception { rename("def.erl", "?DEF(hello)", 2, "MAKE"); }

  public void testRenameRecordField() throws Exception { rename("calls.erl", "{val, other}", 2, "value"); }

  public void testRenameRecordFieldFromUse() throws Exception { rename("calls.erl", "val = 1", 1, "value"); }

  public void testRenameRecord() throws Exception { rename("calls.erl", "record(st", 8, "state"); }

  public void testRenameVariableInsideMacroArgument() throws Exception { rename("calls.erl", "?HANDLER(S)", 9, "State"); }

  public void testRenameNestedClauseFunction() throws Exception { rename("nest.erl", "n() -> n(1)", 7, "nn"); }

  // ------------------------------------------------------------------------------------------------ 3. navigation

  private void navigate(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("navigate in " + file + " at '" + marker + "'+" + shift);
    int offset = myFixture.getCaretOffset();
    List<Failure> f = guarded(() -> {
      PsiReference ref = myFixture.getFile().findReferenceAt(offset);
      log("  reference: " + (ref == null ? "none" : ref.getClass().getSimpleName() + " " + ref.getRangeInElement() + " in " + describe(ref.getElement())));
      if (ref instanceof PsiPolyVariantReference poly) {
        for (ResolveResult rr : poly.multiResolve(false)) log("  multiResolve: " + describe(rr.getElement()));
      }
      else if (ref != null) log("  resolve: " + describe(ref.resolve()));
      PsiElement target = GotoDeclarationAction.findTargetElement(getProject(), myFixture.getEditor(), offset);
      log("  goto target: " + describe(target));
      if (target != null) {
        PsiElement nav = target.getNavigationElement();
        log("  navigation element: " + describe(nav) + " textOffset=" + nav.getTextOffset() + " line=" + line(nav.getTextOffset()));
      }
    });
    logFailures("navigate", f);
  }

  private int line(int offset) {
    Document d = myFixture.getEditor().getDocument();
    return offset < 0 || offset > d.getTextLength() ? -1 : d.getLineNumber(offset) + 1;
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
        PsiElement ue = u.getElement();
        TextRange r = u.getSegment() == null ? null : TextRange.create(u.getSegment());
        lines.add("  usage: " + describe(ue) + " segment=" + r + " line=" + (r == null ? -1 : line(r.getStartOffset())) +
                  " emptyRange=" + (r != null && r.isEmpty()));
      }
      Collections.sort(lines);
      log("  usages: " + usages.size());
      for (String l : lines) log(l);
    });
    logFailures("usages", f);
  }

  public void testGotoMacroFromSepUse() throws Exception { navigate("sep.erl", "?SEP\n", 2); }

  public void testGotoMacroFromDefUse() throws Exception { navigate("def.erl", "?DEF(hello)", 2); }

  public void testGotoFunctionMadeByMacro() throws Exception { navigate("def.erl", "hello(),", 2); }

  public void testGotoFunctionMadeByWholeMacro() throws Exception { navigate("def.erl", "world(),", 2); }

  public void testGotoFunctionMadeByClauseMacro() throws Exception { navigate("def.erl", "m(1).", 0); }

  public void testGotoVariableBindingInMacroBody() throws Exception { navigate("pat.erl", "Value;", 2); }

  public void testGotoFunctionFromMacroArgument() throws Exception { navigate("calls.erl", "?CLAUSES(helper", 10); }

  public void testGotoRecordFromSubstitutedBody() throws Exception { navigate("calls.erl", "#st{val", 2); }

  public void testUsagesOfFunctionCalledInMacroArguments() throws Exception { usages("calls.erl", "helper() -> ok", 2); }

  public void testUsagesOfFunctionMadeByMacro() throws Exception { usages("def.erl", "hello(),", 2); }

  public void testUsagesOfSepMacro() throws Exception { usages("sep.erl", "SEP, ;", 1); }

  public void testUsagesOfDefMacro() throws Exception { usages("def.erl", "DEF(N)", 1); }

  public void testUsagesOfRecordField() throws Exception { usages("calls.erl", "{val, other}", 2); }

  public void testUsagesOfVariableFromMacroBody() throws Exception { usages("pat.erl", "Value;", 2); }

  // ------------------------------------------------------------------------------------------------ 5. typing

  private void typeAt(String file, String marker, int shift, String chars) throws Exception {
    open(file, marker, shift);
    log("type '" + chars.replace("\n", "\\n") + "' in " + file + " at '" + marker.replace("\n", "\\n") + "'+" + shift);
    for (char c : chars.toCharArray()) {
      List<Failure> f = guarded(() -> {
        myFixture.type(c);
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      });
      logFailures("type " + c, f);
      checkStructure("typing '" + (c == '\n' ? "\\n" : String.valueOf(c)) + "'");
    }
    log("--- text after\n" + myFixture.getEditor().getDocument().getText() + "---");
  }

  public void testTypeInsideDefineBody() throws Exception { typeAt("def.erl", "-> ok).", 3, " orelse x"); }

  public void testTypeInsideDefineName() throws Exception { typeAt("sep.erl", "SEP, ;", 3, "X"); }

  public void testTypeInsideUseArguments() throws Exception { typeAt("def.erl", "?DEF(hello)", 10, "_x"); }

  public void testTypeInsideUseName() throws Exception { typeAt("sep.erl", "?SEP\n", 3, "Z"); }

  public void testTypeBeforeUse() throws Exception { typeAt("sep.erl", "?SEP\n", 0, " ok "); }

  public void testTypeAfterUse() throws Exception { typeAt("sep.erl", "?SEP\n", 4, " "); }

  public void testTypeAfterClauseUse() throws Exception { typeAt("def.erl", "?WHOLE.", 6, "\n"); }

  public void testTypeInPatternMacroUse() throws Exception { typeAt("pat.erl", "f(?PAT)", 6, "X"); }

  public void testTypeNestedUse() throws Exception { typeAt("nest.erl", "?CL(2)", 4, "0"); }

  public void testDeleteDefine() throws Exception {
    for (String[] c : new String[][]{{"sep.erl", "-define(SEP, ;).\n"}, {"def.erl", "-define(DEF(N), N() -> ok).\n"},
      {"pat.erl", "-define(PAT, {ok, Value}).\n"}, {"nest.erl", "-define(SEP, ;).\n"}}) {
      open(c[0], c[1], 0);
      log("delete '" + c[1].trim() + "' in " + c[0]);
      Document d = myFixture.getEditor().getDocument();
      int at = d.getText().indexOf(c[1]);
      List<Failure> f = guarded(() -> WriteCommandAction.runWriteCommandAction(getProject(), () -> d.deleteString(at, at + c[1].length())));
      logFailures("delete", f);
      checkStructure("deleting the define");
      List<Failure> u = guarded(() -> WriteCommandAction.runWriteCommandAction(getProject(), () -> d.insertString(at, c[1])));
      logFailures("reinsert", u);
      checkStructure("re-inserting the define");
    }
  }

  /** 50 keystrokes in a big file with many substituted uses; PROBE_TYPING_FILE, PROBE_TYPING_MARKER (text before the caret). */
  public void testTypingSpeed() throws Exception {
    String path = env("PROBE_TYPING_FILE");
    String marker = env("PROBE_TYPING_MARKER");
    if (path == null || marker == null) return;
    String text = read(Paths.get(path));
    int at = text.indexOf(marker);
    if (at < 0) throw new IllegalArgumentException(marker);
    at += marker.length();
    myFixture.configureByText(Paths.get(path).getFileName().toString(), text.substring(0, at) + "<caret>" + text.substring(at));
    String typed = "        foo(Bar),\n        baz(Qux, 1),\n        ok,\n          ";
    typed = (typed + typed).substring(0, 50);
    Document d = myFixture.getEditor().getDocument();
    List<Long> runs = new ArrayList<>();
    List<Long> firstHighlight = new ArrayList<>();
    for (int rep = 0; rep < 6; rep++) {
      WriteCommandAction.runWriteCommandAction(getProject(), () -> d.setText(text));
      PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      myFixture.getFile().getNode().getFirstChildNode();
      myFixture.getEditor().getCaretModel().moveToOffset(at);
      long t0 = System.nanoTime();
      for (char c : typed.toCharArray()) {
        myFixture.type(c);
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      }
      long ms = (System.nanoTime() - t0) / 1_000_000;
      if (rep > 0) runs.add(ms);
      log("rep " + rep + ": 50 keystrokes with commit: " + ms + " ms");
    }
    Collections.sort(runs);
    log("median of 5 measured reps: " + runs.get(runs.size() / 2) + " ms; per keystroke " + runs.get(runs.size() / 2) / 50.0 + " ms");
    int[] counts = treeCounts(myFixture.getFile());
    log("tree after typing: errors=" + counts[0] + " foreign=" + counts[1] + " calls=" + counts[2]);
    checkStructure("typing speed run");
  }

  // ------------------------------------------------------------------------------------------------ 6. other features

  private static final String[] FIXTURES = {"sep.erl", "def.erl", "commas.erl", "pat.erl", "expr.erl", "nest.erl", "cnd.erl", "calls.erl"};

  public void testFolding() throws Exception {
    for (String name : FIXTURES) {
      open(name, null, 0);
      log("folding " + name);
      List<Failure> f = guarded(() -> {
        FoldingDescriptor[] ds = LanguageFolding.buildFoldingDescriptors(
          LanguageFolding.INSTANCE.forLanguage(ErlangLanguage.INSTANCE), myFixture.getFile(), myFixture.getEditor().getDocument(), false);
        for (FoldingDescriptor d : ds) log("  " + d.getRange() + " '" + d.getPlaceholderText() + "' empty=" + d.getRange().isEmpty());
      });
      logFailures("folding", f);
    }
  }

  public void testStructureView() throws Exception {
    for (String name : FIXTURES) {
      open(name, null, 0);
      log("structure " + name);
      List<Failure> f = guarded(() -> {
        StructureViewBuilder b = LanguageStructureViewBuilder.getInstance().getStructureViewBuilder(myFixture.getFile());
        StructureViewModel model = ((TreeBasedStructureViewBuilder)b).createStructureViewModel(myFixture.getEditor());
        try {
          print(model.getRoot(), 1);
        }
        finally {
          Disposer.dispose(model);
        }
      });
      logFailures("structure", f);
    }
  }

  private void print(TreeElement e, int depth) {
    String text = e.getPresentation().getPresentableText();
    String where = "";
    if (e instanceof com.intellij.ide.structureView.StructureViewTreeElement s && s.getValue() instanceof PsiElement p) {
      where = " offset=" + p.getTextOffset() + " range=" + p.getTextRange();
    }
    log("  ".repeat(depth) + text + where);
    for (TreeElement c : e.getChildren()) print(c, depth + 1);
  }

  private void complete(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("complete in " + file + " at '" + marker.replace("\n", "\\n") + "'+" + shift);
    List<Failure> f = guarded(() -> {
      LookupElement[] items = myFixture.completeBasic();
      if (items == null) {
        log("  single item inserted; text now: " + clip(myFixture.getEditor().getDocument().getText(), 400));
        return;
      }
      List<String> names = new ArrayList<>();
      for (LookupElement i : items) names.add(i.getLookupString());
      Collections.sort(names);
      log("  items: " + names.size() + " " + clip(String.join(",", names), 600));
      LookupManager.hideActiveLookup(getProject());
    });
    logFailures("completion", f);
  }

  public void testCompletion() throws Exception {
    complete("def.erl", "?DEF(hello)", 7);
    complete("def.erl", "?DEF(hello).\n", 13);
    complete("def.erl", "hello(), world()", 9);
    complete("sep.erl", "?SEP\n", 4);
    complete("sep.erl", "?SEP\n", 2);
    complete("pat.erl", "-> Value;", 4);
    complete("nest.erl", "[?OUTER(3)]", 8);
    complete("commas.erl", "{?PAIR, 3}", 7);
  }

  private void action(String file, String marker, int shift, String actionId, int times) throws Exception {
    open(file, marker, shift);
    log("action " + actionId + " x" + times + " in " + file + " at '" + marker.replace("\n", "\\n") + "'+" + shift);
    for (int i = 0; i < times; i++) {
      List<Failure> f = guarded(() -> myFixture.performEditorAction(actionId));
      logFailures(actionId, f);
      Editor ed = myFixture.getEditor();
      if (ed.getSelectionModel().hasSelection()) {
        log("  selection " + i + ": " + ed.getSelectionModel().getSelectionStart() + ".." + ed.getSelectionModel().getSelectionEnd() +
            " '" + clip(ed.getSelectionModel().getSelectedText(), 80) + "'");
      }
    }
    log("--- text after\n" + myFixture.getEditor().getDocument().getText() + "---");
    checkStructure(actionId);
  }

  public void testEnterInsideUse() throws Exception { action("def.erl", "?DEF(hello)", 5, IdeActions.ACTION_EDITOR_ENTER, 1); }

  public void testEnterAfterSepUse() throws Exception { action("sep.erl", "?SEP\n", 4, IdeActions.ACTION_EDITOR_ENTER, 1); }

  public void testEnterAfterClauseUse() throws Exception { action("def.erl", "?M(1);", 6, IdeActions.ACTION_EDITOR_ENTER, 1); }

  public void testCommentLineWithUse() throws Exception { action("sep.erl", "?SEP\n", 1, IdeActions.ACTION_COMMENT_LINE, 2); }

  public void testCommentLineWithClauseUse() throws Exception { action("def.erl", "?DEF(hello)", 1, IdeActions.ACTION_COMMENT_LINE, 2); }

  public void testExtendSelectionInUse() throws Exception { action("def.erl", "?DEF(hello)", 7, IdeActions.ACTION_EDITOR_SELECT_WORD_AT_CARET, 6); }

  public void testExtendSelectionInPatternUse() throws Exception { action("pat.erl", "f(?PAT)", 4, IdeActions.ACTION_EDITOR_SELECT_WORD_AT_CARET, 6); }

  public void testExtendSelectionAfterSep() throws Exception { action("sep.erl", "g(2)", 1, IdeActions.ACTION_EDITOR_SELECT_WORD_AT_CARET, 6); }

  public void testReformatLineIndent() throws Exception { action("sep.erl", "g(2)", 0, IdeActions.ACTION_EDITOR_AUTO_INDENT_LINES, 1); }

  public void testCopyPasteUse() throws Exception {
    open("sep.erl", "g(1)", 0);
    log("copy lines 4-5 of sep.erl and paste at the end");
    Editor ed = myFixture.getEditor();
    Document d = ed.getDocument();
    int start = d.getText().indexOf("g(1)");
    int end = d.getText().indexOf("g(2) -> b.") + "g(2) -> b.\n".length();
    ed.getSelectionModel().setSelection(start, end);
    logFailures("copy", guarded(() -> myFixture.performEditorAction(IdeActions.ACTION_COPY)));
    ed.getSelectionModel().removeSelection();
    ed.getCaretModel().moveToOffset(d.getTextLength());
    logFailures("paste", guarded(() -> myFixture.performEditorAction(IdeActions.ACTION_PASTE)));
    log("--- text after\n" + d.getText() + "---");
    checkStructure("paste");
    open("def.erl", "?DEF(hello).", 0);
    ed = myFixture.getEditor();
    d = ed.getDocument();
    int s2 = d.getText().indexOf("?DEF(hello).");
    ed.getSelectionModel().setSelection(s2, s2 + "?DEF(hello).\n".length());
    logFailures("cut", guarded(() -> myFixture.performEditorAction(IdeActions.ACTION_CUT)));
    ed.getCaretModel().moveToOffset(d.getTextLength());
    logFailures("paste", guarded(() -> myFixture.performEditorAction(IdeActions.ACTION_PASTE)));
    log("--- text after cut/paste of ?DEF(hello).\n" + d.getText() + "---");
    checkStructure("cut and paste");
  }

  private void paramInfo(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("parameter info in " + file + " at '" + marker + "'+" + shift);
    List<Failure> f = guarded(() -> {
      ErlangParameterInfoHandler h = new ErlangParameterInfoHandler();
      MockCreateParameterInfoContext ctx = new MockCreateParameterInfoContext(myFixture.getEditor(), myFixture.getFile());
      Object list = ApplicationManager.getApplication()
        .executeOnPooledThread(() -> ReadAction.compute(() -> h.findElementForParameterInfo(ctx))).get(1, TimeUnit.MINUTES);
      log("  argument list: " + (list == null ? "none" : describe((PsiElement)list)));
      Object[] items = ctx.getItemsToShow();
      log("  items: " + (items == null ? "none" : items.length));
      if (items != null) for (Object o : items) log("   " + (o instanceof PsiElement p ? describe(p) : String.valueOf(o)));
    });
    logFailures("parameter info", f);
  }

  public void testParameterInfo() throws Exception {
    paramInfo("def.erl", "?DEF(hello)", 6);
    paramInfo("nest.erl", "?STR(1 + 2)", 6);
    paramInfo("calls.erl", "?CLAUSES(helper(), other())", 17);
    paramInfo("nest.erl", "?OUTER(3)", 7);
  }

  private void doc(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("quick doc in " + file + " at '" + marker + "'+" + shift);
    List<Failure> f = guarded(() -> {
      int offset = myFixture.getCaretOffset();
      PsiElement target = GotoDeclarationAction.findTargetElement(getProject(), myFixture.getEditor(), offset);
      log("  target: " + describe(target));
      DocumentationProvider p = new ErlangDocumentationProvider();
      PsiElement original = myFixture.getFile().findElementAt(offset);
      String html = target == null ? null : ReadAction.compute(() -> p.generateDoc(target, original));
      log("  doc: " + (html == null ? "null" : clip(html, 300)));
      String quick = target == null ? null : ReadAction.compute(() -> p.getQuickNavigateInfo(target, original));
      log("  quick navigate: " + (quick == null ? "null" : clip(quick, 200)));
    });
    logFailures("doc", f);
  }

  public void testQuickDoc() throws Exception {
    doc("sep.erl", "?SEP\n", 2);
    doc("def.erl", "?DEF(hello)", 2);
    doc("def.erl", "hello(),", 2);
    doc("expr.erl", "?X *", 1);
  }

  private void intentions(String file, String marker, int shift) throws Exception {
    open(file, marker, shift);
    log("intentions in " + file + " at '" + marker.replace("\n", "\\n") + "'+" + shift);
    List<Failure> f = guarded(() -> {
      List<IntentionAction> actions = myFixture.getAvailableIntentions();
      List<String> names = new ArrayList<>();
      for (IntentionAction a : actions) names.add(a.getText());
      Collections.sort(names);
      log("  available: " + names);
    });
    logFailures("intentions", f);
  }

  public void testIntentions() throws Exception {
    intentions("def.erl", "?DEF(hello)", 6);
    intentions("def.erl", "hello(),", 2);
    intentions("sep.erl", "?SEP\n", 2);
    intentions("sep.erl", "g(2)", 0);
    intentions("calls.erl", "?CLAUSES(helper", 10);
    intentions("pat.erl", "Value;", 2);
  }

  public void testIntroduceMacroFix() throws Exception {
    String text = fixture("sep.erl").replace("g(2) -> b.", "g(2) -> ?NEW_ONE.");
    int at = text.indexOf("?NEW_ONE") + 2;
    myFixture.configureByText("sep.erl", text.substring(0, at) + "<caret>" + text.substring(at));
    log("introduce macro fix on ?NEW_ONE in sep.erl");
    List<Failure> f = guarded(() -> {
      myFixture.enableInspections(new org.intellij.erlang.inspection.ErlangUnresolvedMacrosInspection());
      List<IntentionAction> fixes = myFixture.filterAvailableIntentions("Introduce macro");
      log("  fixes: " + fixes.size());
      if (!fixes.isEmpty()) myFixture.launchAction(fixes.get(0));
    });
    logFailures("introduce macro", f);
    log("--- text after\n" + myFixture.getEditor().getDocument().getText() + "---");
    saveEdit("after");
    checkStructure("introduce macro");
  }

  public void testHighlightFixtures() throws Exception {
    List<com.intellij.codeInspection.InspectionProfileEntry> tools = new ArrayList<>();
    for (com.intellij.codeInspection.LocalInspectionEP ep : com.intellij.codeInspection.LocalInspectionEP.LOCAL_INSPECTION.getExtensionList()) {
      if (ep.implementationClass == null || !ep.implementationClass.startsWith("org.intellij.erlang.")) continue;
      if (ep.implementationClass.contains(".dialyzer.")) continue;
      tools.add(ep.instantiateTool());
    }
    myFixture.enableInspections(tools.toArray(new com.intellij.codeInspection.InspectionProfileEntry[0]));
    for (String name : FIXTURES) {
      open(name, null, 0);
      log("highlight " + name);
      List<Failure> f = guarded(() -> {
        List<String> lines = new ArrayList<>();
        for (HighlightInfo i : myFixture.doHighlighting()) {
          if (i.getDescription() == null) continue;
          String covered = myFixture.getEditor().getDocument().getText().substring(i.getStartOffset(), i.getEndOffset());
          lines.add("  " + i.getSeverity().getName() + " [" + i.getInspectionToolId() + "] " + i.getStartOffset() + ".." + i.getEndOffset() +
                    " line " + line(i.getStartOffset()) + " '" + clip(covered, 40) + "': " + clip(i.getDescription(), 160));
        }
        Collections.sort(lines);
        for (String l : lines) log(l);
      });
      logFailures("highlight", f);
    }
  }

  public void testReformatFixtures() throws Exception {
    for (String name : FIXTURES) {
      open(name, null, 0);
      String before = myFixture.getEditor().getDocument().getText();
      List<Failure> f = guarded(() -> {
        WriteCommandAction.runWriteCommandAction(getProject(), () -> { com.intellij.psi.codeStyle.CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile()); });
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      });
      String once = myFixture.getEditor().getDocument().getText();
      List<Failure> g = guarded(() -> {
        WriteCommandAction.runWriteCommandAction(getProject(), () -> { com.intellij.psi.codeStyle.CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile()); });
        PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
      });
      String twice = myFixture.getEditor().getDocument().getText();
      log("reformat " + name + ": changed=" + !once.equals(before) + " stable=" + once.equals(twice));
      logFailures("reformat", f);
      logFailures("reformat 2", g);
      log("--- text after\n" + once + "---");
      Files.createDirectories(myDir.resolve("edits"));
      Files.writeString(myDir.resolve("edits").resolve("reformat-" + name + ".txt"), once);
    }
  }

  // ------------------------------------------------------------------------------------------------ 7. stubs, cross-file

  public void testCrossFileResolve() throws Exception {
    PsiFile a = myFixture.addFileToProject("def.erl", fixture("def.erl"));
    String b = "-module(user).\n-export([u/0]).\nu() -> def:hel<caret>lo(), def:world(), def:m(2).\n";
    myFixture.configureByText("user.erl", b);
    log("cross-file: user.erl calls def:hello() (made by ?DEF(hello) in def.erl)");
    List<Failure> f = guarded(() -> {
      PsiFileImpl impl = (PsiFileImpl)PsiManager.getInstance(getProject()).findFile(a.getVirtualFile());
      impl.clearCaches();
      log("  def.erl AST loaded before resolve: " + impl.isContentsLoaded());
      for (String fn : new String[]{"hello()", "world()", "m(2)"}) {
        int offset = myFixture.getEditor().getDocument().getText().indexOf("def:" + fn) + 5;
        PsiReference ref = myFixture.getFile().findReferenceAt(offset);
        PsiElement target = ref == null ? null : ref.resolve();
        log("  def:" + fn + " resolves to: " + describe(target) + (target == null ? "" : " in " + target.getContainingFile().getName()));
        log("  def.erl AST loaded after resolve: " + impl.isContentsLoaded());
      }
      LookupElement[] items = null;
      Document d = myFixture.getEditor().getDocument();
      int at = d.getText().indexOf("def:hello") + 4;
      myFixture.getEditor().getCaretModel().moveToOffset(at);
      items = myFixture.completeBasic();
      List<String> names = new ArrayList<>();
      if (items != null) for (LookupElement i : items) names.add(i.getLookupString());
      Collections.sort(names);
      log("  completion after def: " + names);
      LookupManager.hideActiveLookup(getProject());
      Collection<String> keys = com.intellij.psi.stubs.StubIndex.getInstance()
        .getAllKeys(org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, getProject());
      List<String> mine = new ArrayList<>();
      for (String k : keys) if (k.isEmpty() || k.equals("hello") || k.equals("world") || k.equals("m")) mine.add("'" + k + "'");
      Collections.sort(mine);
      log("  ErlangAllNameIndex keys among hello/world/m/empty: " + mine);
    });
    logFailures("cross-file", f);
  }

  public void testAtomIndexOfFixtures() throws Exception {
    for (String name : FIXTURES) {
      PsiFile file = myFixture.addFileToProject("idx/" + name, fixture(name));
      List<Failure> f = guarded(() -> {
        Map<String, Void> data = com.intellij.util.indexing.FileBasedIndex.getInstance()
          .getFileData(com.intellij.util.indexing.ID.findByName("erlang.atom.index"), file.getVirtualFile(), getProject());
        List<String> keys = new ArrayList<>();
        for (String k : data.keySet()) keys.add("'" + k + "'");
        Collections.sort(keys);
        log("atoms " + name + ": " + keys);
        String stub = com.intellij.psi.impl.DebugUtil.stubTreeToString(((PsiFileImpl)file).calcStubTree().getRoot());
        log("stubs " + name + ":\n" + stub);
        com.intellij.psi.stubs.StubTextInconsistencyException.checkStubTextConsistency(file);
      });
      logFailures("atoms " + name, f);
    }
  }

  // ------------------------------------------------------------------------------------------------ real files

  private static final String OTP = "/opt/homebrew/Cellar/erlang/29.1/lib/erlang/lib/";
  private static final String CORPUS = "/Users/ignatov/factory-work/macro-research/count/corpus/";

  private void renameInReal(String path, String marker, int shift, String newName) throws Exception {
    String text = read(Paths.get(path));
    int at = text.indexOf(marker);
    if (at < 0) throw new IllegalArgumentException(marker);
    int caret = at + shift;
    myFixture.configureByText(Paths.get(path).getFileName().toString(), text.substring(0, caret) + "<caret>" + text.substring(caret));
    log("rename in " + path + " at '" + marker + "'+" + shift + " to " + newName);
    PsiElement[] target = new PsiElement[1];
    guarded(() -> target[0] = myFixture.getElementAtCaret());
    log("  element at caret: " + describe(target[0]));
    List<Failure> f = guarded(() -> myFixture.renameElementAtCaret(newName));
    logFailures("rename", f);
    String after = myFixture.getEditor().getDocument().getText();
    log("  changed: " + !after.equals(text));
    // the changed lines only
    String[] a = text.split("\n", -1), b = after.split("\n", -1);
    if (a.length == b.length) {
      for (int i = 0; i < a.length; i++) if (!a[i].equals(b[i])) log("  line " + (i + 1) + ": '" + a[i].trim() + "' -> '" + b[i].trim() + "'");
    }
    else log("  line count " + a.length + " -> " + b.length);
    Files.createDirectories(myDir.resolve("edits"));
    Files.writeString(myDir.resolve("edits").resolve(getName() + "-" + newName + "-" + Paths.get(path).getFileName()), after);
    checkStructure("rename");
  }

  public void testRealRenameIoAnsiBlack() throws Exception {
    renameInReal(OTP + "stdlib-8.1/src/io_ansi.erl", "-export([black/0", 10, "jet");
  }

  public void testRealRenameXmerlVariableInMacroArgument() throws Exception {
    renameInReal(OTP + "xmerl-2.2/src/xmerl_scan.erl", "S5b = cleanup(S5),\n\t\t\t ?fatal({failed_validation, Reason}, S5b);", 1, "S5c");
  }

  public void testRealRenameStreamVariableInMacroArgument() throws Exception {
    renameInReal(CORPUS + "rabbitmq-server/deps/rabbitmq_stream_common/src/rabbit_stream_core.erl", "|| Stream <- Streams],", 4, "Strm");
  }

  public void testRealUsagesIoAnsiBlack() throws Exception {
    String path = OTP + "stdlib-8.1/src/io_ansi.erl";
    String text = read(Paths.get(path));
    int caret = text.indexOf("-export([black/0") + 10;
    myFixture.configureByText("io_ansi.erl", text.substring(0, caret) + "<caret>" + text.substring(caret));
    log("usages of black/0 in io_ansi.erl");
    List<Failure> f = guarded(() -> {
      PsiElement e = myFixture.getElementAtCaret();
      log("  element: " + describe(e) + " line=" + line(e.getTextOffset()));
      for (UsageInfo u : myFixture.findUsages(e)) log("  usage: " + describe(u.getElement()) + " line=" + line(u.getSegment().getStartOffset()));
    });
    logFailures("usages", f);
  }

  public void testEmptyNameIndexKey() throws Exception {
    for (String name : FIXTURES) myFixture.addFileToProject("k/" + name, fixture(name));
    List<Failure> f = guarded(() -> {
      for (String k : new String[]{""}) {
        Collection<org.intellij.erlang.psi.ErlangNamedElement> els = com.intellij.psi.stubs.StubIndex.getElements(
          org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, k, getProject(), GlobalSearchScope.allScope(getProject()),
          org.intellij.erlang.psi.ErlangNamedElement.class);
        log("ErlangAllNameIndex key '" + k + "': " + els.size());
        for (PsiElement e : els) log("  " + e.getContainingFile().getName() + " " + describe(e) + " line=" + e.getContainingFile().getViewProvider().getDocument().getLineNumber(e.getTextOffset()));
      }
    });
    logFailures("empty key", f);
  }

  public void testCrossFileControl() throws Exception {
    PsiFile a = myFixture.addFileToProject("def.erl", fixture("def.erl"));
    myFixture.configureByText("user.erl", "-module(user).\nu() -> def:f().\n");
    List<Failure> f = guarded(() -> {
      PsiFileImpl impl = (PsiFileImpl)PsiManager.getInstance(getProject()).findFile(a.getVirtualFile());
      impl.clearCaches();
      log("control: def.erl AST loaded before resolve: " + impl.isContentsLoaded());
      int offset = myFixture.getEditor().getDocument().getText().indexOf("def:f") + 4;
      PsiReference ref = myFixture.getFile().findReferenceAt(offset);
      log("  def:f() resolves to: " + describe(ref == null ? null : ref.resolve()));
      log("  def.erl AST loaded after resolve: " + impl.isContentsLoaded());
    });
    logFailures("control", f);
  }

  public void testEmptyNameKeySource() throws Exception {
    for (String name : FIXTURES) myFixture.addFileToProject("e/" + name, fixture(name));
    myFixture.configureByText("user.erl", "-module(user).\nu() -> def:hello().\n");
    List<Failure> f = guarded(() -> {
      com.intellij.psi.stubs.StubIndex index = com.intellij.psi.stubs.StubIndex.getInstance();
      Collection<String> keys = index.getAllKeys(org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, getProject());
      log("has empty key: " + keys.contains(""));
      Iterator<com.intellij.openapi.vfs.VirtualFile> it = index.getContainingFilesIterator(
        org.intellij.erlang.stubs.index.ErlangAllNameIndex.KEY, "", getProject(), GlobalSearchScope.allScope(getProject()));
      while (it.hasNext()) log("  file with empty key: " + it.next().getPath());
      for (String name : FIXTURES) {
        PsiFile file = PsiManager.getInstance(getProject()).findFile(myFixture.getTempDirFixture().getFile("e/" + name));
        com.intellij.psi.stubs.StubElement<?> root = ((PsiFileImpl)file).calcStubTree().getRoot();
        for (com.intellij.psi.stubs.StubElement<?> c : root.getChildrenStubs()) {
          if (c instanceof com.intellij.psi.stubs.NamedStub<?> n && (n.getName() == null || n.getName().isEmpty())) {
            log("  empty-named stub in " + name + ": " + c.getClass().getSimpleName() + " psi=" + describe(c.getPsi()));
          }
        }
      }
    });
    logFailures("empty key source", f);
  }
}
