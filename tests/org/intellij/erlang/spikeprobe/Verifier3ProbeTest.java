package org.intellij.erlang.spikeprobe;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationAction;
import com.intellij.ide.structureView.StructureViewBuilder;
import com.intellij.ide.structureView.StructureViewModel;
import com.intellij.ide.structureView.TreeBasedStructureViewBuilder;
import com.intellij.ide.util.treeView.smartTree.TreeElement;
import com.intellij.lang.LanguageStructureViewBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.lang.folding.LanguageFolding;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.ref.GCUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.psi.ErlangQVar;
import org.intellij.erlang.search.ErlangFindUsagesProvider;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.ref.WeakReference;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static org.intellij.erlang.spikeprobe.MacroSpikeProbeSupport.*;
import static org.intellij.erlang.spikeprobe.Verifier3Support.*;

/** Verifier 3: probes of the oracle design. Writes PROBE_OUT/label/v3/test.txt and v3edits/; never fails on a finding. */
public class Verifier3ProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final StringBuilder myLog = new StringBuilder();
  private int myN;

  @Override protected LightProjectDescriptor getProjectDescriptor() { return descriptor(); }

  @Override protected String getTestDataPath() { return "testData/verifier3/"; }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  @Override
  protected void runTestRunnable(com.intellij.util.ThrowableRunnable<Throwable> testRunnable) throws Throwable {
    if (env("PROBE_OUT") == null) return;
    super.runTestRunnable(testRunnable);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      Path dir = Paths.get(env("PROBE_OUT"), label(), "v3");
      Files.createDirectories(dir);
      Files.writeString(dir.resolve(getName() + ".txt"), myLog.toString());
    }
    catch (Throwable ignored) {
    }
    finally {
      super.tearDown();
    }
  }

  private void log(String s) { myLog.append(s).append('\n'); }

  private String fixture(String name) throws Exception { return read(Paths.get("testData/verifier3", name)); }

  private PsiFile open(String name, String text) {
    PsiFile f = myFixture.addFileToProject("f" + (myN++) + "/" + name, text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    return myFixture.getFile();
  }

  private int at(String marker, int shift) {
    String t = myFixture.getEditor().getDocument().getText();
    int i = t.indexOf(marker);
    assertTrue("marker " + marker, i >= 0);
    return i + shift;
  }

  private String gotoAt(int offset) {
    PsiElement[] r = new PsiElement[1];
    List<Failure> f = guarded(() -> r[0] = GotoDeclarationAction.findTargetElement(getProject(), myFixture.getEditor(), offset));
    String s = where(r[0]);
    PsiReference ref = myFixture.getFile().findReferenceAt(offset);
    if (ref instanceof PsiPolyVariantReference poly) {
      List<String> all = new ArrayList<>();
      for (ResolveResult rr : poly.multiResolve(false)) all.add(where(rr.getElement()));
      s += " multi=" + all;
    }
    for (Failure x : f) s += " FAIL " + x.line();
    return s;
  }

  private void writeEdit(String tag, String text) throws Exception {
    Path dir = Paths.get(env("PROBE_OUT"), label(), "v3edits");
    Files.createDirectories(dir);
    Files.writeString(dir.resolve(tag + ".erl"), text);
  }

  private void rename(String file, String marker, int shift, String newName, boolean handler, String tag) throws Exception {
    open(file, fixture(file));
    myFixture.getEditor().getCaretModel().moveToOffset(at(marker, shift));
    List<Failure> f = guarded(() -> {
      if (handler) myFixture.renameElementAtCaretUsingHandler(newName);
      else myFixture.renameElementAtCaret(newName);
    });
    String text = myFixture.getEditor().getDocument().getText();
    boolean changed = !text.equals(fixture(file));
    log("RENAME " + tag + " " + (handler ? "handler" : "processor") + " '" + marker + "'+" + shift + " -> " + newName + " changed=" + changed);
    for (Failure x : f) log("  FAIL " + x.line());
    writeEdit(tag + (handler ? "-h" : "-p"), text);
  }

  private void usages(String what, PsiElement e) {
    if (e == null) { log("USAGES " + what + ": no element"); return; }
    List<String> r = new ArrayList<>();
    List<Failure> f = guarded(() -> {
      for (UsageInfo u : myFixture.findUsages(e)) r.add(where(u.getElement()));
    });
    Collections.sort(r);
    log("USAGES " + what + " of " + where(e) + ": " + r.size() + " " + r);
    for (Failure x : f) log("  FAIL " + x.line());
  }

  private PsiElement varAt(int offset) {
    return PsiTreeUtil.getParentOfType(myFixture.getFile().findElementAt(offset), ErlangQVar.class, false);
  }

  // ------------------------------------------------------------------ part 2.1 mapping
  public void testMapping() throws Exception {
    open("w1_map.erl", fixture("w1_map.erl"));
    String[][] marks = {{"Xa + 1", "0"}, {"Xb.", "0"}, {"{Xc, Xd}", "1"}, {"{Xc, Xd}", "5"}, {"Xe.", "0"}, {"Xf.", "0"},
      {"Xg + Xh", "0"}, {"Xg + Xh", "5"}, {"{Xi, Arg}", "1"}, {"{Xi, Arg}", "5"},
      {"?BIND(Xa", "6"}, {"BIND2(Xb)", "6"}, {"SWAPBIND(Xc", "9"}, {"SWAPBIND(Xc, Xd)", "13"}, {"STRBIND(Xe", "8"},
      {"OUTERB(Xf", "7"}, {"BIND(Xg", "5"}, {"BIND(Xh", "5"}, {"BIND(Xi", "5"}};
    for (String[] m : marks) log("GOTO '" + m[0] + "'+" + m[1] + " -> " + gotoAt(at(m[0], Integer.parseInt(m[1]))));
    String[][] decls = {{"?BIND(Xa", "6"}, {"BIND2(Xb)", "6"}, {"SWAPBIND(Xc", "9"}, {"SWAPBIND(Xc, Xd)", "13"},
      {"STRBIND(Xe", "8"}, {"OUTERB(Xf", "7"}, {"BIND(Xg", "5"}, {"BIND(Xh", "5"}, {"BIND(Xi", "5"}};
    for (String[] m : decls) usages("decl '" + m[0] + "'", varAt(at(m[0], Integer.parseInt(m[1]))));
    String[][] uses = {{"Xa + 1", "0"}, {"Xb.", "0"}, {"{Xc, Xd}", "1"}, {"{Xc, Xd}", "5"}, {"Xe.", "0"}, {"Xf.", "0"},
      {"Xg + Xh", "0"}, {"Xg + Xh", "5"}, {"{Xi, Arg}", "1"}};
    int k = 0;
    for (String[] m : uses) {
      rename("w1_map.erl", m[0], Integer.parseInt(m[1]), "Renamed", false, "map" + k);
      rename("w1_map.erl", m[0], Integer.parseInt(m[1]), "Renamed", true, "map" + k);
      k++;
    }
  }

  // ------------------------------------------------------------------ part 2.5 and 3: functions made by macros
  public void testFunctions() throws Exception {
    PsiFile file = open("w2_fun.erl", fixture("w2_fun.erl"));
    String[][] marks = {{"hello(), world", "0"}, {"world(), hello", "0"}, {"hello()}", "0"}, {"hello/0", "0"}, {"?GETTER(hello)", "8"}};
    for (String[] m : marks) log("GOTO '" + m[0] + "'+" + m[1] + " -> " + gotoAt(at(m[0], Integer.parseInt(m[1]))));
    PsiElement target = GotoDeclarationAction.findTargetElement(getProject(), myFixture.getEditor(), at("hello(), world", 0));
    usages("target of hello()", target);
    for (PsiElement e : new PsiElement[]{target, file.findElementAt(at("?GETTER(hello)", 8)),
      file.findElementAt(at("?GETTER(hello)", 8)).getParent(), file.findElementAt(at("?GETTER(hello)", 8)).getParent().getParent()}) {
      if (e == null) continue;
      String[] type = new String[1];
      List<Failure> f = guarded(() -> type[0] = new ErlangFindUsagesProvider().getType(e));
      log("GETTYPE " + where(e) + " -> " + type[0] + (f.isEmpty() ? "" : " FAIL " + f.get(0).line()));
    }
    rename("w2_fun.erl", "hello(), world", 0, "greet", false, "fun-call");
    rename("w2_fun.erl", "hello(), world", 0, "greet", true, "fun-call");
    rename("w2_fun.erl", "hello/0", 0, "greet", false, "fun-export");
    rename("w2_fun.erl", "hello/0", 0, "greet", true, "fun-export");
    rename("w2_fun.erl", "?GETTER(hello)", 8, "greet", true, "fun-arg");
    // structure view, folding, completion, formatter, unused function
    open("w2_fun.erl", fixture("w2_fun.erl"));
    List<String> sv = new ArrayList<>();
    guarded(() -> {
      StructureViewBuilder b = LanguageStructureViewBuilder.getInstance().getStructureViewBuilder(myFixture.getFile());
      StructureViewModel model = ((TreeBasedStructureViewBuilder)b).createStructureViewModel(myFixture.getEditor());
      for (TreeElement c : model.getRoot().getChildren()) sv.add(c.getPresentation().getPresentableText());
      Disposer.dispose(model);
    });
    log("STRUCTURE " + sv);
    FoldingDescriptor[] ds = LanguageFolding.buildFoldingDescriptors(LanguageFolding.INSTANCE.forLanguage(ErlangLanguage.INSTANCE),
                                                                     myFixture.getFile(), myFixture.getEditor().getDocument(), false);
    log("FOLDING " + ds.length);
    String before = myFixture.getEditor().getDocument().getText();
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { com.intellij.psi.codeStyle.CodeStyleManager.getInstance(getProject()).reformat(myFixture.getFile()); });
    log("FORMAT changed=" + !before.equals(myFixture.getEditor().getDocument().getText()));
    writeEdit("fun-format", myFixture.getEditor().getDocument().getText());
    myFixture.configureByText("w2_comp.erl", "-module(w2_comp).\n-define(GETTER(N), N() -> N).\n?GETTER(hello).\nuse() -> hel<caret>\n");
    LookupElement[] items = myFixture.completeBasic();
    List<String> names = new ArrayList<>();
    if (items != null) for (LookupElement i : items) names.add(i.getLookupString());
    log("COMPLETION after hel: " + (items == null ? "single item inserted: " + myFixture.getEditor().getDocument().getText().replace('\n', ' ') : names));
    myFixture.enableInspections(new ErlangUnusedFunctionInspection());
    myFixture.configureByText("w2_unused.erl", "-module(w2_unused).\n-export([use/0]).\n-define(GETTER(N), N() -> N).\n?GETTER(hello).\n?GETTER(unused_one).\nplain_unused() -> ok.\nuse() -> hello().\n");
    List<String> hl = new ArrayList<>();
    for (var info : myFixture.doHighlighting()) if (info.getDescription() != null) hl.add(info.getSeverity() + " " + info.getDescription());
    log("UNUSED-FUNCTION infos " + hl);
    // remote call: does resolve load the other file's AST?
    for (boolean remote : new boolean[]{false, true}) {
      Disposable d = Disposer.newDisposable();
      boolean set = setRemote(remote, d);
      try {
        String m = "w2_remote" + (remote ? "on" : "off");
        PsiFile other = myFixture.addFileToProject("rem/" + m + ".erl", "-module(" + m + ").\n-export([hello/0]).\n-define(GETTER(N), N() -> N).\n?GETTER(hello).\n");
        boolean astBefore = ((PsiFileImpl)other).getTreeElement() != null;
        open("caller" + remote + ".erl", "-module(caller" + remote + ").\n-export([c/0]).\nc() -> " + m + ":hello().\n");
        String g = gotoAt(at(":hello", 1));
        boolean astAfter = ((PsiFileImpl)other).getTreeElement() != null;
        log("REMOTE switch=" + (set ? remote : "n/a") + " goto=" + g + " astLoadedBefore=" + astBefore + " astLoadedAfter=" + astAfter);
        List<String> hl2 = new ArrayList<>();
        myFixture.enableInspections(new org.intellij.erlang.inspection.ErlangUnresolvedFunctionInspection());
        for (var info : myFixture.doHighlighting()) if (info.getDescription() != null) hl2.add(info.getSeverity() + " " + info.getDescription());
        log("REMOTE infos " + hl2 + " astLoadedAfterHighlight=" + (((PsiFileImpl)other).getTreeElement() != null));
      }
      finally {
        Disposer.dispose(d);
      }
    }
  }

  // ------------------------------------------------------------------ part 2.2 ambiguity and fallback
  public void testAmbiguity() throws Exception {
    String[][] cases = {{"w3_amb.erl", "Am.", "?MK(Am", "4"}, {"w3_amb.erl", "Ra.", "?RD(Ra", "4"}, {"w3_amb.erl", "Rb.", "?RD(Rb", "4"},
      {"w3_only.erl", "Ov.", "?ONLYIF(Ov", "8"}, {"w3_before.erl", "Ub.", "?LATER(Ub", "7"}, {"w3_bad.erl", "Bx.", "?BAD(Bx", "5"},
      {"w3_cap.erl", "Bv.", "?BIG(Bv", "5"}};
    for (String[] c : cases) {
      open(c[0], fixture(c[0]));
      log("GOTO " + c[0] + " '" + c[1] + "' -> " + gotoAt(at(c[1], 0)));
      usages(c[0] + " decl '" + c[2] + "'", varAt(at(c[2], Integer.parseInt(c[3]))));
    }
  }

  // ------------------------------------------------------------------ part 2.3 staleness
  private void edit(String from, String to) {
    Document d = myFixture.getEditor().getDocument();
    int i = d.getText().indexOf(from);
    assertTrue(from, i >= 0);
    WriteCommandAction.runWriteCommandAction(getProject(), "v3 edit", null, () -> { d.replaceString(i, i + from.length(), to); });
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  private String copyState(PsiFile f) {
    Object c = copy(f);
    if (c == null) return "copy=null";
    Object exp = field(c, "expansion");
    Object src = field(exp, "sourceText");
    PsiFile cf = (PsiFile)field(c, "copyFile");
    return "copySourceMatches=" + (src != null && src.toString().contentEquals(f.getText())) + " copyValid=" + (cf != null && cf.isValid()) +
           " builds=" + counter("EXPANSIONS_BUILT");
  }

  public void testStaleness() throws Exception {
    PsiFile file = open("w5_ok.erl", fixture("w5_ok.erl"));
    myFixture.doHighlighting();
    log("S0 " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    Object c0 = copy(file);
    WeakReference<Object> oldCopy = new WeakReference<>(field(c0, "copyFile"));
    c0 = null;
    edit("V = E).", "E).");
    log("S1 define body changed: " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    myFixture.performEditorAction(IdeActions.ACTION_UNDO);
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    log("S2 undo: text restored=" + myFixture.getEditor().getDocument().getText().equals(fixture("w5_ok.erl")) + " " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    myFixture.performEditorAction(IdeActions.ACTION_REDO);
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    log("S3 redo: " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    myFixture.performEditorAction(IdeActions.ACTION_UNDO);
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    edit("?BIND(Xa, 1)", "?BIND(Xz, 1)");
    log("S4 argument changed: " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    edit("?BIND(Xz, 1)", "?BIND(Xa, 1)");
    edit("-module(w5_ok).", "%% far\n%% away\n-module(w5_ok).");
    log("S5 far edit above: " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    edit("k() -> ok.", "k() -> okk.");
    log("S6 far edit below: " + gotoAt(at("Xa + 1", 0)) + " " + copyState(file));
    PsiFile other = myFixture.addFileToProject("w5_other.erl", "-module(w5_other).\n-export([z/0]).\nz() -> 1.\n");
    int b0 = counter("EXPANSIONS_BUILT");
    gotoAt(at("Xa + 1", 0));
    int b1 = counter("EXPANSIONS_BUILT");
    Document od = PsiDocumentManager.getInstance(getProject()).getDocument(other);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { od.insertString(od.getTextLength(), "y() -> 2.\n"); });
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    String g = gotoAt(at("Xa + 1", 0));
    int b2 = counter("EXPANSIONS_BUILT");
    log("S7 edit in another file: " + g + " builds before other edit=" + (b1 - b0) + " builds after other edit=" + (b2 - b1));
    GCUtil.tryGcSoftlyReachableObjects();
    System.gc();
    log("LEAK old copy collected after edits=" + (oldCopy.get() == null));
    myFixture.getEditor().getCaretModel().moveToOffset(at("Xa + 1", 0));
    List<Failure> f = guarded(() -> myFixture.renameElementAtCaretUsingHandler("Yy"));
    log("S8 rename after edits via handler: failures=" + f.size());
    writeEdit("stale-h", myFixture.getEditor().getDocument().getText());
    myFixture.getEditor().getCaretModel().moveToOffset(at("Yy + 1", 0));
    f = guarded(() -> myFixture.renameElementAtCaret("Zz"));
    log("S9 rename again via processor: failures=" + f.size());
    writeEdit("stale-p", myFixture.getEditor().getDocument().getText());
    // a copy of a deleted file
    PsiFile gone = myFixture.addFileToProject("gone/w5_gone.erl", fixture("w5_ok.erl").replace("w5_ok", "w5_gone"));
    copy(gone);
    WeakReference<Object> goneCopy = new WeakReference<>(field(copy(gone), "copyFile"));
    VirtualFile gv = gone.getVirtualFile();
    gone = null;
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { try { gv.delete(this); } catch (Exception e) { throw new RuntimeException(e); } });
    GCUtil.tryGcSoftlyReachableObjects();
    System.gc();
    log("LEAK copy of a deleted file collected=" + (goneCopy.get() == null));
  }

  // ------------------------------------------------------------------ part 2.6 cost (layer only)
  public void testCost() throws Exception {
    String list = env("V3_COST_FILES");
    if (list == null || layer() == null) { log("no cost files or no layer"); return; }
    List<Path> paths = new ArrayList<>();
    for (String l : Files.readAllLines(Paths.get(list))) if (!l.isBlank()) paths.add(Paths.get(l.trim()));
    paths.sort(Comparator.comparingLong((Path p) -> p.toFile().length()).reversed());
    for (Path p : paths.subList(0, Math.min(5, paths.size()))) {
      PsiFile f = myFixture.addFileToProject("big" + (myN++) + "/" + p.getFileName(), read(p));
      List<Long> ns = new ArrayList<>();
      for (int i = 0; i < 6; i++) {
        long t0 = System.nanoTime();
        Object c = com.intellij.openapi.application.ReadAction.compute(() -> build(f));
        long t = System.nanoTime() - t0;
        if (i > 0) ns.add(t / 1_000_000);
        if (i == 0) log("BUILD " + p.getFileName() + " size=" + p.toFile().length() + " copyParsed=" + (field(c, "copyFile") != null));
      }
      List<Long> s = new ArrayList<>(ns);
      Collections.sort(s);
      log("BUILD " + p.getFileName() + " ms median=" + s.get(s.size() / 2) + " all=" + ns);
    }
    // typing in one file while 20 others are open and highlighted
    List<Path> open = new ArrayList<>(paths.subList(5, Math.min(26, paths.size())));
    List<VirtualFile> vfs = new ArrayList<>();
    for (Path p : open) {
      PsiFile f = myFixture.addFileToProject("open" + (myN++) + "/" + p.getFileName(), read(p));
      vfs.add(f.getVirtualFile());
    }
    for (VirtualFile v : vfs) { myFixture.configureFromExistingVirtualFile(v); myFixture.doHighlighting(); }
    int b0 = counter("EXPANSIONS_BUILT");
    long t0 = System.nanoTime();
    for (VirtualFile v : vfs.subList(1, vfs.size())) { myFixture.configureFromExistingVirtualFile(v); myFixture.doHighlighting(); }
    long quiet = (System.nanoTime() - t0) / 1_000_000;
    int b1 = counter("EXPANSIONS_BUILT");
    myFixture.configureFromExistingVirtualFile(vfs.get(0));
    myFixture.getEditor().getCaretModel().moveToOffset(myFixture.getEditor().getDocument().getTextLength());
    long tt = System.nanoTime();
    for (char ch : "\n%% typed\n".toCharArray()) { myFixture.type(ch); PsiDocumentManager.getInstance(getProject()).commitAllDocuments(); myFixture.doHighlighting(); }
    long typing = (System.nanoTime() - tt) / 1_000_000;
    int b2 = counter("EXPANSIONS_BUILT");
    t0 = System.nanoTime();
    for (VirtualFile v : vfs.subList(1, vfs.size())) { myFixture.configureFromExistingVirtualFile(v); myFixture.doHighlighting(); }
    long after = (System.nanoTime() - t0) / 1_000_000;
    int b3 = counter("EXPANSIONS_BUILT");
    log("OPEN20 files=" + vfs.size() + " rehighlight others without edit: builds=" + (b1 - b0) + " ms=" + quiet +
        "; 10 keystrokes+highlight in file 0: builds=" + (b2 - b1) + " ms=" + typing +
        "; rehighlight others after the keystrokes: builds=" + (b3 - b2) + " ms=" + after);
    FileEditorManager.getInstance(getProject()).getOpenFiles();
  }
}
