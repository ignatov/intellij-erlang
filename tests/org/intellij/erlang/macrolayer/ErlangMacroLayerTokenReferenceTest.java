package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.codeInsight.daemon.impl.IdentifierHighlightingComputer;
import com.intellij.codeInsight.daemon.impl.IdentifierOccurrence;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.ProperTextRange;
import com.intellij.openapi.util.Segment;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.*;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6: a token of the edited file that the main tree gives no reference, but that has one after expansion:
 * {@code foo} in {@code foo?PAR} with {@code -define(PAR, ())} names a call of foo/0. Go to declaration, identifier
 * highlighting, find usages, rename and the function inspections see it through the expanded copy. Every file here
 * compiles with erlc (OTP 29.1), except where a test says it does not. Markers: '|' is the caret.
 */
public class ErlangMacroLayerTokenReferenceTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String OWNER = ErlangMacroUseTest.OWNER;

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
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
  }

  private void files(String... nameText) {
    for (int i = 0; i < nameText.length; i += 2) myFiles.put(nameText[i], myFixture.addFileToProject("u/" + nameText[i], nameText[i + 1]));
  }

  private int caret(String file, String marker) {
    int bar = marker.indexOf('|');
    String m = marker.substring(0, bar) + marker.substring(bar + 1);
    String text = myFiles.get(file).getText();
    int at = text.indexOf(m);
    assertTrue("no " + m + " in " + file, at >= 0);
    assertEquals("marker is not unique: " + m, -1, text.indexOf(m, at + 1));
    return at + bar;
  }

  private Editor editorAt(String file, int offset) {
    myFixture.configureFromExistingVirtualFile(myFiles.get(file).getVirtualFile());
    myFixture.getEditor().getCaretModel().moveToOffset(offset);
    return myFixture.getEditor();
  }

  static String where(PsiElement t) {
    if (t == null) return "-";
    PsiElement n = t.getNavigationElement();
    PsiFile f = n.getContainingFile();
    int line = StringUtil.offsetToLineNumber(f.getText(), n.getTextOffset()) + 1;
    String text = n.getText();
    if (text.length() > 24) text = text.substring(0, 24) + "..";
    return f.getName() + ":" + line + ":" + text.replace('\n', ' ');
  }

  static String at(PsiFile f, Segment s) {
    int line = StringUtil.offsetToLineNumber(f.getText(), s.getStartOffset()) + 1;
    return f.getName() + ":" + line + ":" + f.getText().substring(s.getStartOffset(), s.getEndOffset());
  }

  /** Go to declaration as the IDE computes it: the reference at the caret, then the go-to handlers. */
  private String gotoDeclaration(String file, String marker) throws Exception {
    PsiFile f = myFiles.get(file);
    int offset = caret(file, marker);
    Editor editor = editorAt(file, offset);
    return inBackground(() -> {
      Set<String> r = new TreeSet<>();
      PsiElement target = TargetElementUtil.findTargetElement(editor, TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED);
      if (target != null) r.add(where(target));
      for (GotoDeclarationHandler h : GotoDeclarationHandler.EP_NAME.getExtensionList()) {
        PsiElement[] t = h.getGotoDeclarationTargets(f.findElementAt(offset), offset, editor);
        if (t != null) for (PsiElement e : t) r.add(where(e));
      }
      return r.isEmpty() ? "NOTHING" : String.join(" ", r);
    });
  }

  /** The identifier highlighting of the caret (the occurrences the editor paints), as "file:line:text". */
  private String identifierHighlighting(String file, String marker) throws Exception {
    PsiFile f = myFiles.get(file);
    int offset = caret(file, marker);
    Editor editor = editorAt(file, offset);
    return inBackground(() -> {
      List<String> r = new ArrayList<>();
      for (IdentifierOccurrence o : new IdentifierHighlightingComputer(f, editor, new ProperTextRange(0, f.getTextLength()), offset)
        .computeRanges().occurrences()) {
        r.add(at(f, o.range()));
      }
      Collections.sort(r);
      return r.toString();
    });
  }

  private PsiElement targetAt(String file, String marker) {
    Editor editor = editorAt(file, caret(file, marker));
    PsiElement t = TargetElementUtil.findTargetElement(editor, TargetElementUtil.ELEMENT_NAME_ACCEPTED | TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED);
    assertNotNull("no target at " + marker, t);
    return t;
  }

  /** Find usages of the element at the caret: the references search in the background, as "file:line:text". */
  private String usages(String file, String marker) throws Exception {
    PsiElement t = targetAt(file, marker);
    return inBackground(() -> {
      List<String> r = new ArrayList<>();
      for (PsiReference ref : ReferencesSearch.search(t).findAll()) {
        PsiElement e = ref.getElement();
        r.add(at(e.getContainingFile(), ref.getRangeInElement().shiftRight(e.getTextRange().getStartOffset())));
      }
      Collections.sort(r);
      return r.toString();
    });
  }

  private void rename(String file, String marker, String newName) {
    myFixture.renameElement(targetAt(file, marker), newName);
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
  }

  private String text(String file) {
    return myFiles.get(file).getText();
  }

  private List<String> problems(String file) {
    myFixture.configureFromExistingVirtualFile(myFiles.get(file).getVirtualFile());
    List<String> r = new ArrayList<>();
    for (HighlightInfo i : myFixture.doHighlighting(HighlightSeverity.WEAK_WARNING)) {
      r.add(at(myFiles.get(file), new ProperTextRange(i.getStartOffset(), i.getEndOffset())) + " " + i.getDescription());
    }
    Collections.sort(r);
    return r;
  }

  // ------------------------------------------------------------------------------------------------ foo in foo?PAR

  public void testCallNameFromFragmentGoesToFunction() throws Exception {
    files("main.erl", OWNER);
    assertEquals("main.erl:16:foo() ->   ok.", gotoDeclaration("main.erl", "f|oo?PAR"));
  }

  public void testCallNameFromFragmentIdentifierHighlighting() throws Exception {
    files("main.erl", OWNER);
    String all = "[main.erl:11:foo, main.erl:12:foo, main.erl:16:foo]";
    assertEquals(all, identifierHighlighting("main.erl", "f|oo?PAR"));
    assertEquals(all, identifierHighlighting("main.erl", "f|oo(),"));
    assertEquals(all, identifierHighlighting("main.erl", "f|oo() ->"));
  }

  public void testFindUsagesOfFunctionListsFragmentUse() throws Exception {
    files("main.erl", OWNER);
    assertEquals("[main.erl:11:foo, main.erl:12:foo]", usages("main.erl", "f|oo() ->"));
  }

  public void testRenameFunctionFromDeclaration() {
    renameFoo("f|oo() ->");
  }

  public void testRenameFunctionFromCall() {
    renameFoo("f|oo(),");
  }

  public void testRenameFunctionFromFragmentUse() {
    renameFoo("f|oo?PAR");
  }

  /** The result compiles with erlc. */
  private void renameFoo(String marker) {
    files("main.erl", OWNER);
    rename("main.erl", marker, "bar");
    assertEquals(OWNER.replace("foo", "bar"), text("main.erl"));
  }

  /** erlc: no warning, foo/0 is used. */
  public void testFunctionUsedOnlyThroughFragmentIsNotUnused() {
    enableAll(myFixture);
    files("main.erl", "-module(main).\n-export([main/0]).\n-define(PAR, ()).\nmain() -> foo?PAR.\nfoo() -> ok.\n");
    assertEquals(List.of(), problems("main.erl"));
  }

  /** erlc: "function foo/0 undefined" (the file does not compile). */
  public void testUndefinedFunctionThroughFragmentIsUnresolved() {
    enableAll(myFixture);
    files("main.erl", "-module(main).\n-export([main/0]).\n-define(PAR, ()).\nmain() -> foo?PAR.\n");
    assertEquals(List.of("main.erl:4:foo Unresolved function foo/0"), problems("main.erl"));
  }

  // ------------------------------------------------------------------------------------------------ other roles

  /** rabbit_variable_queue: {@code St#vqstate{?UP(ram_msg_count, +1)}}; the argument becomes a field name. */
  public void testRecordFieldNamedInMacroArguments() throws Exception {
    files("main.erl", "-module(main).\n-export([f/1]).\n-record(st, {a = 0, b = 0}).\n-define(UP(A, Inc), A = St#st.A + Inc).\n" +
                      "f(St) -> St#st{?UP(a, 1)}.\n");
    assertEquals("main.erl:3:a = 0", gotoDeclaration("main.erl", "?UP(|a, 1)"));
  }

  public void testVariableUsedOnlyThroughFragment() throws Exception {
    enableAll(myFixture);
    files("main.erl", "-module(main).\n-export([f/1]).\n-define(ARGS(X), (X)).\nf(V) -> g?ARGS(V).\ng(X) -> X.\n");
    assertEquals("main.erl:4:V", gotoDeclaration("main.erl", "?ARGS(|V)"));
    assertEquals("[main.erl:4:V, main.erl:4:V]", identifierHighlighting("main.erl", "f(|V)"));
    assertEquals("g/1 is called and V is used", List.of(), problems("main.erl"));
  }
}
