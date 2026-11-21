package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
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
 * Round 6: a macro use in a position where the grammar expects no expression ({@code foo?PAR}, a separator between
 * clauses, a body consumed as raw tokens) is bare tokens in the tree. Go to declaration, identifier highlighting, find
 * usages, rename and completion still work on it. Every file here compiles with erlc (OTP 29.1).
 * Markers: '|' is the caret, on the macro name.
 */
public class ErlangMacroUseTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String OWNER = """
    -module(main).
    -author("ignatov").

    %% API
    -export([main/0]).


    -define(PAR, ()).

    main() ->
      foo(),
      foo?PAR,

      io:format("hello").

    foo() ->
      ok.
    """;

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

  // ------------------------------------------------------------------------------------------------ the owner's file

  public void testOwnerGoToDeclaration() throws Exception {
    files("main.erl", OWNER);
    assertEquals("main.erl:8:-define(PAR, ()).", gotoDeclaration("main.erl", "foo?P|AR"));
  }

  public void testOwnerIdentifierHighlighting() throws Exception {
    files("main.erl", OWNER);
    assertEquals("[main.erl:12:PAR, main.erl:8:PAR]", identifierHighlighting("main.erl", "foo?P|AR"));
    assertEquals("[main.erl:12:PAR, main.erl:8:PAR]", identifierHighlighting("main.erl", "define(P|AR"));
  }

  public void testOwnerFindUsages() throws Exception {
    files("main.erl", OWNER);
    assertEquals("[main.erl:12:PAR]", usages("main.erl", "define(P|AR"));
    assertEquals("[main.erl:12:PAR]", usages("main.erl", "foo?P|AR"));
  }

  public void testOwnerRenameFromUse() {
    files("main.erl", OWNER);
    rename("main.erl", "foo?P|AR", "PAR2");
    assertEquals(OWNER.replace("?PAR,", "?PAR2,").replace("define(PAR,", "define(PAR2,"), text("main.erl"));
  }

  public void testOwnerRenameFromDefine() {
    files("main.erl", OWNER);
    rename("main.erl", "define(P|AR", "Par");
    assertEquals(OWNER.replace("?PAR,", "?Par,").replace("define(PAR,", "define(Par,"), text("main.erl"));
  }

  public void testOwnerCompletion() {
    files("main.erl", OWNER.replace("foo?PAR,", "foo?P,"));
    assertEquals("PAR", completeAt("main.erl", "foo?P|,", "PAR"));
  }

  /** Completes at the caret; returns the expected name if it is offered (or inserted as the only one), else the list. */
  private String completeAt(String file, String marker, String expected) {
    editorAt(file, caret(file, marker));
    LookupElement[] items = myFixture.completeBasic();
    String doc = myFixture.getEditor().getDocument().getText();
    if (items == null) return doc.contains("?" + expected) ? expected : "inserted: " + doc;
    List<String> names = new ArrayList<>();
    for (LookupElement i : items) names.add(i.getLookupString());
    return names.contains(expected) ? expected : names.toString();
  }

  // ------------------------------------------------------------------------------------------------ other positions

  public void testSeparatorBetweenClauses() throws Exception {
    String t = "-module(main).\n-export([f/1]).\n-define(SEP, ;).\nf(1) -> a ?SEP f(2) -> b.\n";
    files("main.erl", t);
    assertEquals("main.erl:3:-define(SEP, ;).", gotoDeclaration("main.erl", "a ?S|EP"));
    assertEquals("[main.erl:4:SEP]", usages("main.erl", "define(S|EP"));
    rename("main.erl", "a ?S|EP", "SEMI");
    assertEquals(t.replace("SEP", "SEMI"), text("main.erl"));
  }

  public void testUseWithArgumentsInFragmentPosition() throws Exception {
    String t = "-module(main).\n-export([f/0]).\n-define(TAIL(X), , X).\nf() -> [1 ?TAIL(2)].\n";
    files("main.erl", t);
    assertEquals("main.erl:3:-define(TAIL(X), , X).", gotoDeclaration("main.erl", "1 ?T|AIL"));
    assertEquals("[main.erl:3:TAIL, main.erl:4:TAIL]", identifierHighlighting("main.erl", "1 ?T|AIL"));
  }

  public void testUseInsideArgumentsOfAnotherUse() throws Exception {
    files("main.erl", "-module(main).\n-export([main/0]).\n-define(PAR, ()).\n-define(ID(X), X).\nmain() -> ?ID(foo?PAR).\nfoo() -> ok.\n");
    assertEquals("main.erl:3:-define(PAR, ()).", gotoDeclaration("main.erl", "foo?P|AR"));
    assertEquals("[main.erl:5:PAR]", usages("main.erl", "define(P|AR"));
  }

  public void testUseInsideDefineBody() throws Exception {
    String t = "-module(main).\n-export([main/0]).\n-define(PAR, ()).\n-define(CALLFOO, foo?PAR).\nmain() -> ?CALLFOO.\nfoo() -> ok.\n";
    files("main.erl", t);
    assertEquals("main.erl:3:-define(PAR, ()).", gotoDeclaration("main.erl", "foo?P|AR"));
    rename("main.erl", "define(P|AR", "PAR2");
    assertEquals(t.replace("PAR", "PAR2"), text("main.erl"));
  }

  /** The user is typing: the form has a real syntax error too. */
  public void testUseInBrokenForm() throws Exception {
    files("main.erl", "-module(main).\n-export([main/0]).\n-define(PAR, ()).\nmain() ->\n  foo?PAR,\n  X = ,\n  io:format(\"hello\").\nfoo() -> ok.\n");
    assertEquals("main.erl:3:-define(PAR, ()).", gotoDeclaration("main.erl", "foo?P|AR"));
    assertEquals("[main.erl:3:PAR, main.erl:5:PAR]", identifierHighlighting("main.erl", "foo?P|AR"));
  }

  public void testHeaderMacroUsedInTwoIncluders() throws Exception {
    files("h.hrl", "-define(HPAR, ()).\n",
          "a.erl", "-module(a).\n-include(\"h.hrl\").\n-export([main/0]).\nmain() -> foo?HPAR.\nfoo() -> ok.\n",
          "b.erl", "-module(b).\n-include(\"h.hrl\").\n-export([main/0]).\nmain() -> bar?HPAR.\nbar() -> ok.\n");
    assertEquals("h.hrl:1:-define(HPAR, ()).", gotoDeclaration("a.erl", "foo?H|PAR"));
    assertEquals("[a.erl:4:HPAR, b.erl:4:HPAR]", usages("h.hrl", "define(H|PAR"));
    rename("h.hrl", "define(H|PAR", "HP");
    assertEquals("-define(HP, ()).\n", text("h.hrl"));
    assertTrue(text("a.erl"), text("a.erl").contains("foo?HP."));
    assertTrue(text("b.erl"), text("b.erl").contains("bar?HP."));
  }

  /** epp picks the definition by the number of arguments of the use. */
  public void testArityChoosesTheDefinition() throws Exception {
    files("main.erl", "-module(main).\n-export([main/0]).\n-define(P, ()).\n-define(P(X), (X)).\n" +
                      "main() -> foo?P, foo?P(1).\nfoo() -> ok.\nfoo(X) -> X.\n");
    assertEquals("main.erl:4:-define(P(X), (X)).", gotoDeclaration("main.erl", "foo?|P(1)"));
    assertEquals("main.erl:3:-define(P, ()).", gotoDeclaration("main.erl", "foo?|P,"));
  }

  /** The same choice for a use in an expression position (a real macro element in the tree). */
  public void testArityChoosesTheDefinitionInExpressionPosition() throws Exception {
    files("main.erl", "-module(main).\n-export([main/0]).\n-define(P(X), {X}).\n-define(P, ok).\n" +
                      "main() -> {?P(foo), ?P}.\n");
    assertEquals("main.erl:3:-define(P(X), {X}).", gotoDeclaration("main.erl", "?|P(foo)"));
    assertEquals("main.erl:4:-define(P, ok).", gotoDeclaration("main.erl", "?|P}"));
  }

  public void testClauseRunAfterClauses() throws Exception {
    files("main.erl", "-module(main).\n-export([f/1]).\n-define(CL(A), ;f(A) -> A).\nf(a) -> a\n?CL(b).\n");
    assertEquals("main.erl:3:-define(CL(A), ;f(A) -> ..", gotoDeclaration("main.erl", "?C|L(b)"));
    assertEquals("[main.erl:5:CL]", usages("main.erl", "define(C|L"));
  }

  /** A macro of the same name in an unrelated file is not a usage (master listed its definition). */
  public void testSameNameInAnotherFileIsNotAUsage() throws Exception {
    files("main.erl", OWNER, "other.erl", "-module(other).\n-define(PAR, 1).\n-export([g/0]).\ng() -> ?PAR.\n");
    assertEquals("[main.erl:12:PAR]", usages("main.erl", "define(P|AR"));
  }

}
