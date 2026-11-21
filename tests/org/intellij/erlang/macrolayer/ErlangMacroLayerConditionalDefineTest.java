package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * Round 6 (next), item 4: a macro with definitions in {@code -ifdef}/{@code -else} branches. When the active branch can
 * be told from the file (the condition's macro is defined earlier outside any branch), a use resolves to the active
 * definition only; when it cannot (a build flag such as {@code -DTEST}), to every definition of the use's arity, so go
 * to declaration offers them all. Find usages from either definition finds the uses; rename renames both definitions.
 */
public class ErlangMacroLayerConditionalDefineTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private PsiFile myFile;

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return descriptor();
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
    ErlangMacroLayer.setModeForTests(ErlangMacroLayer.Mode.LAYER, getTestRootDisposable());
  }

  private List<HighlightInfo> open(String name, String text) {
    myFile = myFixture.addFileToProject("i/" + name, text);
    myFixture.configureFromExistingVirtualFile(myFile.getVirtualFile());
    return myFixture.doHighlighting();
  }

  private String line(PsiElement e) {
    return String.valueOf(StringUtil.offsetToLineNumber(myFile.getText(), e.getTextRange().getStartOffset()) + 1);
  }

  /** The lines of the definitions that the use at the marker resolves to (all results of a poly-variant reference). */
  private String targets(String marker) {
    int at = myFile.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|');
    PsiReference ref = myFile.findReferenceAt(at);
    assertNotNull(marker, ref);
    List<String> r = new ArrayList<>();
    for (PsiReference one : ref instanceof com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference m ? m.getReferences() : new PsiReference[]{ref}) {
      if (one instanceof PsiPolyVariantReference p) {
        for (ResolveResult rr : p.multiResolve(false)) {
          if (rr.getElement() instanceof ErlangMacrosDefinition) r.add(line(rr.getElement()));
        }
      }
      else if (one.resolve() instanceof ErlangMacrosDefinition d) {
        r.add(line(d));
      }
    }
    Collections.sort(r);
    return r.toString();
  }

  private String usages(int define) throws Exception {
    ErlangMacrosDefinition d = ((ErlangFile)myFile).getMacroses().get(define);
    return inBackgroundWithIndicator(() -> {
      List<String> r = new ArrayList<>();
      for (PsiReference ref : ReferencesSearch.search(d).findAll()) {
        TextRange abs = ref.getRangeInElement().shiftRight(ref.getElement().getTextRange().getStartOffset());
        r.add((StringUtil.offsetToLineNumber(myFile.getText(), abs.getStartOffset()) + 1) + ":" + abs.substring(myFile.getText()));
      }
      Collections.sort(r);
      return r.toString();
    });
  }

  private static final String UNKNOWN = """
    -module(%s).
    -export([f/0, foo/0]).
    -ifdef(TEST).
    -define(LOG(X), {test, X}).
    -else.
    -define(LOG(X), {prod, X}).
    -endif.
    foo() -> ok.
    f() -> ?LOG(1).
    """;

  public void testUnknownBranchOffersBothDefinitions() throws Exception {
    open("u4.erl", UNKNOWN.formatted("u4"));
    assertEquals("[4, 6]", targets("?L|OG(1)"));
    assertEquals("[9:LOG]", usages(0));
    assertEquals("[9:LOG]", usages(1));
  }

  /** N2 (from the real IDE): the go-to-declaration action's targets (the path of GotoDeclarationAction), not only multiResolve. */
  public void testGoToDeclarationActionOffersBothDefinitions() {
    open("ifd.erl", "-module(ifd).\n-export([f/0]).\n\n-ifdef(X).\n-define(A, 1).\n-else.\n-define(A, 2).\n-endif.\n\nf() -> ?A.\n");
    int at = myFile.getText().indexOf("?A.") + 1;
    myFixture.getEditor().getCaretModel().moveToOffset(at);
    // the targets that the action (GotoDeclarationOrUsageHandler2) goes to: the symbols at the caret
    List<String> lines = new ArrayList<>();
    for (com.intellij.model.Symbol sym : com.intellij.model.psi.impl.TargetsKt.targetSymbols(myFile, at)) {
      PsiElement t = com.intellij.model.psi.PsiSymbolService.getInstance().extractElementFromSymbol(sym);
      if (t != null) lines.add(line(t));
    }
    Collections.sort(lines);
    assertEquals("[5, 7]", lines.toString());
  }

  public void testKnownBranchGoesToTheActiveDefinition() {
    open("k4.erl", """
      -module(k4).
      -export([f/0, g/0]).
      -define(DEBUG, true).
      -ifdef(DEBUG).
      -define(LOG(X), {debug, X}).
      -else.
      -define(LOG(X), ok).
      -endif.
      -ifndef(DEBUG).
      -define(TAG, prod).
      -else.
      -define(TAG, debug).
      -endif.
      f() -> ?LOG(1).
      g() -> ?TAG.
      """);
    assertEquals("[5]", targets("?L|OG(1)"));
    assertEquals("[12]", targets("?T|AG."));
  }

  public void testRenameRenamesBothDefinitions() {
    open("r4.erl", UNKNOWN.formatted("r4"));
    myFixture.renameElement(((ErlangFile)myFile).getMacroses().get(1), "TRACE");
    assertEquals(UNKNOWN.formatted("r4").replace("LOG", "TRACE"), myFile.getText());
  }

  /** Row R11 of the case table: a fragment macro defined in both branches; erlc compiles the file in either case. */
  public void testFragmentDefinedInBothBranchesHasNoFalseError() {
    List<HighlightInfo> infos = open("e4.erl", """
      -module(e4).
      -export([main/0, foo/0]).
      -ifdef(TEST).
      -define(PAR, ()).
      -else.
      -define(PAR, ()).
      -endif.
      main() ->
        foo?PAR,
        ok.
      foo() -> ok.
      """);
    List<String> errors = new ArrayList<>();
    for (HighlightInfo h : infos) if (h.getSeverity() == HighlightSeverity.ERROR) errors.add(h.getDescription());
    assertEquals(List.of(), errors);
    assertEquals("[4, 6]", targets("foo?P|AR"));
  }
}
