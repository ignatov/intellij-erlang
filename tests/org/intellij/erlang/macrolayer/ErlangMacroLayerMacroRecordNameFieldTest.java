package org.intellij.erlang.macrolayer;

import com.intellij.codeInsight.TargetElementUtil;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import org.intellij.erlang.psi.ErlangTypedExpr;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.macrolayer.LayerTestSupport.*;

/**
 * #1728: a field of a record whose name is written as a macro ({@code #?R{a = 1}}, {@code #?R{a :: integer()}})
 * resolves to the field of {@code -record(r, ...)}. erlc compiles the module.
 */
public class ErlangMacroLayerMacroRecordNameFieldTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String TEXT = """
    -module(m).
    -export([f/0, g/0]).
    -define(R, r).
    -record(r, {a, b}).
    -type t() :: #?R{a :: integer()}.
    -spec f() -> t().
    f() -> #?R{a = 1}.
    g() -> #r{a = 2}.
    """;

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

  private void assertFieldResolves(String marker) throws Exception {
    PsiFile f = myFixture.addFileToProject("m.erl", TEXT);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    int at = TEXT.indexOf(marker.replace("|", "")) + marker.indexOf('|');
    String target = inBackgroundWithIndicator(() -> {
      PsiReference ref = f.findReferenceAt(at);
      if (ref == null) return "no reference";
      PsiElement t = ref.resolve();
      return t == null ? "null" : t.getContainingFile().getName() + "@" + t.getTextOffset();
    });
    assertEquals(marker, "m.erl@" + (TEXT.indexOf("{a, b}") + 1), target);
  }

  /** Control: the same field with the record name written plainly. */
  public void testFieldInRecordExpressionNamedPlainly() throws Exception {
    assertFieldResolves("#r{|a = 2}");
  }

  public void testFieldInRecordExpressionNamedByMacro() throws Exception {
    assertFieldResolves("#?R{|a = 1}");
  }

  public void testFieldInRecordTypeNamedByMacro() throws Exception {
    assertFieldResolves("#?R{|a :: integer()}");
  }

  /** Rename of the field from its declaration renames the uses in the expression and the type named by the macro. */
  public void testRenameFieldRenamesUsesUnderMacroRecordName() {
    ErlangMacroLayer.setEdtBuildForTests(true, getTestRootDisposable());
    PsiFile f = myFixture.addFileToProject("m.erl", TEXT);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    myFixture.getEditor().getCaretModel().moveToOffset(TEXT.indexOf("{a, b}") + 1);
    PsiElement target = TargetElementUtil.findTargetElement(myFixture.getEditor(),
      TargetElementUtil.ELEMENT_NAME_ACCEPTED | TargetElementUtil.REFERENCED_ELEMENT_ACCEPTED);
    assertNotNull(target);
    myFixture.renameElement(target, "z");
    PsiDocumentManager.getInstance(getProject()).commitAllDocuments();
    assertEquals(TEXT.replace("{a, b}", "{z, b}").replace("{a :: ", "{z :: ").replace("{a = ", "{z = "), f.getText());
  }

  /** Find Usages of the field from its declaration finds the uses under the macro record name and the plain one. */
  public void testFindUsagesOfFieldFindsUsesUnderMacroRecordName() throws Exception {
    PsiFile f = myFixture.addFileToProject("m.erl", TEXT);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    PsiElement field = PsiTreeUtil.getParentOfType(f.findElementAt(TEXT.indexOf("{a, b}") + 1), ErlangTypedExpr.class);
    List<Integer> offsets = inBackgroundWithIndicator(() -> {
      List<Integer> r = new ArrayList<>();
      for (PsiReference ref : ReferencesSearch.search(field).findAll()) {
        r.add(ref.getElement().getTextRange().getStartOffset() + ref.getRangeInElement().getStartOffset());
      }
      Collections.sort(r);
      return r;
    });
    assertEquals(field.getText(), List.of(TEXT.indexOf("{a :: ") + 1, TEXT.indexOf("{a = 1") + 1, TEXT.indexOf("{a = 2") + 1), offsets);
  }

  /** The same uses when the macro and the record come from an included header. */
  public void testFieldsResolveWhenMacroAndRecordComeFromHeader() throws Exception {
    PsiFile h = myFixture.addFileToProject("h.hrl", "-define(R, r).\n-record(r, {a, b}).\n");
    String text = "-module(n).\n-include(\"h.hrl\").\n-export([f/0]).\n-type t() :: #?R{a :: integer()}.\n-spec f() -> t().\nf() -> #?R{a = 1}.\n";
    PsiFile f = myFixture.addFileToProject("n.erl", text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    myFixture.doHighlighting();
    for (String m : List.of("{a :: ", "{a = 1")) {
      int at = text.indexOf(m) + 1;
      String r = inBackgroundWithIndicator(() -> {
        PsiReference ref = f.findReferenceAt(at);
        PsiElement t = ref == null ? null : ref.resolve();
        return t == null ? "null" : t.getContainingFile().getName() + "@" + t.getTextOffset();
      });
      assertEquals(m, "h.hrl@" + (h.getText().indexOf("{a, b}") + 1), r);
    }
  }
}
