package org.intellij.erlang.psi.impl;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * The definitions a macro name refers to, and epp's certain errors at a use. The expected results are erlc's
 * ({@code erlc -P} for the chosen branch, the errors erlc reports).
 */
public class ErlangMacroResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private int myCount;

  private ErlangFile module(String text) {
    return (ErlangFile)myFixture.configureByText("m" + myCount++ + ".erl", text);
  }

  /** Adds the headers (name, text, ...) to a fresh directory and opens the module m.erl there. */
  private ErlangFile moduleWithHeaders(String text, String... headers) {
    String dir = "d" + myCount++ + "/";
    for (int k = 0; k < headers.length; k += 2) myFixture.addFileToProject(dir + headers[k], headers[k + 1]);
    PsiFile f = myFixture.addFileToProject(dir + "m.erl", text);
    myFixture.configureFromExistingVirtualFile(f.getVirtualFile());
    return (ErlangFile)f;
  }

  private static List<String> texts(List<ErlangMacrosDefinition> definitions) {
    List<String> r = new ArrayList<>();
    for (ErlangMacrosDefinition d : definitions) r.add(d.getText());
    return r;
  }

  private static PsiElement leafAt(PsiFile file, String marker) {
    PsiElement leaf = file.findElementAt(file.getText().indexOf(marker.replace("|", "")) + marker.indexOf('|'));
    assertNotNull(marker, leaf);
    return leaf;
  }

  private static ErlangAttribute attributeAt(PsiFile file, String marker) {
    ErlangAttribute a = PsiTreeUtil.getParentOfType(leafAt(file, marker), ErlangAttribute.class);
    assertNotNull(marker, a);
    return a;
  }

  private static List<String> resolveAllBare(ErlangFile file, String name) {
    return texts(ErlangMacroResolve.resolveAll(file, name, ErlangMacroResolve.NO_ARGUMENTS));
  }

  public void testMatchingWithoutAUseChoosesByArity() {
    ErlangFile f = module("-module(m).\n-define(M, none).\n-define(M(A), one).\n-define(M(A, B), two).\n-define(N(A), n1).\n" +
                          "-define(K, k).\n");
    assertEquals(List.of("-define(M(A), one)."), texts(ErlangMacroResolve.matching(f, "M", 1)));
    assertEquals(List.of("-define(M(A, B), two)."), texts(ErlangMacroResolve.matching(f, "M", 2)));
    assertEquals(List.of("-define(M, none)."), texts(ErlangMacroResolve.matching(f, "M", ErlangMacroResolve.NO_ARGUMENTS)));
    // ?K(a, b): the only definition has no parameter list, epp expands it and leaves the parentheses as a call
    assertEquals(List.of("-define(K, k)."), texts(ErlangMacroResolve.matching(f, "K", 2)));
    assertEquals(List.of("-define(M, none).", "-define(M(A), one).", "-define(M(A, B), two)."),
                 texts(ErlangMacroResolve.matching(f, "M", ErlangMacroResolve.UNKNOWN)));
    // ?N(): argument mismatch, no definition fits
    assertEquals(List.of(), texts(ErlangMacroResolve.matching(f, "N", 0)));
    assertEquals(List.of(), texts(ErlangMacroResolve.matching(f, "Z", 1)));
  }

  public void testResolveAllWithoutAUseIfdefOfAMacroDefinedBefore() {
    ErlangFile f = module("-module(m).\n-define(TEST, 1).\n-ifdef(TEST).\n-define(X, a).\n-else.\n-define(X, b).\n-endif.\n");
    assertEquals(List.of("-define(X, a)."), resolveAllBare(f, "X"));
  }

  public void testResolveAllWithoutAUseIfndefOfAMacroDefinedBefore() {
    ErlangFile f = module("-module(m).\n-define(TEST, 1).\n-ifndef(TEST).\n-define(X, a).\n-else.\n-define(X, b).\n-endif.\n");
    assertEquals(List.of("-define(X, b)."), resolveAllBare(f, "X"));
  }

  /** TEST may come from the command line (-DTEST): either branch. */
  public void testResolveAllWithoutAUseIfdefOfAMacroTheFileDoesNotDefine() {
    ErlangFile f = module("-module(m).\n-ifdef(TEST).\n-define(X, a).\n-else.\n-define(X, b).\n-endif.\n");
    assertEquals(List.of("-define(X, a).", "-define(X, b)."), resolveAllBare(f, "X"));
  }

  /** After -undef(T), -ifdef(T) is no longer known to be taken; the definition outside every branch is the one. */
  public void testResolveAllWithoutAUseUndefEndsTheDefinitionForIfdef() {
    ErlangFile f = module("-module(m).\n-define(T, 1).\n-undef(T).\n-ifdef(T).\n-define(X, a).\n-endif.\n-define(X, b).\n");
    assertEquals(List.of("-define(X, b)."), resolveAllBare(f, "X"));
  }

  public void testResolveAllWithoutAUseInnerBranchOfAnInactiveBranch() {
    ErlangFile f = module("-module(m).\n-define(T, 1).\n-ifndef(T).\n-ifdef(T).\n-define(X, a).\n-endif.\n-else.\n-define(X, b).\n-endif.\n");
    assertEquals(List.of("-define(X, b)."), resolveAllBare(f, "X"));
  }

  public void testResolveAllWithoutAUseIfdefOfAPredefinedMacro() {
    String branches = "-define(X, a).\n-else.\n-define(X, b).\n-endif.\n";
    assertEquals(List.of("-define(X, a)."), resolveAllBare(module("-module(m).\n-ifdef(MACHINE).\n" + branches), "X"));
    assertEquals(List.of("-define(X, a)."), resolveAllBare(module("-module(m).\n-ifdef(MODULE).\n" + branches), "X"));
  }

  /** The plugin does not evaluate -if and -elif conditions: every branch. */
  public void testResolveAllWithoutAUseIfAndElifBranches() {
    ErlangFile f = module("-module(m).\n-if(?OTP_RELEASE >= 99).\n-define(X, a).\n-elif(?OTP_RELEASE >= 20).\n-define(X, b).\n" +
                          "-else.\n-define(X, c).\n-endif.\n");
    assertEquals(List.of("-define(X, a).", "-define(X, b).", "-define(X, c)."), resolveAllBare(f, "X"));
  }

  /**
   * Both definitions are in a branch that erlc skips (a use is "undefined macro 'X'"); go to declaration still offers
   * them, as the plugin does when no definition is in force.
   */
  public void testResolveAllWithoutAUseEveryDefinitionInactive() {
    ErlangFile f = module("-module(m).\n-define(T, 1).\n-ifndef(T).\n-define(X, a).\n-define(X, b).\n-endif.\n");
    for (ErlangMacrosDefinition d : ErlangMacroResolve.candidates(f, "X")) {
      assertEquals(d.getText(), ErlangMacroResolve.INACTIVE, ErlangMacroResolve.branchState(d));
    }
    assertEquals(List.of("-define(X, a).", "-define(X, b)."), resolveAllBare(f, "X"));
  }

  /** A branch that the file decides as skipped stays skipped inside an -if: erlc -P gives b. */
  public void testResolveAllWithoutAUseInactiveBranchInsideAnIf() {
    ErlangFile f = module("-module(m).\n-define(T, 1).\n-if(true).\n-ifndef(T).\n-define(X, a).\n-endif.\n-endif.\n-define(X, b).\n");
    List<ErlangMacrosDefinition> c = ErlangMacroResolve.candidates(f, "X");
    assertEquals(ErlangMacroResolve.INACTIVE, ErlangMacroResolve.branchState(c.get(0)));
    assertEquals(ErlangMacroResolve.ACTIVE, ErlangMacroResolve.branchState(c.get(1)));
    assertEquals(List.of("-define(X, b)."), resolveAllBare(f, "X"));
  }

  /** The same inside -ifdef of a flag the file does not define: erlc -P gives b with and without -DUNKNOWN_FLAG. */
  public void testResolveAllWithoutAUseInactiveBranchInsideAnUnknownIfdef() {
    ErlangFile f = module("-module(m).\n-define(T, 1).\n-ifdef(UNKNOWN_FLAG).\n-ifndef(T).\n-define(X, a).\n-endif.\n-endif.\n" +
                          "-define(X, b).\n");
    List<ErlangMacrosDefinition> c = ErlangMacroResolve.candidates(f, "X");
    assertEquals(ErlangMacroResolve.INACTIVE, ErlangMacroResolve.branchState(c.get(0)));
    assertEquals(ErlangMacroResolve.ACTIVE, ErlangMacroResolve.branchState(c.get(1)));
    assertEquals(List.of("-define(X, b)."), resolveAllBare(f, "X"));
  }

  public void testResolveAllWithoutAUseOneDefinition() {
    ErlangFile f = module("-module(m).\n-define(X, a).\n-define(Y(A), A).\n");
    assertEquals(List.of("-define(X, a)."), resolveAllBare(f, "X"));
    assertEquals(List.of(), resolveAllBare(f, "Y"));
  }

  /** A directive sees the definitions of the headers included before it, also those of a header that a header includes. */
  public void testDefinedBeforeSeesTheHeadersOfAHeader() {
    ErlangFile f = moduleWithHeaders("-module(m).\n-ifdef(X).\n-endif.\n-include(\"x.hrl\").\n-include(\"h.hrl\").\n-ifdef(X).\n-endif.\n",
                                     "h.hrl", "-include(\"x.hrl\").\n-include_lib(\"y.hrl\").\n",
                                     "x.hrl", "-ifndef(X_HRL).\n-define(X_HRL, true).\n-define(X, 1).\n-endif.\n",
                                     "y.hrl", "-define(X(A), A).\n");
    ErlangAttribute first = attributeAt(f, "-if|def(X).\n-endif.\n-include");
    ErlangAttribute second = attributeAt(f, "h.hrl\").\n-if|def(X)");
    assertEquals(List.of(), texts(ErlangMacroResolve.definedBefore(f, first, "X")));
    assertEquals(List.of("-define(X, 1).", "-define(X(A), A)."), texts(ErlangMacroResolve.definedBefore(f, second, "X")));
    assertEquals(List.of(), texts(ErlangMacroResolve.definedBefore(f, second, "Y")));
  }

  /** A header guard's name goes to its definition in the body; a nested -if and its -endif do not end the body. */
  public void testDirectiveTargetsOfAGuardWithANestedIf() {
    ErlangFile f = module("-module(m).\n-ifndef(G).\n-if(?MODULE == m).\n-define(H, 1).\n-endif.\n-define(G, 2).\n-endif.\n" +
                          "-undef(G).\n-define(G, 3).\n");
    ErlangAttribute guard = attributeAt(f, "-if|ndef(G)");
    assertEquals(List.of("-define(G, 2)."), texts(ErlangMacroResolve.directiveTargets(f, guard, "G")));
  }

  /** ?FEATURE_ENABLED(a, b) next to an own definition without parameters: erlc says "argument mismatch". */
  public void testFeatureArgumentMismatchWithHeadersThatIncludeHeaders() {
    ErlangFile f = moduleWithHeaders("-module(m).\n-export([f/0]).\n-include(\"h.hrl\").\n-define(FEATURE_ENABLED, x).\n" +
                                     "f() -> ?FEATURE_ENABLED(a, b).\n",
                                     "h.hrl", "-include(\"h2.hrl\").\n-include_lib(\"h3.hrl\").\n",
                                     "h2.hrl", "-define(OTHER, 1).\n",
                                     "h3.hrl", "-define(MORE, 1).\n");
    assertTrue(ErlangMacroResolve.isFeatureArgumentMismatch(leafAt(f, "?F|EATURE_ENABLED(a"), "FEATURE_ENABLED"));
  }

  /** An -undef in a header that a header includes may remove the predefined definition: nothing is certain. */
  public void testFeatureArgumentMismatchNotCertainAfterAnUndefInANestedHeader() {
    ErlangFile f = moduleWithHeaders("-module(m).\n-export([f/0]).\n-include(\"h.hrl\").\n-define(FEATURE_ENABLED, x).\n" +
                                     "f() -> ?FEATURE_ENABLED(a, b).\n",
                                     "h.hrl", "-include(\"h2.hrl\").\n-include_lib(\"u.hrl\").\n",
                                     "h2.hrl", "-define(OTHER, 1).\n",
                                     "u.hrl", "-undef(FEATURE_ENABLED).\n");
    assertFalse(ErlangMacroResolve.isFeatureArgumentMismatch(leafAt(f, "?F|EATURE_ENABLED(a"), "FEATURE_ENABLED"));
  }

  /** An -undef in a header included outside every conditional of the chain: erlc says "undefined macro 'FEATURE_ENABLED/1'". */
  public void testFeatureMacroUndefinedByANestedHeader() {
    ErlangFile f = moduleWithHeaders("-module(m).\n-export([f/0]).\n-include(\"h.hrl\").\nf() -> ?FEATURE_ENABLED(maps).\n",
                                     "h.hrl", "-include(\"o.hrl\").\n-include(\"u.hrl\").\n",
                                     "o.hrl", "-define(O, 1).\n",
                                     "u.hrl", "-undef(FEATURE_ENABLED).\n");
    assertTrue(ErlangMacroResolve.isFeatureMacroUndefined(leafAt(f, "?F|EATURE_ENABLED(maps"), "FEATURE_ENABLED"));
  }

  /** The header that has the -undef is included only when FOO is defined: erlc compiles the module. */
  public void testFeatureMacroNotUndefinedByAHeaderIncludedInAConditional() {
    ErlangFile f = moduleWithHeaders("-module(m).\n-export([f/0]).\n-include(\"h.hrl\").\nf() -> ?FEATURE_ENABLED(maps).\n",
                                     "h.hrl", "-ifdef(FOO).\n-include(\"u.hrl\").\n-endif.\n-include(\"o.hrl\").\n",
                                     "o.hrl", "-define(O, 1).\n",
                                     "u.hrl", "-undef(FEATURE_ENABLED).\n");
    assertFalse(ErlangMacroResolve.isFeatureMacroUndefined(leafAt(f, "?F|EATURE_ENABLED(maps"), "FEATURE_ENABLED"));
  }

  public void testArityAfterAnArgumentListThatTheFileEndsIn() {
    ErlangFile f = module("-module(m).\n-define(M(A, B), {A, B}).\nf() -> ?M(a, b");
    assertEquals(ErlangMacroResolve.UNKNOWN, ErlangMacroResolve.arityAfter(leafAt(f, "?|M(a")));
  }

  public void testNameToken() {
    PsiElement var = ErlangMacroResolve.nameToken(getProject(), "Foo");
    assertNotNull(var);
    assertEquals(ErlangTypes.ERL_VAR, var.getNode().getElementType());
    assertEquals("Foo", var.getText());
    PsiElement atom = ErlangMacroResolve.nameToken(getProject(), "foo");
    assertNotNull(atom);
    assertEquals(ErlangTypes.ERL_ATOM_NAME, atom.getNode().getElementType());
    assertEquals("foo", atom.getText());
    // names that need quotes
    assertNull(ErlangMacroResolve.nameToken(getProject(), "1"));
    assertNull(ErlangMacroResolve.nameToken(getProject(), ""));
    assertNull(ErlangMacroResolve.nameToken(getProject(), "-"));
  }

  /** A bare use foo?'p q' is renamed with its quotes: erlc compiles both texts. */
  public void testRenameOfAQuotedBareUse() {
    module("-module(m).\n-export([f/0, foo/0]).\n-define('p q', ()).\nf() -> foo?'p q'.\nfoo() -> ok.\n");
    String text = myFixture.getFile().getText();
    myFixture.getEditor().getCaretModel().moveToOffset(text.indexOf("-define('p q'") + "-define('p".length());
    myFixture.renameElementAtCaret("PAR");
    assertEquals("-module(m).\n-export([f/0, foo/0]).\n-define(PAR, ()).\nf() -> foo?PAR.\nfoo() -> ok.\n", myFixture.getFile().getText());
  }
}
