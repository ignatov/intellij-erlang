package org.intellij.erlang.navigation;

import com.intellij.psi.*;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * F5 (fuzz): the name in {@code -ifdef(X)}, {@code -ifndef(X)}, {@code -undef(X)} refers to what epp tests there: the
 * definitions of X before the directive (any arity), in the file and in the headers included before it; none when X is
 * not defined yet (a header guard), not a definition after the directive.
 */
public class ErlangMacroDirectiveNameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private List<String> targets(String text) {
    myFixture.configureByText("m.erl", text);
    PsiReference ref = myFixture.getFile().findReferenceAt(myFixture.getCaretOffset());
    assertNotNull(ref);
    List<String> r = new ArrayList<>();
    List<PsiElement> found = new ArrayList<>();
    if (ref instanceof PsiPolyVariantReference p) for (ResolveResult rr : p.multiResolve(false)) found.add(rr.getElement());
    else if (ref.resolve() != null) found.add(ref.resolve());
    for (PsiElement e : found) r.add(e.getContainingFile().getName() + ":" + e.getText());
    return r;
  }

  public void testIfndefAfterTwoArities() {
    assertEquals(List.of("m.erl:-define(M5(X), {X}).", "m.erl:-define(M5(X, Y), [X | Y])."),
                 targets("-module(m).\n-define(M5(X), {X}).\n-define(M5(X, Y), [X | Y]).\n-ifndef(M<caret>5).\n-define(M5, 0).\n-endif.\n"));
  }

  public void testHeaderGuardHasNoTarget() {
    assertEquals(List.of(), targets("-module(m).\n-ifndef(G<caret>UARD).\n-define(GUARD, 1).\n-endif.\n"));
  }

  public void testUndef() {
    assertEquals(List.of("m.erl:-define(S, 1)."), targets("-module(m).\n-define(S, 1).\n-undef(<caret>S).\n-define(S, 2).\n"));
  }

  public void testIfdefSeesAHeaderIncludedBefore() {
    myFixture.addFileToProject("h.hrl", "-define(H, 1).\n");
    assertEquals(List.of("h.hrl:-define(H, 1)."), targets("-module(m).\n-include(\"h.hrl\").\n-ifdef(<caret>H).\n-define(H2, 2).\n-endif.\n"));
  }

  public void testIfdefDoesNotSeeAHeaderIncludedAfter() {
    myFixture.addFileToProject("h.hrl", "-define(H, 1).\n");
    assertEquals(List.of(), targets("-module(m).\n-ifdef(<caret>H).\n-define(H2, 2).\n-endif.\n-include(\"h.hrl\").\n"));
  }
}
