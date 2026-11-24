package org.intellij.erlang.parser.macro;

import com.intellij.openapi.util.TextRange;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.DefineShape;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Directive;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.FormStatus;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.HeaderSummary;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.HeaderTok;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

/**
 * The source map of an expansion (forms, uses, the source of expanded ranges, copies of argument, body and header
 * tokens), and what a header summary knows of its directives.
 */
public class ErlangMacroExpanderSourceMapTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String TEXT = """
    -module(m).

    -define(A, {x, y}).
    -define(D(X), X X).
    -define(P(X, Y), X Y).
    -define(S(X), ??X).
    f() -> ?A.
    g() -> ?D(a).
    h() -> ?P(b, c).
    i() -> ?S(d).
    """;

  private Expansion expand(String text) {
    return ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000);
  }

  private HeaderSummary summarize(String text, String name) {
    return ErlangMacroExpander.summarize(text, new ErlangLexer(getProject()), name);
  }

  private static int at(String text, String what) {
    int i = text.indexOf(what);
    assertTrue(what, i >= 0);
    return i;
  }

  /** The offset of the expansion in the expanded text: the first occurrence after the function head. */
  private static int expanded(Expansion x, String head, String what) {
    int i = x.text.indexOf(what, at(x.text, head));
    assertTrue(what, i >= 0);
    return i;
  }

  private static void assertOffsets(int[] actual, int... expected) {
    assertEquals(Arrays.toString(expected), Arrays.toString(actual));
  }

  public void testForms() {
    Expansion x = expand(TEXT);
    Form module = x.formAtSource(0);
    assertNotNull(module);
    assertEquals("Form[0..11 -> 0..11 NO_USES]", module.toString());
    assertTrue(module.containsSource(11));
    assertFalse(module.containsSource(12));
    // the empty line between -module and -define is in no form
    assertNull(x.formAtSource(12));
    assertNull(x.formAtExpanded(x.text.length() + 1));
    Form f = x.formAtExpanded(expanded(x, "f()", "{x, y}"));
    assertNotNull(f);
    assertSame(f, x.formAtSource(at(TEXT, "f()")));
    assertEquals(FormStatus.EXPANDED, f.status);
    assertTrue(x.hasExpansions());
    assertEquals(4, x.expandedFormCount());
    assertEquals(0, expand("-module(m).\nf() -> ok.\n").expandedFormCount());
  }

  public void testUseRanges() {
    Expansion x = expand(TEXT);
    int body = expanded(x, "f()", "{x, y}");
    TextRange use = TextRange.create(at(TEXT, "?A."), at(TEXT, "?A.") + 2);
    assertEquals(use, x.useSourceRange(body + 1));
    assertNull(x.useSourceRange(0));
    assertEquals(use, x.useBefore(0, body + 1));
    assertNull(x.useBefore(0, 0));
    assertNull(x.useBefore(body + 10, body + 11));
  }

  public void testToSource() {
    Expansion x = expand(TEXT);
    int body = expanded(x, "f()", "{x, y}");
    int source = at(TEXT, "{x, y}");
    assertEquals(TextRange.create(source, source + 6), x.toSource(body, body + 6));
    assertEquals(TextRange.create(source + 1, source + 1), x.toSource(body + 1, body + 1));
    assertEquals(ErlangMacroExpander.SRC_BODY, x.kindAt(body));
    // the space put before an expansion has no source
    assertEquals(-1, x.kindAt(body - 1));
    assertNull(x.toSource(body - 1, body - 1));
    assertNull(x.toSource(body - 1, body + 1));
    // copied text and a body token
    assertNull(x.toSource(body - 3, body + 1));
    // the two copies of the argument of ?D(a): the second one does not follow the first one in the source
    int copies = expanded(x, "g()", "a a");
    assertEquals(ErlangMacroExpander.SRC_ARGUMENT, x.kindAt(copies));
    assertEquals(TextRange.create(at(TEXT, "a)"), at(TEXT, "a)") + 1), x.toSource(copies, copies + 1));
    assertNull(x.toSource(copies, copies + 3));
    // the arguments of ?P(b, c): a comma lies between them in the source
    int pair = expanded(x, "h()", "b c");
    assertNull(x.toSource(pair, pair + 3));
    // ??X makes a string that has no source
    int string = expanded(x, "i()", "\"d\"");
    assertEquals(ErlangMacroExpander.SRC_NONE, x.kindAt(string));
    assertNull(x.toSource(string, string + 3));
    assertNull(x.toSource(string + 1, string + 1));
  }

  public void testArgumentAndBodyCopies() {
    Expansion x = expand(TEXT);
    int copies = expanded(x, "g()", "a a");
    assertOffsets(x.argumentCopies(at(TEXT, "a)")), copies, copies + 2);
    assertEquals(0, x.argumentCopies(0).length);
    assertOffsets(x.bodyCopies(at(TEXT, "x, y"), null), expanded(x, "f()", "x, y"));
    // the parameters of a body are replaced by the arguments: no copy of them
    assertEquals(0, x.bodyCopies(at(TEXT, "X X"), null).length);
    assertNull(x.headerOrigin(0, 1));
  }

  public void testHeaderTokens() {
    String text = "-module(m).\n-include(\"h.hrl\").\nf() -> ?H.\n";
    HeaderSummary h = summarize("-define(H, {hx, hy}).\n", "h.hrl");
    Expansion x = ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000, (owner, attribute, string) -> h);
    assertEquals(1, x.headerUses);
    int hx = at(x.text, "{hx, hy}") + 1;
    // the tokens of -define(H, {hx, hy}): hx is the 7th one of the first directive
    HeaderTok origin = new HeaderTok("h.hrl", 0, 6);
    assertEquals(ErlangMacroExpander.SRC_HEADER, x.kindAt(hx));
    assertEquals(origin, x.headerOrigin(hx, hx + 2));
    assertNull(x.headerOrigin(hx, hx + 1));
    assertOffsets(x.bodyCopies(-1, origin), hx);
    assertEquals(0, x.bodyCopies(-1, new HeaderTok("h.hrl", 0, 3)).length);
    assertNull(x.toSource(hx, hx + 2));
    assertNull(x.toSource(hx, hx));
  }

  public void testHeaderDirectives() {
    HeaderSummary h = summarize("""
                                  -include("a.hrl").
                                  -include_lib(foo).
                                  -export([f/0]).
                                  -define(MODULE, x).
                                  -define(X foo).
                                  """, "h.hrl");
    List<String> attributes = new ArrayList<>();
    for (Directive d : h.directives) attributes.add(d.attribute);
    assertOrderedEquals(attributes, "include", "include_lib", "define", "define");
    assertEquals(2, h.defines);
    List<Directive> includes = h.includes();
    assertEquals(2, includes.size());
    assertEquals("a.hrl", includes.get(0).includeString());
    assertNull(includes.get(1).includeString());
    assertEquals("", includes.get(0).name());
    assertNull(h.directives.get(2).includeString());
    // a predefined name and a badly formed -define: shapes that are never plain
    List<DefineShape> shapes = h.defineShapes();
    assertSame(shapes, h.defineShapes());
    assertEquals(2, shapes.size());
    for (int i = 0; i < 2; i++) {
      DefineShape shape = shapes.get(i);
      assertEquals(i == 0 ? "MODULE" : "X", shape.name());
      assertEquals(-1, shape.arity());
      assertFalse(shape.plainBody());
      assertEmpty(shape.params());
      assertEmpty(shape.nestedUses());
    }
  }

  /** Which bodies are "locally plain" (see {@code isPlainBody}). */
  public void testPlainBodies() {
    HeaderSummary h = summarize("""
                                  -define(R(T), receive a -> ok after T -> ok end).
                                  -define(TR(B), try B after ok end).
                                  -define(N(A), A after A).
                                  -define(Q(A), #'r'{f = A}).
                                  -define(Q2(A), #r{(f) = A}).
                                  -define(DR(R), R#?REC.f).
                                  -define(DX(R), R.f).
                                  """, "h.hrl");
    List<DefineShape> shapes = h.defineShapes();
    List<String> plain = new ArrayList<>();
    for (DefineShape s : shapes) plain.add(s.name() + "=" + s.plainBody());
    assertOrderedEquals(plain, "R=false", "TR=true", "N=false", "Q=true", "Q2=false", "DR=true", "DX=false");
    // the timeout after 'after' of receive is a clause head; the body of try ... after is not
    assertEquals(List.of("T"), shapes.get(0).params());
    assertEquals(Set.of("REC"), shapes.get(5).nestedUses());
    for (DefineShape s : shapes) assertEmpty(s.name(), s.freeVars());
  }
}
