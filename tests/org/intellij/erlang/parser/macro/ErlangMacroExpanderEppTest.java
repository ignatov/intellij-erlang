package org.intellij.erlang.parser.macro;

import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * The expanded text of a form is what epp gives (OTP 29.1, {@code erlc -P}; the probes are named per test). A form
 * that epp rejects is not substituted.
 */
public class ErlangMacroExpanderEppTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private Expansion expand(String text) {
    return ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000);
  }

  /** The tokens of the expanded form that starts with the prefix, joined by single spaces. */
  private String form(Expansion x, String prefix) {
    int at = x.sourceText.toString().indexOf(prefix);
    assertTrue(prefix, at >= 0);
    Form f = x.formAtSource(at);
    assertNotNull(f);
    return tokens(x.text.substring(f.expStart, f.expEnd));
  }

  private String tokens(String text) {
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    List<String> r = new ArrayList<>();
    for (IElementType t; (t = lexer.getTokenType()) != null; lexer.advance()) {
      if (t == TokenType.WHITE_SPACE || ErlangParserDefinition.COMMENTS.contains(t)) continue;
      r.add(lexer.getTokenText());
    }
    return String.join(" ", r);
  }

  private String form(String text, String prefix) {
    return form(expand(text), prefix);
  }

  /** B5 (probe p4): the argument ?PAIR is not expanded before the substitution; ?G gets one argument. epp: one. */
  public void testArgumentIsSubstitutedRaw() {
    assertEquals("a ( ) -> one .", form("""
                                          -module(p4).
                                          -define(PAIR, x, y).
                                          -define(G(X), one).
                                          -define(G(X, Y), two).
                                          -define(CALL(A), ?G(A)).
                                          a() -> ?CALL(?PAIR).
                                          """, "a()"));
  }

  /** B5 (probe p7): ?F(?F) is no circular macro for epp: 1(1), ok, ok. */
  public void testMacroAsItsOwnArgumentIsNotCircular() {
    Expansion x = expand("-module(p7).\n-define(F(X), X(1), ok).\nb(_) -> ?F(?F).\n");
    assertEquals("b ( _ ) -> 1 ( 1 ) , ok , ok .", form(x, "b(_)"));
    assertNull(x.formAtSource(x.sourceText.toString().indexOf("b(_)")).circularMacro);
  }

  /** B5 (probe argpre): ?G(?F) gives ?F(1), so F/1: [1]. The plugin made "; (1)" of F/0. */
  public void testArgumentMacroTakesArgumentsFromTheBody() {
    assertEquals("f ( ) -> [ 1 ] .", form("-module(argpre).\n-define(F, ;).\n-define(F(X), [X]).\n-define(G(X), X(1)).\nf() -> ?G(?F).\n", "f()"));
    assertEquals("f ( ) -> [ 1 ] .", form("-module(argpre2).\n-define(F, x).\n-define(F(X), [X]).\n-define(G(X), X(1)).\nf() -> ?G(?F).\n", "f()"));
  }

  /** B5 (probe circ_arity): F/1 uses F/0, which uses nothing: no cycle. epp: {1, 2}. */
  public void testUseOfAnotherArityIsNoCycle() {
    assertEquals("f ( ) -> { 1 , 2 } .", form("-module(circ_arity).\n-define(F(X), {?F, X}).\n-define(F, 1).\nf() -> ?F(2).\n", "f()"));
  }

  /** B5: a body that uses itself is circular for epp ("circular macro 'F'"), also through another macro. */
  public void testCircularBodies() {
    Expansion x = expand("-module(c).\n-define(F(X), [?F(X)]).\n-define(A, ?B).\n-define(B, {?A}).\nf() -> ?F(1).\ng() -> ?A.\n");
    assertEquals("F", x.formAtSource(x.sourceText.toString().indexOf("f()")).circularMacro);
    assertEquals("A", x.formAtSource(x.sourceText.toString().indexOf("g()")).circularMacro);
  }

  /** B5 (probe p2): ??Y of a name that is not a parameter is the variable Y. epp: [Y, 1]. */
  public void testStringifyOfANonParameter() {
    assertEquals("j ( Y ) -> [ Y , 1 ] .", form("-module(p2).\n-define(T(X), [??Y, X]).\nj(Y) -> ?T(1).\n", "j(Y)"));
  }
}
