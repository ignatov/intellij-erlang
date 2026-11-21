package org.intellij.erlang.parser.macro;

import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Form;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The expanded text of a form is what epp gives (OTP 29.1, {@code erlc -P}; the probes are named per test). A form
 * that epp rejects is not substituted.
 */
public class ErlangMacroExpanderEppTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private Expansion expand(String text) {
    return ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000);
  }

  /** Expands with headers: name, text, name, text, ... (an include string is a header name). */
  private Expansion expand(String text, String... headers) {
    Map<String, ErlangMacroExpander.HeaderSummary> map = new HashMap<>();
    for (int i = 0; i < headers.length; i += 2) {
      map.put(headers[i], ErlangMacroExpander.summarize(headers[i + 1], new ErlangLexer(getProject()), headers[i]));
    }
    return ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000, (owner, attribute, string) -> map.get(string));
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

  private static final String[] PREDEFINED = {"MODULE", "MODULE_STRING", "FILE", "LINE", "MACHINE", "BEAM", "OTP_RELEASE",
    "FEATURE_AVAILABLE", "FEATURE_ENABLED", "FUNCTION_NAME", "FUNCTION_ARITY", "BASE_MODULE", "BASE_MODULE_STRING"};

  /**
   * B6 (probes p3, b6): which predefined macros -ifdef sees, before and after -module. epp: MODULE and MODULE_STRING
   * only after -module; FUNCTION_NAME, FUNCTION_ARITY, BASE_MODULE, BASE_MODULE_STRING never (no -extends).
   */
  public void testIfdefOfPredefinedMacros() {
    StringBuilder text = new StringBuilder();
    for (int i = 0; i < PREDEFINED.length; i++) {
      text.append("-ifdef(").append(PREDEFINED[i]).append("). -define(B").append(i).append(", yes). -else. -define(B").append(i).append(", no). -endif.\n");
    }
    text.append("-module(b6).\n");
    for (int i = 0; i < PREDEFINED.length; i++) {
      text.append("-ifdef(").append(PREDEFINED[i]).append("). -define(A").append(i).append(", yes). -else. -define(A").append(i).append(", no). -endif.\n");
    }
    List<String> uses = new ArrayList<>();
    for (int i = 0; i < PREDEFINED.length; i++) uses.add("{'" + PREDEFINED[i] + "', ?B" + i + ", ?A" + i + "}");
    text.append("f() -> [").append(String.join(", ", uses)).append("].\n");
    String expected = "f ( ) -> [ { ' MODULE ' , no , yes } , { ' MODULE_STRING ' , no , yes } , { ' FILE ' , yes , yes } , " +
                      "{ ' LINE ' , yes , yes } , { ' MACHINE ' , yes , yes } , { ' BEAM ' , yes , yes } , " +
                      "{ ' OTP_RELEASE ' , yes , yes } , { ' FEATURE_AVAILABLE ' , yes , yes } , " +
                      "{ ' FEATURE_ENABLED ' , yes , yes } , { ' FUNCTION_NAME ' , no , no } , " +
                      "{ ' FUNCTION_ARITY ' , no , no } , { ' BASE_MODULE ' , no , no } , { ' BASE_MODULE_STRING ' , no , no } ] .";
    assertEquals(expected, form(text.toString(), "f()"));
  }

  /** B6: -ifndef of BASE_MODULE is taken. */
  public void testIfndefOfBaseModule() {
    assertEquals("a ( ) -> yes .", form("-module(p3).\n-ifndef(BASE_MODULE). -define(A, yes). -else. -define(A, no). -endif.\na() -> ?A.\n", "a()"));
  }

  /** B7 (probe p8, b7): a default after the definition: the -ifndef branch is not taken. epp: 5000. */
  public void testDefaultAfterTheDefinition() {
    assertEquals("t ( ) -> 5000 .", form("-module(b7).\n-define(TIMEOUT, 5000).\n-ifndef(TIMEOUT).\n-define(TIMEOUT, 1000).\n-endif.\nt() -> ?TIMEOUT.\n", "t()"));
  }

  /** B7 (probe b7b): the -ifndef(T) branch is not taken, ?T has no definition of its arity: epp "argument mismatch". */
  public void testDefaultOfAnotherArityIsNotTaken() {
    Expansion x = expand("-module(b7b).\n-define(T(X), X).\n-ifndef(T).\n-define(T, 1).\n-endif.\nt() -> ?T.\n");
    assertEquals("t ( ) -> ? T .", form(x, "t()"));
    assertNull(x.outcomes.get(ErlangMacroExpander.Outcome.SUBSTITUTED));
  }

  /** B7 (probe b7c): the -else of a known -ifndef, and a known -ifdef before -elif and -else. epp: {some, yes}. */
  public void testElseOfAKnownCondition() {
    assertEquals("t ( ) -> { some , yes } .", form("""
                                                     -module(b7c).
                                                     -define(LEVEL, 1).
                                                     -ifndef(LEVEL).
                                                     -define(LOG, none).
                                                     -else.
                                                     -define(LOG, some).
                                                     -endif.
                                                     -ifdef(LEVEL).
                                                     -define(L2, yes).
                                                     -elif(true).
                                                     -define(L2, maybe).
                                                     -else.
                                                     -define(L2, no).
                                                     -endif.
                                                     t() -> {?LOG, ?L2}.
                                                     """, "t()"));
  }

  /** B7: an included header whose default comes after the file's own definition. epp: 5000. */
  public void testHeaderDefaultAfterTheDefinition() {
    Expansion x = expand("-module(m).\n-define(TIMEOUT, 5000).\n-include(\"d.hrl\").\nt() -> ?TIMEOUT.\n",
                         "d.hrl", "-ifndef(TIMEOUT).\n-define(TIMEOUT, 1000).\n-endif.\n");
    assertEquals("t ( ) -> 5000 .", form(x, "t()"));
  }

  /** B8 (probe b8): -undef inside a guarded header. epp: mine(a). */
  public void testUndefInsideAGuardedHeader() {
    Expansion x = expand("-module(b8).\n-include(\"a.hrl\").\n-include(\"b.hrl\").\nt() -> ?LOG(a).\n",
                         "a.hrl", "-define(LOG(X), theirs(X)).\n",
                         "b.hrl", "-ifndef(B_HRL).\n-define(B_HRL, 1).\n-undef(LOG).\n-define(LOG(X), mine(X)).\n-endif.\n");
    assertEquals("t ( ) -> mine ( a ) .", form(x, "t()"));
  }

  /** B8: -undef inside an unknown branch still does nothing (the branch may not be taken). */
  public void testUndefInsideAnUnknownBranch() {
    Expansion x = expand("-module(m).\n-define(LOG, a).\n-ifdef(DEBUG).\n-undef(LOG).\n-endif.\nt() -> ?LOG.\n");
    assertEquals("t ( ) -> a .", form(x, "t()"));
  }
}
