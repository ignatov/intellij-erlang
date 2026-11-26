package org.intellij.erlang.parser.macro;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

/**
 * The arguments of a macro use are split as epp's macro_arg/3 splits them (OTP 29.1). The expected counts are erlc's:
 * a module with {@code -define(N(), zero). -define(N(A), one). -define(N(A, B), two). ...} and {@code fK() -> ?N(shape).}
 * per shape, preprocessed with {@code erlc -P} (probe {@code b4.erl}).
 */
public class ErlangMacroArgSplitterTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  static final String[][] SHAPES = {
    {"fun()", "1"},
    {"fun((a, b) -> ok)", "1"},
    {"fun(() -> ok)", "1"},
    {"fun(), c", "2"},
    {"fun L(X) -> X end, 2", "2"},
    {"fun(X) -> X end, 2", "2"},
    {"fun(X) when X > 0, true -> X end, 2", "2"},
    {"fun foo/1, 2", "2"},
    {"fun m:f/1, b", "2"},
    {"[a, b], {c, d}", "2"},
    {"<<1, 2>>, x", "2"},
    {"begin a, b end, c", "2"},
    {"case X of a -> 1; _ -> 2 end, y", "2"},
    {"\"a, b\", $,", "2"},
    {"'a,b', $)", "2"},
    {"a, % comment, here\nb", "2"},
    {"(a, b)", "1"},
    {"try a catch _:_ -> b end, c", "2"},
    {"receive X -> X after 0 -> ok end, z", "2"},
    {"if true -> a end, b", "2"},
    {"maybe ok ?= a end, b", "2"},
    {"#{a => b}, c", "2"},
    {"a]", "1"},
    {"cond, b end", "1"},
    {"'cond', b", "2"},
    {"fun((a) -> b), c", "2"},
    {"fun() -> ok end", "1"},
    {"fun (A) -> A end(1), 2", "2"},
    {"[fun() -> a end, x], y", "2"},
    {"fun(), fun()", "2"},
    {"fun F() -> ok end", "1"},
    {"fun() when true -> ok end, x", "2"},
    {"{fun(), b}, c", "2"},
    {"fun(a) , b", "2"},
    {"<<\"x\">>, $(", "2"},
    {"", "0"},
  };

  private List<ErlangMacroExpander.Tok> tokens(String text) {
    ErlangLexer lexer = new ErlangLexer(getProject());
    lexer.start(text);
    List<ErlangMacroExpander.Tok> r = new ArrayList<>();
    for (IElementType t; (t = lexer.getTokenType()) != null; lexer.advance()) {
      if (t == TokenType.WHITE_SPACE || ErlangParserDefinition.COMMENTS.contains(t)) continue;
      r.add(new ErlangMacroExpander.Tok(t, lexer.getTokenText(), ErlangMacroExpander.SRC_NONE, lexer.getTokenStart(), lexer.getTokenEnd(), null));
    }
    return r;
  }

  /** The expander's split of {@code (shape)}: the arguments joined by " | ", tokens by spaces. */
  private String split(String shape) {
    List<ErlangMacroExpander.Tok> t = tokens("(" + shape + ") rest");
    ErlangMacroExpander.ArgsResult r = ErlangMacroExpander.parseArgs(t, 0);
    if (r == null) return "null";
    assertEquals(ErlangTypes.ERL_PAR_RIGHT, t.get(r.closeIndex()).type());
    assertEquals("rest", t.get(r.closeIndex() + 1).text());
    List<String> args = new ArrayList<>();
    for (List<ErlangMacroExpander.Tok> a : r.args()) {
      List<String> s = new ArrayList<>();
      for (ErlangMacroExpander.Tok x : a) s.add(x.text());
      args.add(String.join(" ", s));
    }
    return args.size() + ": " + String.join(" | ", args);
  }

  public void testArgumentCountsAsEpp() {
    List<String> wrong = new ArrayList<>();
    for (String[] s : SHAPES) {
      String r = split(s[0]);
      if (!r.startsWith(s[1] + ":")) wrong.add(s[0] + " -> " + r + " (erlc: " + s[1] + ")");
    }
    assertEquals("", String.join("\n", wrong));
  }

  public void testArgumentsOfAFunType() {
    assertEquals("2: fun ( ( a ) -> b ) | c", split("fun((a) -> b), c"));
    assertEquals("2: fun L ( X ) -> X end | 2", split("fun L(X) -> X end, 2"));
    assertEquals("2: fun ( ) | c", split("fun(), c"));
    assertEquals("2: fun ( X ) when X > 0 , true -> X end | 2", split("fun(X) when X > 0, true -> X end, 2"));
  }

  public void testNoClosingParenthesis() {
    assertEquals("null", split("fun() -> ok, b"));
  }

  /** The arity of a use in the file, as references and inspections count it, is the same. */
  public void testArityAfterAsEpp() {
    StringBuilder text = new StringBuilder("-module(b4).\n-define(N(), zero).\n-define(N(A), one).\n-define(N(A, B), two).\n");
    for (int k = 0; k < SHAPES.length; k++) text.append("f").append(k).append("() -> ?N(").append(SHAPES[k][0]).append(").\n");
    text.append("g() -> ?N.\n");
    PsiFile file = myFixture.configureByText("b4.erl", text.toString());
    List<String> wrong = new ArrayList<>();
    for (int k = 0; k < SHAPES.length; k++) {
      int at = text.indexOf("f" + k + "() -> ?N(") + ("f" + k + "() -> ?").length();
      PsiElement name = file.findElementAt(at);
      assertNotNull(name);
      assertEquals("N", name.getText());
      int arity = ErlangMacroResolve.arityAfter(name);
      if (!String.valueOf(arity).equals(SHAPES[k][1])) wrong.add(SHAPES[k][0] + " -> " + arity + " (erlc: " + SHAPES[k][1] + ")");
    }
    assertEquals("", String.join("\n", wrong));
    PsiElement bare = file.findElementAt(text.indexOf("g() -> ?N") + "g() -> ?".length());
    assertEquals(ErlangMacroResolve.NO_ARGUMENTS, ErlangMacroResolve.arityAfter(bare));
  }

  /** The tokens of the expanded form that starts with the prefix (see {@link ErlangMacroExpanderEppTest}). */
  private String form(String text, String prefix) {
    ErlangMacroExpander.Expansion x = ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000);
    ErlangMacroExpander.Form f = x.formAtSource(text.indexOf(prefix));
    List<String> r = new ArrayList<>();
    for (ErlangMacroExpander.Tok t : tokens(x.text.substring(f.expStart, f.expEnd))) r.add(t.text());
    return String.join(" ", r);
  }

  /** B4 (probes p6, funtype_spec2): a fun type in an argument. */
  public void testFunTypeArguments() {
    String defs = "-module(p6).\n-define(ID(X), X).\n-define(T(A, B), {A, B}).\n";
    assertEquals("- type t1 ( ) :: fun ( ) .", form(defs + "-type t1() :: ?ID(fun()).\n", "-type t1"));
    assertEquals("- type t2 ( ) :: fun ( ( a , b ) -> ok ) .", form(defs + "-type t2() :: ?ID(fun((a, b) -> ok)).\n", "-type t2"));
    assertEquals("- spec a ( ) -> fun ( ( ) -> ok ) .", form(defs + "-spec a() -> ?ID(fun(() -> ok)).\n", "-spec a"));
    assertEquals("- spec f ( { fun ( ) , c } ) -> ok .", form(defs + "-spec f(?T(fun(), c)) -> ok.\n", "-spec f"));
  }

  /** B4: a named fun in an argument; the arity is 2. epp: b. */
  public void testNamedFunArgument() {
    assertEquals("f ( ) -> b .", form("-module(m).\n-define(M(A), a).\n-define(M(A,B), b).\nf() -> ?M(fun L(X) -> X end, 2).\n", "f()"));
  }
}
