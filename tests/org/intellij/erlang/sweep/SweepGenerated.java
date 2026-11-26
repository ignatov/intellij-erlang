package org.intellij.erlang.sweep;

import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangTokenType;
import org.intellij.erlang.sweep.ErlangRobustnessSweepTest.Input;

import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.function.Function;

/**
 * Generated inputs of the robustness sweep (SWEEP_SYNTH): small Erlang files in the shapes that tend to break IDE
 * features. Each input id is {@code synthetic/<family>-<name>}, so SWEEP_ONLY can select a family.
 * <ul>
 *   <li>Fixed in every run: deep nesting of each construct, long lines and terms, odd Unicode, records, macros and
 *   includes in unusual places, maybe/else and -feature, sigils and triple-quoted strings, broken attributes.</li>
 *   <li>Seeded, so that every run of the synthetic cell sees new ones: the cut points of cut-* (a construct of each
 *   grammar rule left unterminated), soup-* (random token sequences from the lexer's token set) and gen-* (random,
 *   mostly valid modules).</li>
 * </ul>
 * The inputs are small, so a synthetic run fits in its time budget; the runner also caps the time of one input
 * (SWEEP_INPUT_BUDGET_SECONDS).
 */
final class SweepGenerated {
  private SweepGenerated() {
  }

  /** Nesting depth of the deep-* inputs: deep enough for recursive code, small enough to stay fast. */
  static final int DEPTH = 100;
  static final int CUTS_PER_SNIPPET = 2;
  static final int SOUPS = 20;
  static final int PROGRAMS = 12;

  static List<Input> inputs(long seed, Function<String, List<int[]>> tokens) {
    List<Input> r = new ArrayList<>();
    deep(r);
    longTerms(r);
    unicode(r);
    oddPlaces(r);
    includes(r);
    maybe(r);
    sigils(r);
    attributes(r);
    cuts(r, seed, tokens);
    soup(r, seed);
    programs(r, seed);
    return r;
  }

  private static final String HEADER = "-export([f/0, f/1, f/6]).\n-record(r, {a = 1, b}).\n-define(M(X), {X}).\n" +
                                       "-define(C, 1).\n";

  private static String mod(String name, String body) {
    return "-module(" + name + ").\n" + HEADER + body + (body.endsWith("\n") ? "" : "\n");
  }

  /** A module whose f/0 returns the expression. */
  private static String fun(String name, String expr) {
    return mod(name, "f() -> " + expr + ".\n");
  }

  private static void add(List<Input> r, String family, String name, String text) {
    String file = (family + "_" + name).replace('-', '_') + ".erl";
    r.add(new Input("synthetic/" + family + "-" + name, file, text));
  }

  private static void addFun(List<Input> r, String family, String name, String expr) {
    String module = (family + "_" + name).replace('-', '_');
    add(r, family, name, fun(module, expr));
  }

  private static void addMod(List<Input> r, String family, String name, String body) {
    String module = (family + "_" + name).replace('-', '_');
    add(r, family, name, mod(module, body));
  }

  private static void addBytes(List<Input> r, String family, String name, byte[] bytes) {
    r.add(new Input("synthetic/" + family + "-" + name, (family + "_" + name).replace('-', '_') + ".erl", bytes));
  }

  private static String nest(int n, String open, String leaf, String close) {
    return open.repeat(n) + leaf + close.repeat(n);
  }

  private static String wrap(int n, String leaf, Function<String, String> step) {
    String s = leaf;
    for (int i = 0; i < n; i++) s = step.apply(s);
    return s;
  }

  private static String join(int n, String sep, java.util.function.IntFunction<String> item) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < n; i++) {
      if (i > 0) sb.append(sep);
      sb.append(item.apply(i));
    }
    return sb.toString();
  }

  // ---------------------------------------------------------------------------------------------- deep nesting

  private static void deep(List<Input> r) {
    int d = DEPTH;
    addFun(r, "deep", "maps", nest(d, "#{a => ", "1", "}"));
    // The parser takes twice as long per level of maps or maybe (20 levels: 10 s, 24: a hang); at 100 deep-maps and
    // deep-maybe hang. A hang costs the wait and a restart of the test JVM, so the map keys stay at a depth that ends.
    addFun(r, "deep", "map-keys", wrap(12, "1", s -> "#{" + s + " => 1}"));
    addFun(r, "deep", "binaries", wrap(d, "<<1>>", s -> "<<(" + s + ")/binary>>"));
    addFun(r, "deep", "records", nest(d, "#r{a = ", "1", "}"));
    addMod(r, "deep", "record-access", "f(R) -> R" + "#r.a".repeat(d) + ".\n");
    addMod(r, "deep", "record-update", "f(R) -> R" + "#r{a = 1}".repeat(d) + ".\n");
    addMod(r, "deep", "map-update", "f(M) -> M" + "#{a => 1}".repeat(d) + ".\n");
    addFun(r, "deep", "begin", nest(d, "begin ", "ok", " end"));
    addFun(r, "deep", "try", nest(d, "try ", "ok", " catch _:_ -> ok end"));
    addFun(r, "deep", "try-after", nest(d, "try ", "ok", " after ok end"));
    addFun(r, "deep", "receive", nest(d, "receive _ -> ", "ok", " after 0 -> ok end"));
    addFun(r, "deep", "if", nest(d, "if true -> ", "ok", " end"));
    addFun(r, "deep", "maybe", nest(d, "maybe ", "ok", " end"));
    // Comprehensions, cases and maybe-else cost more than linearly per level: at 100 each took 1 to 3 minutes.
    int slow = 30;
    addFun(r, "deep", "maybe-else", nest(slow, "maybe {ok, _} ?= ", "{ok, 1}", " else _ -> ok end"));
    addMod(r, "deep", "list-comprehension", "f(L) -> " + wrap(slow, "L", s -> "[X || X <- " + s + "]") + ".\n");
    addMod(r, "deep", "binary-comprehension", "f(B) -> " + wrap(slow, "B", s -> "<< <<X>> || <<X>> <= " + s + " >>") + ".\n");
    addMod(r, "deep", "map-comprehension", "f(M) -> " + wrap(slow, "M", s -> "#{K => V || K := V <- " + s + "}") + ".\n");
    addFun(r, "deep", "catch", "catch ".repeat(d) + "ok");
    addFun(r, "deep", "minus", "- ".repeat(d) + "1");
    addFun(r, "deep", "not", "not ".repeat(d) + "true");
    addFun(r, "deep", "bnot", "bnot ".repeat(d) + "1");
    addFun(r, "deep", "send", "self() ! ".repeat(d) + "ok");
    addFun(r, "deep", "match", join(d, " = ", i -> "A" + i) + " = 1");
    addMod(r, "deep", "andalso-guard", "f(X) when " + "X > 0 andalso ".repeat(d) + "true -> ok.\n");
    addFun(r, "deep", "remote-calls", nest(d, "lists:reverse(", "[]", ")"));
    addMod(r, "deep", "local-calls", "f(X) -> " + nest(d, "f(", "X", ")") + ".\n");
    addFun(r, "deep", "fun-calls", nest(d, "(fun() -> ", "ok", " end)()"));
    addFun(r, "deep", "named-funs", nest(d, "fun F() -> ", "F", " end"));
    addFun(r, "deep", "macro-args", nest(d, "?M(", "1", ")"));
    // Each macro uses the previous one twice: an expansion of the last one would have 2^12 leaves.
    StringBuilder defs = new StringBuilder("-define(D0(X), X).\n");
    for (int i = 1; i <= 12; i++) defs.append("-define(D").append(i).append("(X), {?D").append(i - 1).append("(X), ?D").append(i - 1).append("(X)}).\n");
    addMod(r, "deep", "macro-doubling", defs + "f() -> ?D12(1).\n");
    addMod(r, "deep", "macro-chain", join(d, "", i -> "-define(K" + i + ", " + (i == 0 ? "1" : "?K" + (i - 1)) + ").\n") + "f() -> ?K" + (d - 1) + ".\n");
    addMod(r, "deep", "type-tuples", "-type t() :: " + nest(d, "{", "a", "}") + ".\n-spec f() -> t().\nf() -> a.\n");
    addMod(r, "deep", "type-lists", "-type t() :: " + nest(d, "[", "a", "]") + ".\n-spec f() -> t().\nf() -> [].\n");
    addMod(r, "deep", "type-funs", "-type t() :: " + nest(d, "fun(() -> ", "ok", ")") + ".\n-spec f() -> t().\nf() -> ok.\n");
    addMod(r, "deep", "type-maps", "-type t() :: " + nest(d, "#{a => ", "b", "}") + ".\n-spec f() -> t().\nf() -> ok.\n");
    addMod(r, "deep", "type-unions", "-type t() :: " + nest(d, "(a | ", "b", ")") + ".\n-spec f() -> t().\nf() -> a.\n");
    addMod(r, "deep", "ifdef", "-ifdef(A).\n".repeat(d) + "f() -> ok.\n" + "-endif.\n".repeat(d));
    addMod(r, "deep", "ifdef-else", "-ifdef(A).\n-else.\n".repeat(d) + "f() -> ok.\n" + "-endif.\n".repeat(d));
    addMod(r, "deep", "tuple-pattern", "f(" + nest(d, "{", "X", "}") + ") -> X.\n");
    addMod(r, "deep", "cons-pattern", "f(" + nest(d, "[a | ", "T", "]") + ") -> T.\n");
    addMod(r, "deep", "map-pattern", "f(" + nest(d, "#{a := ", "X", "}") + ") -> X.\n");
    addMod(r, "deep", "record-pattern", "f(" + nest(d, "#r{a = ", "X", "}") + ") -> X.\n");
    addMod(r, "deep", "binary-pattern", "f(" + wrap(d, "X", s -> "<<" + s + ":8>>") + ") -> X.\n");
    addFun(r, "deep", "case-in-args", nest(slow, "{case 1 of _ -> ", "ok", " end}"));
    addFun(r, "deep", "parens-ops", wrap(d, "1", s -> "(" + s + " + 1)"));
    addFun(r, "deep", "right-ops", nest(d, "1 ++ (", "[]", ")"));
  }

  // ---------------------------------------------------------------------------------------------- long lines and terms

  private static void longTerms(List<Input> r) {
    addFun(r, "long", "binary", "<<" + join(20000, ", ", i -> String.valueOf(i % 256)) + ">>");
    addMod(r, "long", "binary-segments", "f(X) -> <<" + join(500, ", ", i -> "X:8/integer-unsigned-big-unit:1") + ">>.\n");
    addFun(r, "long", "binary-string", "<<\"" + "z".repeat(200_000) + "\">>");
    addFun(r, "long", "map", "#{" + join(10000, ", ", i -> "k" + i + " => " + i) + "}");
    addMod(r, "long", "map-pattern", "f(#{" + join(2000, ", ", i -> "k" + i + " := V" + i) + "}) -> V0.\n");
    addFun(r, "long", "map-update-chain", "#{}" + join(1000, "", i -> "#{k" + i + " => " + i + "}"));
    addFun(r, "long", "tuple", "{" + join(20000, ", ", i -> "a") + "}");
    addFun(r, "long", "string-concat", join(10000, " ", i -> "\"a\""));
    addFun(r, "long", "char-list", "[" + join(20000, ", ", i -> "$a") + "]");
    addMod(r, "long", "export", "-export([" + join(5000, ", ", i -> "g" + i + "/0") + "]).\n");
    addMod(r, "long", "type-union", "-type t() :: " + join(5000, " | ", i -> "a" + i) + ".\n-spec f() -> t().\nf() -> a0.\n");
    addMod(r, "long", "record", "-record(big, {" + join(3000, ", ", i -> "f" + i + " = " + i) + "}).\n" +
                               "f() -> #big{f0 = 1, f2999 = 2}.\n");
    addMod(r, "long", "function-name", "g" + "x".repeat(50_000) + "() -> ok.\nf() -> g" + "x".repeat(50_000) + "().\n");
    addMod(r, "long", "variable", "f() -> V" + "a".repeat(50_000) + " = 1, V" + "a".repeat(50_000) + ".\n");
    addMod(r, "long", "quoted-atom-spaces", "f() -> '" + "a b ".repeat(30_000) + "'.\n");
    addMod(r, "long", "macro-params", "-define(P(" + join(1000, ", ", i -> "A" + i) + "), {A0, A999}).\n" +
                                     "f() -> ?P(" + join(1000, ", ", i -> String.valueOf(i)) + ").\n");
    addFun(r, "long", "integer", "1".repeat(100_000));
    addFun(r, "long", "hex-integer", "16#" + "F".repeat(50_000));
    addFun(r, "long", "base36-integer", "36#" + "Z".repeat(10_000));
    addFun(r, "long", "underscore-integer", "1" + "_000".repeat(20_000));
    addFun(r, "long", "float", "1." + "0".repeat(50_000) + "e308");
    addFun(r, "long", "float-exponent", "1.0e" + "9".repeat(10_000));
    addMod(r, "long", "guards", "f(X) when " + join(500, "; ", i -> "X =:= " + i) + " -> ok.\n");
    addMod(r, "long", "guard-commas", "f(X) when " + join(500, ", ", i -> "X =/= " + i) + " -> ok.\n");
    add(r, "long", "one-line-forms", "-module(long_one_line_forms). " + join(5000, " ", i -> "g" + i + "() -> " + i + ".") + "\n");
    addMod(r, "long", "comment-line", "%" + "x".repeat(200_000) + "\nf() -> ok.\n");
    addFun(r, "long", "triple-quoted", "\"\"\"\n" + "  line\n".repeat(20_000) + "  \"\"\"");
    addFun(r, "long", "sigil", "~\"" + "y".repeat(200_000) + "\"");
    addMod(r, "long", "case-clauses", "f(X) -> case X of " + join(5000, "; ", i -> i + " -> " + i) + "; _ -> ok end.\n");
    addMod(r, "long", "receive-clauses", "f() -> receive " + join(1000, "; ", i -> "{m" + i + ", X} -> X") + " after 0 -> ok end.\n");
    addMod(r, "long", "spec-clauses", "-spec g" + join(2000, "; ", i -> "(" + i + ") -> ok") + ".\ng(_) -> ok.\nf() -> ok.\n");
    addMod(r, "long", "ifdef-chain", join(3000, "", i -> "-ifdef(X" + i + ").\n-endif.\n") + "f() -> ok.\n");
    addMod(r, "long", "compile-list", "-compile([" + join(1500, ", ", i -> "{inline, [f/0]}") + "]).\nf() -> ok.\n");
    addMod(r, "long", "fun-clauses", "f() -> fun " + join(3000, "; ", i -> "(" + i + ") -> " + i) + " end.\n");
    addMod(r, "long", "body-expressions", "f() ->\n" + join(5000, ",\n", i -> "  g(" + i + ")") + ".\ng(X) -> X.\n");
    addMod(r, "long", "string-escapes", "f() -> \"" + "\\x{1F600}\\n\\t\\\\\\\"".repeat(20_000) + "\".\n");
  }

  // ---------------------------------------------------------------------------------------------- Unicode

  private static byte[] bytes(String before, int[] raw, String after) {
    byte[] a = before.getBytes(StandardCharsets.UTF_8);
    byte[] b = after.getBytes(StandardCharsets.UTF_8);
    byte[] out = Arrays.copyOf(a, a.length + raw.length + b.length);
    for (int i = 0; i < raw.length; i++) out[a.length + i] = (byte)raw[i];
    System.arraycopy(b, 0, out, a.length + raw.length, b.length);
    return out;
  }

  private static void unicode(List<Input> r) {
    addFun(r, "unicode", "combining-atom", "'e\u0301\u0301\u0301\u0301'");
    addFun(r, "unicode", "combining-first", "'\u0301a'");
    addFun(r, "unicode", "zwj-emoji-atom", "'\uD83D\uDC68\u200D\uD83D\uDC69\u200D\uD83D\uDC67'");
    addFun(r, "unicode", "flag-string", "\"\uD83C\uDDEF\uD83C\uDDF5\uD83C\uDDFA\uD83C\uDDE6\"");
    addMod(r, "unicode", "rtl-override", "%% \u202Eevil comment\nf() -> \"\u202Ereversed\u202C\".\n");
    addMod(r, "unicode", "line-separator", "f() ->\u2028ok.\n%% a\u2029b\ng() -> \"x\u2028y\".\n");
    addMod(r, "unicode", "bom-between-forms", "f() -> ok.\n\uFEFFg() -> ok.\n");
    addMod(r, "unicode", "nbsp-whitespace", "f()\u00A0->\u00A0ok.\ng(X)\u00A0when\u00A0X\u00A0>\u00A00\u00A0->\u00A0X.\n");
    addFun(r, "unicode", "soft-hyphen-atom", "'a\u00ADb'");
    addMod(r, "unicode", "latin1-atoms", "café() -> ça.\nf() -> {café(), Øre = 1, Øre, ñandú}.\n");
    addMod(r, "unicode", "non-latin1-atom", "λ() -> ok.\nf() -> λ().\n");
    addMod(r, "unicode", "non-latin1-variable", "f() -> Λ = 1, Λ.\n");
    addFun(r, "unicode", "emoji-char", "[$\uD83D\uDE00, $\u0301, $\\x{1F600}, $\\x{110000}, $\\x{D800}, $\\^A, $\\777]");
    addFun(r, "unicode", "string-escapes", "\"\\x{10FFFF}\\x{D800}\\x{110000}\\x{}\\x{FFFFFFFFFFFF}\"");
    addFun(r, "unicode", "unterminated-escape", "\"\\x{");
    addMod(r, "unicode", "control-whitespace", "f()\t->\u000B\u000Cok.\ng() -> \"\u0007\u001B\".\n");
    addFun(r, "unicode", "homoglyph-keyword", "c\u0430se 1 of _ -> ok end");
    addMod(r, "unicode", "fullwidth-parens", "f\uFF08\uFF09 -> ok.\n");
    addMod(r, "unicode", "zero-width-space", "f() ->\u200B ok.\ng() -> a\u200Bb.\n");
    addMod(r, "unicode", "macro-name", "-define('Ünï', 1).\n-define('😀'(X), X).\nf() -> {?'Ünï', ?'😀'(2)}.\n");
    addMod(r, "unicode", "record-name", "-record('ü', {'ß' = 1, 'λ'}).\nf() -> #'ü'{'ß' = 2}.\ng(R) -> R#'ü'.'λ'.\n");
    addMod(r, "unicode", "function-name", "'λ'() -> ok.\n'😀'(X) -> X.\nf() -> {'λ'(), '😀'(1), fun 'λ'/0}.\n");
    addMod(r, "unicode", "doc-comments", "%% @doc \uD83D\uDE00\u0301 ünïcödé\n%%% \u202E\nf() -> ok.\n");
    addMod(r, "unicode", "moduledoc", "-moduledoc \"\"\"\n  ünï \uD83D\uDE00\n  \u2028\n  \"\"\".\n-doc ~\"λ\".\nf() -> ok.\n");
    addFun(r, "unicode", "sigil", "~s\"\uD83D\uDE00\u0301\"");
    r.add(new Input("synthetic/unicode-module-name", "модуль.erl", "-module('модуль').\n-export([f/0]).\nf() -> 'модуль':f().\n"));
    r.add(new Input("synthetic/unicode-module-mismatch", "ünï.erl", "-module(uni).\n-export([f/0]).\nf() -> ok.\n"));
    addBytes(r, "unicode", "lone-surrogate", bytes("-module(unicode_lone_surrogate).\nf() -> \"", new int[]{0xED, 0xA0, 0x80}, "\".\n"));
    addBytes(r, "unicode", "overlong", bytes("-module(unicode_overlong).\nf() -> '", new int[]{0xC0, 0xAF}, "'.\n"));
    addBytes(r, "unicode", "beyond-range", bytes("-module(unicode_beyond_range).\n%% ", new int[]{0xF4, 0x90, 0x80, 0x80}, "\nf() -> ok.\n"));
    addBytes(r, "unicode", "truncated-at-eof", bytes("-module(unicode_truncated_at_eof).\nf() -> \"", new int[]{0xE2, 0x82}, ""));
    addBytes(r, "unicode", "latin1-coding", bytes("%% -*- coding: latin-1 -*-\n-module(unicode_latin1_coding).\nf() -> caf", new int[]{0xE9}, ".\n"));
    byte[] utf16 = "-module(unicode_utf16).\nf() -> ok.\n".getBytes(StandardCharsets.UTF_16LE);
    byte[] withBom = new byte[utf16.length + 2];
    withBom[0] = (byte)0xFF;
    withBom[1] = (byte)0xFE;
    System.arraycopy(utf16, 0, withBom, 2, utf16.length);
    addBytes(r, "unicode", "utf16-bom", withBom);
    add(r, "unicode", "mixed-line-ends", "-module(unicode_mixed_line_ends).\r\n-export([f/0]).\rf() ->\n  \"a\r\nb\rc\",\r\n  ok.\r");
  }

  // ---------------------------------------------------------------------------------------------- records, macros

  private static void oddPlaces(List<Input> r) {
    addMod(r, "odd", "macro-function-name", "-define(F, g).\n?F() -> ok.\nf() -> ?F().\n");
    addMod(r, "odd", "macro-head", "-define(HEAD, g(X) ->).\n?HEAD X.\nf() -> g(1).\n");
    addMod(r, "odd", "macro-clause-body", "-define(BODY, -> ok).\ng() ?BODY.\nf() -> g().\n");
    addMod(r, "odd", "macro-record-name", "-define(R, r).\nf() -> #?R{a = 1}.\ng(X) -> X#?R.a.\n");
    addMod(r, "odd", "macro-remote-call", "-define(Mod, lists).\n-define(F, reverse).\nf() -> {?Mod:?F([]), ?Mod:reverse([]), lists:?F([]), ?MODULE:f()}.\n");
    addMod(r, "odd", "macro-fun-ref", "-define(F, f).\nf() -> {fun ?MODULE:f/0, fun ?F/0, fun ?MODULE:?F/0}.\n");
    addMod(r, "odd", "macro-in-attributes", "-define(F, g).\n-define(T, t).\n-export([?F/0]).\n-type ?T() :: ok.\n-spec ?F() -> ok.\n?F() -> ok.\nf() -> ok.\n");
    addMod(r, "odd", "macro-record-def", "-define(R, rr).\n-record(?R, {a}).\nf() -> #rr{a = 1}.\n");
    addMod(r, "odd", "macro-empty-body", "-define(E, ).\n-define(E2(), ).\nf() -> {?E ?E2()}.\n");
    addMod(r, "odd", "macro-defining-macro", "-define(X, -define(Y, 1).).\n?X\nf() -> ?Y.\n");
    addMod(r, "odd", "macro-forms", "-define(FORMS, g() -> 1. h() -> 2.).\n?FORMS\nf() -> g() + h().\n");
    addMod(r, "odd", "stringify", "-define(S(X), ??X).\nf() -> {?S(a + b), ??f, ?S(?S(1))}.\n");
    addMod(r, "odd", "predefined-outside", "-x(?FUNCTION_NAME).\n-y({?LINE, ?FUNCTION_ARITY, ?MODULE_STRING, ?FILE}).\nf() -> {?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE}.\n");
    addMod(r, "odd", "macro-wrong-arity", "f() -> {?M, ?M(), ?M(1, 2), ?C(1)}.\n");
    addMod(r, "odd", "macro-unbalanced-args", "f() -> ?M((1).\ng() -> ?M(,).\nh() -> ?M([}).\n");
    addMod(r, "odd", "macro-recursive", "-define(L(X), ?L(X)).\n-define(N, ?N + 1).\nf() -> {?L(1), ?N}.\n");
    addMod(r, "odd", "macro-places", "-define(SZ, 8).\n-define(K, k).\n-define(P, {_, _}).\n-define(T, integer()).\n" +
                                    "-type t() :: ?T.\n-spec f(?T) -> t().\nf(?P) -> <<1:?SZ>>;\nf(X) -> #{?K => X, ?SZ => ?K}.\n");
    addMod(r, "odd", "macro-keyword-name", "-define(case, 1).\n-define(end, 2).\n-define(Var, 3).\nf() -> {?case, ?end, ?Var}.\n");
    addMod(r, "odd", "macro-redefine", "-define(A, 1).\n-define(A, 2).\n-define(A(X), X).\n-undef(A).\n-undef(A).\nf() -> {?A, ?A(1)}.\n");
    addMod(r, "odd", "macro-guard", "-define(G(X), is_integer(X) andalso X > 0).\nf(X) when ?G(X) -> ok;\nf(?C) -> one.\n");
    addMod(r, "odd", "ifdef-in-function", "f() ->\n-ifdef(X).\n  a\n-else.\n  b\n-endif.\n  .\n");
    addMod(r, "odd", "ifdef-in-head", "-ifdef(X).\ng(1) ->\n-else.\ng(2) ->\n-endif.\n  ok.\nf() -> g(1).\n");
    addMod(r, "odd", "ifdef-unbalanced", "-endif.\n-else.\n-ifdef(A).\n-ifndef(B).\nf() -> ok.\n");
    addMod(r, "odd", "if-elif", "-if(?OTP_RELEASE >= 27).\nf() -> a.\n-elif(?OTP_RELEASE >= 26).\nf() -> b.\n-else.\nf() -> c.\n-endif.\n-if(.\n-elif().\n");
    addMod(r, "odd", "record-in-guard", "f(X) when is_record(X, r), X#r.a > 1, (X#r{a = 2})#r.b =:= 2 -> ok;\nf(_) -> #r.a.\n");
    addMod(r, "odd", "record-oddities", "-record(e, {}).\n-record('case', {'end', 'of' = 'begin'}).\n" +
                                       "f() -> {#e{}, #'case'{'end' = 1}, #r{_ = 1}, #r{a = 1, a = 2}, #undefined_rec{}, (#r{})#r.nofield, #r.nofield}.\n");
    addMod(r, "odd", "record-default-cycle", "-record(a, {x = #b{}}).\n-record(b, {y = #a{}}).\n-record(c, {z = (#c{})#c.z}).\nf() -> {#a{}, #c{}}.\n");
    addMod(r, "odd", "record-in-function", "f() ->\n  -record(inner, {a}),\n  #inner{}.\n");
    addMod(r, "odd", "record-field-types", "-record(t, {a :: #t{} | undefined, b = fun() -> #t{} end :: fun(), c = ?M(#t{})}).\nf() -> #t{}.\n");
    addMod(r, "odd", "record-index-chain", "f(R) -> {R#r.a#r.b, ((R#r.a)#r.b)#r.a, R#r{a = R#r{}}#r.a}.\n");
    addMod(r, "odd", "attribute-after-function", "f() -> ok.\n-module(late).\n-export([g/0]).\n-record(late, {a}).\n-define(LATE, 1).\ng() -> {#late{}, ?LATE}.\n");
    addMod(r, "odd", "function-in-type", "-type t() :: f().\n-spec f() -> f().\nf() -> ok.\n");
    addMod(r, "odd", "same-name-everywhere", "-define(f, f).\n-record(f, {f = f}).\n-type f() :: f.\n-spec f() -> f().\n" +
                                            "f() -> #f{f = ?f}#f.f.\nf(f) -> f:f(f);\nf(F) -> F(F).\n");
    addMod(r, "odd", "export-missing", "-export([missing/0, f/99, 'quoted'/1]).\n-export_type([nope/0]).\n-import(nomod, [f/0]).\n");
    addMod(r, "odd", "import-self-clash", "-import(lists, [reverse/1, reverse/1]).\n-import(other, [reverse/1]).\nreverse(X) -> X.\nf() -> reverse([]).\n");
    addMod(r, "odd", "behaviour-odd", "-behaviour().\n-behaviour(?MODULE).\n-behavior(gen_server).\n-behaviour(\"str\").\n-callback f() -> ok.\n-callback().\n");
  }

  private static void includes(List<Input> r) {
    String m = "-module(inc).\n-export([f/0]).\n";
    r.add(new Input("synthetic/inc-broken-hrl", "inc_broken.erl", m.replace("inc", "inc_broken") + "-include(\"inc_broken.hrl\").\nf() -> ok.\ng() -> h().\n")
            .with("inc_broken.hrl", "h() -> case X of\n  a -> [{\n"));
    r.add(new Input("synthetic/inc-hrl-functions", "inc_funs.erl", m.replace("inc", "inc_funs") + "-include(\"inc_funs.hrl\").\nf() -> {helper(), ?HM, #hr{}}.\n")
            .with("inc_funs.hrl", "-export([helper/0]).\n-define(HM, helper()).\n-record(hr, {a = helper()}).\nhelper() -> ok.\n"));
    r.add(new Input("synthetic/inc-hrl-module", "inc_mod.erl", "-include(\"inc_mod.hrl\").\n-export([f/0]).\nf() -> ?MODULE.\n")
            .with("inc_mod.hrl", "-module(inc_mod).\n-module(other).\n"));
    r.add(new Input("synthetic/inc-in-function", "inc_fn.erl", m.replace("inc", "inc_fn") + "f() ->\n-include(\"inc_fn.hrl\").\n  ?X.\n")
            .with("inc_fn.hrl", "-define(X, 1).\n"));
    addMod(r, "inc", "odd-paths", "-include(\"\").\n-include(\"/\").\n-include(\"sub/\").\n-include(\"../../../x.hrl\").\n" +
                                 "-include_lib(\"\").\n-include_lib(\"nolib\").\n-include_lib(\"/abs/x.hrl\").\n-include(foo).\n-include(\"a\" \"b.hrl\").\nf() -> ok.\n");
    addMod(r, "inc", "unterminated", "-include(\"inc_x.hrl\"\nf() -> ok.\n");
    r.add(new Input("synthetic/inc-macro-path", "inc_mp.erl", m.replace("inc", "inc_mp") + "-define(P, \"inc_mp.hrl\").\n-include(?P).\n-include(?FILE).\nf() -> ?FROM_HRL.\n")
            .with("inc_mp.hrl", "-define(FROM_HRL, 1).\n"));
    r.add(new Input("synthetic/inc-redefine-record", "inc_rr.erl", m.replace("inc", "inc_rr") + "-include(\"inc_rr.hrl\").\n-record(r, {b}).\n-define(M, 2).\nf() -> {#r{a = 1, b = 2}, ?M, ?M(1)}.\n")
            .with("inc_rr.hrl", "-record(r, {a}).\n-define(M(X), X).\n"));
    r.add(new Input("synthetic/inc-ifdef-across", "inc_if.erl", m.replace("inc", "inc_if") + "-include(\"inc_if.hrl\").\nf() -> ok.\n-endif.\n-else.\n")
            .with("inc_if.hrl", "-ifdef(X).\n-ifndef(Y).\n-define(Z, 1).\n"));
    r.add(new Input("synthetic/inc-chain", "inc_ch.erl", m.replace("inc", "inc_ch") + "-include(\"include/inc_a.hrl\").\nf() -> {#deep{}, ?DEEP}.\n")
            .with("include/inc_a.hrl", "-include(\"inc_b.hrl\").\n")
            .with("include/inc_b.hrl", "-include(\"../include/inc_c.hrl\").\n")
            .with("include/inc_c.hrl", "-record(deep, {a = ?DEEP}).\n-define(DEEP, 1).\n"));
    r.add(new Input("synthetic/inc-hrl-only-broken", "inc_only.hrl", "-record(r, {a = \n-define(X(, 1).\n-type t() :: .\nf() -> \n"));
    r.add(new Input("synthetic/inc-hrl-feature", "inc_ft.erl", m.replace("inc", "inc_ft") + "-include(\"inc_ft.hrl\").\nf() -> maybe ok end.\n")
            .with("inc_ft.hrl", "-feature(maybe_expr, enable).\n-feature(maybe_expr, disable).\n"));
  }

  // ---------------------------------------------------------------------------------------------- maybe, -feature

  private static void maybe(List<Input> r) {
    addMod(r, "maybe", "nested", "f() -> maybe maybe {ok, A} ?= g(), A end, maybe ok end end.\ng() -> {ok, 1}.\n");
    addMod(r, "maybe", "else-empty", "f() -> maybe ok else end.\n");
    addMod(r, "maybe", "else-trailing-semicolon", "f() -> maybe {ok, A} ?= g(), A else error -> e; end.\ng() -> ok.\n");
    addMod(r, "maybe", "cond-match-outside", "f() -> {ok, A} ?= g(), A.\ng() -> ok.\nh(X ?= 1) -> X.\n");
    addMod(r, "maybe", "in-guard", "f(X) when maybe X end -> ok.\n");
    addMod(r, "maybe", "in-macro", "-define(MB(X), maybe {ok, V} ?= X, V end).\nf() -> ?MB({ok, 1}).\n");
    addMod(r, "maybe", "function-named-maybe", "maybe() -> ok.\n'maybe'(X) -> X.\nf() -> {'maybe'(), 'else'}.\n");
    addMod(r, "maybe", "feature-disable", "-feature(maybe_expr, disable).\nf() -> maybe ok end.\nmaybe() -> ok.\n");
    addMod(r, "maybe", "feature-late", "f() -> maybe ok end.\n-feature(maybe_expr, enable).\ng() -> ok.\n");
    addMod(r, "maybe", "feature-broken", "-feature(maybe_expr).\n-feature().\n-feature(no_such_feature, enable).\n-feature(maybe_expr, maybe).\n-feature(\"maybe_expr\", enable).\nf() -> ok.\n");
    addMod(r, "maybe", "else-alone", "f() -> else.\ng() -> ok else ok.\n");
    addMod(r, "maybe", "in-comprehension", "f(L) -> [maybe {ok, Y} ?= X, Y else _ -> 0 end || X <- L].\n");
    addMod(r, "maybe", "in-fun", "f() -> fun(X) -> maybe {ok, Y} ?= X, Y end end.\n");
    addMod(r, "maybe", "else-guards", "f(X) -> maybe {ok, Y} ?= X, Y else {error, E} when is_atom(E) -> E; _ when true -> ok end.\n");
    addMod(r, "maybe", "last-is-cond-match", "f(X) -> maybe {ok, Y} ?= X end.\n");
    addMod(r, "maybe", "type-named-maybe", "-type maybe() :: ok.\n-record(mb, {a = maybe ok end}).\n-spec f() -> maybe().\nf() -> ok.\n");
    addMod(r, "maybe", "cond-match-chain", "f(X) -> maybe A ?= B ?= X, {A, B} end.\n");
    addMod(r, "maybe", "maybe-of", "f(X) -> maybe X of _ -> ok end.\n");
  }

  // ---------------------------------------------------------------------------------------------- sigils, triple quotes

  private static void sigils(List<Input> r) {
    String[][] delimiters = {{"\"", "\""}, {"'", "'"}, {"(", ")"}, {"[", "]"}, {"{", "}"}, {"<", ">"}, {"/", "/"},
      {"|", "|"}, {"`", "`"}, {"#", "#"}};
    StringBuilder all = new StringBuilder("f() -> [");
    for (String prefix : new String[]{"", "b", "B", "s", "S"}) {
      for (String[] d : delimiters) all.append("~").append(prefix).append(d[0]).append("a\\n\\x{41}").append(d[1]).append(", ");
    }
    addMod(r, "sigil", "all-delimiters", all + "ok].\n");
    for (String[] d : delimiters) {
      String name = "unterminated-" + Integer.toHexString(d[0].charAt(0));
      addMod(r, "sigil", name, "f() -> ~s" + d[0] + "abc\ng() -> ok.\n");
    }
    addMod(r, "sigil", "unknown-prefix", "f() -> {~x\"a\", ~q(b), ~1\"c\", ~bb\"d\", ~ \"e\"}.\n");
    addMod(r, "sigil", "tilde-alone", "f() -> ~.\ng() -> ~~.\n");
    addMod(r, "sigil", "eof-quote", "f() -> ~\"");
    addMod(r, "sigil", "eof-triple", "f() -> ~\"\"\"");
    addMod(r, "sigil", "triple-sigils", "f() -> {~\"\"\"\n  a\n  \"\"\", ~B\"\"\"\n  \\n\n  \"\"\", ~s\"\"\"\"\n  \"\"\"\n  \"\"\"\"}.\n");
    addMod(r, "sigil", "in-patterns", "f(~\"a\") -> 1;\nf(<<~\"b\"/binary, _/binary>>) -> 2;\nf(X) when X =:= ~B[c] -> 3.\n");
    addMod(r, "sigil", "in-attributes", "-doc ~\"x\".\n-moduledoc ~B\"\"\"\n  m\n  \"\"\".\n-x(~s{y}).\n-record(sg, {a = ~\"d\" :: binary()}).\n-define(SG, ~\"m\").\nf() -> {?SG, ?M(~\"a\"), #sg{}}.\n");
    addMod(r, "sigil", "adjacent", "f() -> {\"a\" ~\"b\", ~\"a\" ~\"b\", ~\"a\" \"b\", ~\"a\"b, ~\"a\"~\"b\"}.\n");
    addMod(r, "sigil", "in-binary-segments", "f() -> <<~\"a\"/binary, ~\"b\":8, (~\"c\")/utf8>>.\n");
    addMod(r, "sigil", "verbatim-escapes", "f() -> {~B\"\\x{\", ~S(\\), ~B[\\^], ~S{\\x{41}}}.\n");
    addMod(r, "sigil", "triple-bad-indent", "f() -> \"\"\"\n  a\n b\n  \"\"\".\n");
    addMod(r, "sigil", "triple-content-on-open", "f() -> \"\"\" x\n  \"\"\".\ng() -> \"\"\"\n  y \"\"\".\n");
    addMod(r, "sigil", "triple-four-quotes", "f() -> \"\"\"\"\n  \"\"\"\n  \"\"\"\".\ng() -> \"\"\"\"\"\n\"\"\"\"\"\".\n");
    addMod(r, "sigil", "triple-unterminated", "f() -> \"\"\"\n  never closed\ng() -> ok.\n");
    addMod(r, "sigil", "triple-adjacent", "f() -> \"\"\"\n  a\n  \"\"\" \"\"\"\n  b\n  \"\"\".\ng() -> \"a\" \"\"\"\n  b\n  \"\"\".\n");
    addMod(r, "sigil", "triple-empty", "f() -> \"\"\"\n\"\"\".\ng() -> \"\"\"\"\"\".\n");
    addMod(r, "sigil", "triple-in-pattern", "f(\"\"\"\n  a\n  \"\"\") -> ok.\n");
    addMod(r, "sigil", "doc-attributes", "-moduledoc \"\"\"\n  Module.\n  \"\"\".\n-moduledoc false.\n-moduledoc {file, \"x.md\"}.\n" +
                                         "-doc #{since => ~\"1.0\"}.\n-doc false.\n-doc \"\"\"\n  f\n  \"\"\".\n-doc().\n-doc.\nf() -> ok.\n");
    addMod(r, "sigil", "in-type", "-type t() :: ~\"a\" | \"\"\"\n  b\n  \"\"\".\n-spec f() -> t().\nf() -> ok.\n");
    addMod(r, "sigil", "in-macro-call", "f() -> {?M(~\")\"), ?M(~(,)), ?M(\"\"\"\n  )\n  \"\"\")}.\n");
  }

  // ---------------------------------------------------------------------------------------------- broken attributes

  private static final String[] BROKEN_ATTRIBUTES = {
    "-module().", "-module(1).", "-module('').", "-module(a.b).", "-module(x, y).", "-module", "- module(spaced).",
    "-'module'(quoted).", "-export([f/]).", "-export([f/a]).", "-export(f/0).", "-export([/0]).", "-export([f/0,]).",
    "-export([f/-1]).", "-export([f/99999999999999999999]).", "-export([f/0", "-export_type([t/]).",
    "-import(lists, [map/2, ]).", "-import(lists).", "-import([map/2]).", "-spec f(.", "-spec f() -> .", "-spec .",
    "-spec f(X) -> X when .", "-spec f() -> ok; .", "-spec m:f() -> ok.", "-spec f/0 :: () -> ok.", "-type t() :: .",
    "-type t :: ok.", "-type t() :: t() :: t().", "-type T() :: ok.", "-type t(A, A) :: A.", "-opaque o().",
    "-callback .", "-callback f() -> .", "-record(r).", "-record(r, {a = }).", "-record(r, {a :: }).", "-record(, {a}).",
    "-record(r, {1}).", "-record(r, a).", "-record(r, {a, a}).", "-define(.", "-define(X(, 1).", "-define(X(A, A), A).",
    "-define(X Y).", "-define().", "-define(1, 2).", "-include(foo).", "-include().", "-include_lib.", "-behaviour().",
    "-compile([{inline, [f/]}]).", "-compile(.", "-on_load(f/0).", "-on_load(nope).", "-nifs([f/0, ]).",
    "-dialyzer({nowarn_function, f/}).", "-deprecated([{f, 0, nope}]).", "-file(\"x\", ).", "-file().", "-vsn().",
    "-ifdef.", "-ifdef().", "-ifdef(1).", "-ifndef(A, B).", "-else(x).", "-endif(x).", "-undef().", "-if().",
    "-foo bar.", "-foo(bar", "-.", "--.", "-(x).", "-123.", "-\"str\"(x).", "-module(x) -export([]).",
    "-export([f/0]) -record(r, {}).", "-spec f() -> ok -spec g() -> ok.", "-record(r, {a = case 1 of}).",
    "-type t() :: #{a := }.", "-type t() :: <<_:>>.", "-type t() :: 1...", "-type t() :: fun((...) -> ).",
  };

  private static void attributes(List<Input> r) {
    StringBuilder all = new StringBuilder();
    for (String a : BROKEN_ATTRIBUTES) all.append(a).append('\n');
    add(r, "attr", "all-broken", "-module(attr_all_broken).\n" + all + "f() -> ok.\n");
    // Each broken attribute alone, between valid forms, so that recovery after it is exercised too.
    for (int i = 0; i < BROKEN_ATTRIBUTES.length; i++) {
      add(r, "attr", String.format("%02d", i), "-module(attr_" + i + ").\n-export([f/0]).\n" + BROKEN_ATTRIBUTES[i] +
                                               "\n-record(after_attr, {a}).\nf() -> #after_attr{}.\n");
    }
    add(r, "attr", "at-eof-no-dot", "-module(attr_eof).\n-export([f/0]).\nf() -> ok.\n-spec g() -> ok");
  }

  // ---------------------------------------------------------------------------------------------- unterminated constructs

  /** One construct per grammar rule, in expression position. */
  static final String[] EXPRESSIONS = {
    "case X of a -> 1; _ -> 2 end", "if X > 1 -> a; true -> b end", "receive {m, V} -> V after 10 -> ok end",
    "try g(X) of V -> V catch error:E:S -> {E, S}; _ -> c after ok end", "begin X, Y end", "fun(A) -> A end",
    "fun F(0) -> 0; F(N) -> F(N - 1) end", "fun lists:map/2", "fun g/1", "fun M:F/1", "[A || A <- L, A > 1]",
    "<< <<A>> || <<A>> <= B >>", "#{K => V || K := V <- M}", "[A || A := _ <- M]", "#{a => 1, b => X}",
    "M#{a := 1, b => 2}", "#r{a = 1, b = X}", "R#r.a", "R#r{a = 2}", "#r.a", "<<X:8/integer-unit:1, Y/binary>>",
    "<<\"pre\", Rest/binary>> = B", "maybe {ok, A} ?= g(X), A else E -> E end", "catch g(X)", "X ! Y",
    "[H | T] = L", "{a, X, [Y]}", "\"str\" \"ing\"", "'qu oted'", "$a", "?M(X)", "?MODULE:g(X)", "-X + Y * 2 div 3 rem 4",
    "X band Y bor 1 bxor 2 bsl 3 bsr 4", "X andalso Y orelse not X", "X =:= Y orelse X =/= Y", "L ++ [1] -- [2]",
    "(fun() -> ok end)()", "g(X, Y)", "m:g(X, Y)", "X:g(Y)", "A = B = X", "~s\"sigil\"", "\"\"\"\n  tq\n  \"\"\"",
    "[1, 2 | 3]", "{}", "#{}", "<<>>", "[]", "16#FF + 2#101 + 1_000 + 1.5e-3", "X#{a => 1}#{b => 2}",
    "case X of {A, B} when A > B, is_integer(A); A =:= 0 -> A end",
  };

  /** One construct per grammar rule, as a top-level form. */
  static final String[] FORMS = {
    "-spec g(integer()) -> ok.", "-spec g(X) -> X when X :: atom().", "-spec g(a) -> b; (c) -> d.",
    "-type t(A) :: #{A => [A]} | fun((A) -> A) | 1..10 | <<_:8, _:_*4>>.", "-opaque o() :: {o, term()}.",
    "-callback cb(term()) -> ok.", "-record(q, {a = 1 :: integer(), b}).", "-define(D(X), X + 1).",
    "-ifdef(X).\n-else.\n-endif.", "-export([g/1]).", "-import(lists, [map/2]).", "-compile([export_all]).",
    "-include(\"x.hrl\").", "-include_lib(\"kernel/include/file.hrl\").", "-behaviour(gen_server).",
    "-moduledoc \"\"\"\n  text\n  \"\"\".", "-doc \"x\".", "-feature(maybe_expr, enable).",
    "g(X) when is_atom(X), X =/= a; X == b -> ok.", "g(<<\"pre\", Rest/binary>>) -> Rest.",
    "g(#{a := A}, #r{b = B}, [H | T]) -> {A, B, H, T}.", "-if(?OTP_RELEASE >= 27).\n-elif(true).\n-endif.",
    "-dialyzer({nowarn_function, g/0}).", "-nifs([g/0]).", "-on_load(g/0).", "-export_type([t/0]).",
    "-file(\"x.erl\", 1).", "-custom_attr({a, [b], \"c\"}).",
  };

  private static void cuts(List<Input> r, long seed, Function<String, List<int[]>> tokens) {
    Random random = new Random(seed ^ 0x5EEDC075L);
    String trailer = "\n\ng() -> ok.\n-record(r2, {a}).\nh(#r2{a = A}) -> A.\n";
    int i = 0;
    for (String[] group : new String[][]{EXPRESSIONS, FORMS}) {
      boolean expression = group == EXPRESSIONS;
      for (String snippet : group) {
        String head = "-module(cut_" + i + ").\n" + HEADER + (expression ? "f(X, Y, L, M, R, B) ->\n  " : "");
        List<int[]> ts = tokens.apply(snippet);
        if (ts.size() < 2) continue;
        // After the opening token always; then seeded token boundaries inside the construct.
        List<Integer> cutsAt = new ArrayList<>();
        cutsAt.add(ts.get(0)[1]);
        for (int k = 1; k < CUTS_PER_SNIPPET && ts.size() > 2; k++) cutsAt.add(ts.get(1 + random.nextInt(ts.size() - 2))[1]);
        for (int k = 0; k < cutsAt.size(); k++) {
          int at = cutsAt.get(k);
          String text = head + snippet.substring(0, at) + (random.nextBoolean() ? trailer : "");
          r.add(new Input("synthetic/cut-" + i + "-" + k + "@" + at, "cut_" + i + ".erl", text));
        }
        i++;
      }
    }
  }

  // ---------------------------------------------------------------------------------------------- token soup

  private static final Map<String, String[]> LITERALS = Map.of(
    "atom_name", new String[]{"a", "ok", "error", "undefined", "is_atom", "lists"},
    "single_quote", new String[]{"'q'", "'a b'", "''", "'\\''"},
    "char", new String[]{"$a", "$\\n", "$\\x{41}", "$ "},
    "float", new String[]{"1.5", "2.0e-3", "1.0e308"},
    "integer", new String[]{"0", "42", "16#ff", "2#1010", "1_000"},
    "string", new String[]{"\"s\"", "\"\"", "\"a\\\"b\"", "\"~p~n\""},
    "sigil_string", new String[]{"~\"s\"", "~b[x]", "~S{y}", "~B<z>"},
    "triple_quoted_string", new String[]{"\"\"\"\n  t\n  \"\"\""},
    "var", new String[]{"X", "_", "_Y", "Acc", "X1"});

  private static final String[] EXTRA_TOKENS = {"%c\n", "%% @doc d\n", "%%% m\n", "-module", "-export", "-define",
    "-record", "-spec", "-type", "-include", "-ifdef", "-endif", "-else", "-feature", "-doc", "?MODULE", "?LINE", "#r",
    "#r{", "#{", "~", "\"\"\"", "\"", "'", "\n", ".\n", "%", "\\", "@"};

  static List<String> soupTokens() {
    List<String> result = new ArrayList<>();
    for (Field f : ErlangTypes.class.getFields()) {
      Object value;
      try {
        value = f.get(null);
      }
      catch (IllegalAccessException e) {
        continue;
      }
      if (!(value instanceof ErlangTokenType type)) continue;
      String debug = type.toString();
      String[] samples = LITERALS.get(debug);
      if (samples != null) result.addAll(List.of(samples));
      else result.add(debug);
    }
    result.addAll(List.of(EXTRA_TOKENS));
    return result;
  }

  private static String soupText(Random random, List<String> vocabulary, int n) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < n; i++) {
      sb.append(vocabulary.get(random.nextInt(vocabulary.size())));
      int gap = random.nextInt(10);
      sb.append(gap < 6 ? " " : gap < 8 ? "\n" : gap < 9 ? "" : "\n\n");
    }
    return sb.toString();
  }

  private static void soup(List<Input> r, long seed) {
    Random random = new Random(seed ^ 0x50F7L);
    List<String> vocabulary = soupTokens();
    for (int i = 0; i < SOUPS; i++) {
      String soup = soupText(random, vocabulary, 30 + random.nextInt(300));
      String name = "soup_" + i;
      String text = switch (i % 3) {
        case 0 -> soup;
        case 1 -> "-module(" + name + ").\n-export([f/0]).\nf() -> " + soup + ".\ng() -> ok.\n";
        default -> "-module(" + name + ").\n" + HEADER + soup + "\nf() -> ok.\n" + soupText(random, vocabulary, 20) +
                   "\n-record(after_soup, {a}).\nf(#after_soup{a = A}) -> A.\n";
      };
      r.add(new Input("synthetic/soup-" + i, name + ".erl", text));
    }
  }

  // ---------------------------------------------------------------------------------------------- random programs

  private static void programs(List<Input> r, long seed) {
    for (int i = 0; i < PROGRAMS; i++) {
      Random random = new Random(seed * 31 + i);
      String name = "gen_" + i;
      r.add(new Input("synthetic/gen-" + i, name + ".erl", new Program(random).module(name)));
    }
  }

  /** Random modules that are mostly valid Erlang, with every kind of expression, pattern and form. */
  static final class Program {
    private final Random r;
    private int[] myArity;
    private List<String> myScope = new ArrayList<>();
    private int myVars;

    Program(Random r) {
      this.r = r;
    }

    private <T> T pick(List<T> list) {
      return list.get(r.nextInt(list.size()));
    }

    String module(String name) {
      int funs = 2 + r.nextInt(5);
      myArity = new int[funs];
      for (int i = 0; i < funs; i++) myArity[i] = r.nextInt(4);
      StringBuilder sb = new StringBuilder("-module(").append(name).append(").\n");
      sb.append("-export([").append(join(funs, ", ", i -> "f" + i + "/" + myArity[i])).append("]).\n");
      sb.append("-record(r, {a = 0 :: integer(), b = [] :: list(), c}).\n-define(M(X), {m, X}).\n-define(C, 42).\n");
      sb.append("-type t() :: ").append(type(3)).append(".\n-export_type([t/0]).\n");
      for (int f = 0; f < funs; f++) {
        if (r.nextBoolean()) {
          sb.append("-spec f").append(f).append("(").append(join(myArity[f], ", ", i -> type(2))).append(") -> ").append(type(2)).append(".\n");
        }
        int clauses = 1 + r.nextInt(3);
        for (int c = 0; c < clauses; c++) {
          myScope = new ArrayList<>();
          sb.append("f").append(f).append("(").append(join(myArity[f], ", ", i -> pattern(2))).append(")");
          if (!myScope.isEmpty() && r.nextInt(3) == 0) sb.append(" when ").append(guard());
          sb.append(" ->\n    ").append(body(3)).append(c == clauses - 1 ? ".\n" : ";\n");
        }
      }
      return sb.toString();
    }

    private String fresh() {
      String v = "V" + myVars++;
      myScope.add(v);
      return v;
    }

    private String atom() {
      return pick(List.of("ok", "error", "a", "'hello world'", "undefined", "true", "'Q'"));
    }

    private String type(int d) {
      int k = r.nextInt(d <= 0 ? 4 : 12);
      return switch (k) {
        case 0 -> pick(List.of("integer()", "atom()", "binary()", "term()", "pid()", "t()"));
        case 1 -> atom();
        case 2 -> String.valueOf(r.nextInt(100));
        case 3 -> "1..10";
        case 4 -> "{" + type(d - 1) + ", " + type(d - 1) + "}";
        case 5 -> "[" + type(d - 1) + "]";
        case 6 -> type(d - 1) + " | " + type(d - 1);
        case 7 -> "#{" + type(d - 1) + " => " + type(d - 1) + "}";
        case 8 -> "fun((" + type(d - 1) + ") -> " + type(d - 1) + ")";
        case 9 -> "#r{}";
        case 10 -> "<<_:8, _:_*4>>";
        default -> "maybe_improper_list(" + type(d - 1) + ", " + type(d - 1) + ")";
      };
    }

    private String pattern(int d) {
      int k = r.nextInt(d <= 0 ? 5 : 11);
      return switch (k) {
        case 0, 1 -> fresh();
        case 2 -> "_";
        case 3 -> atom();
        case 4 -> String.valueOf(r.nextInt(10));
        case 5 -> "{" + pattern(d - 1) + ", " + pattern(d - 1) + "}";
        case 6 -> "[" + pattern(d - 1) + " | " + pattern(d - 1) + "]";
        case 7 -> "#{" + atom() + " := " + pattern(d - 1) + "}";
        case 8 -> "#r{a = " + pattern(d - 1) + "}";
        case 9 -> "<<" + fresh() + ":8, " + fresh() + "/binary>>";
        default -> "\"ab\" ++ " + fresh();
      };
    }

    private String guard() {
      if (myScope.isEmpty()) return "true";
      String v = pick(myScope);
      return pick(List.of("is_integer(" + v + ")", v + " > 0", "is_atom(" + v + ") andalso " + v + " =/= a",
                          "is_list(" + v + "); is_map(" + v + ")", "map_size(" + v + ") > 0", "is_record(" + v + ", r)"));
    }

    private String body(int d) {
      int n = 1 + r.nextInt(3);
      StringBuilder sb = new StringBuilder();
      for (int i = 0; i < n; i++) {
        if (i > 0) sb.append(",\n    ");
        if (i < n - 1 && r.nextBoolean()) {
          String e = expr(d);
          sb.append(fresh()).append(" = ").append(e);
        }
        else {
          sb.append(expr(d));
        }
      }
      return sb.toString();
    }

    /** A nested clause: the variables it binds do not leak into the enclosing scope. */
    private String clause(int d) {
      List<String> saved = new ArrayList<>(myScope);
      String head = pattern(1);
      String g = r.nextInt(4) == 0 && !myScope.isEmpty() ? " when " + guard() : "";
      String result = head + g + " -> " + body(d - 1);
      myScope = saved;
      return result;
    }

    private String call(int d) {
      int f = r.nextInt(myArity.length);
      String args = join(myArity[f], ", ", i -> expr(d - 1));
      return (r.nextInt(4) == 0 ? "?MODULE:" : "") + "f" + f + "(" + args + ")";
    }

    private String expr(int d) {
      int k = r.nextInt(d <= 0 ? 6 : 27);
      return switch (k) {
        case 0 -> myScope.isEmpty() ? atom() : pick(myScope);
        case 1 -> atom();
        case 2 -> pick(List.of(String.valueOf(r.nextInt(1000)), "16#FF", "$a", "1_000", "2#1010"));
        case 3 -> pick(List.of("1.5", "2.0e-3", "-0.0"));
        case 4 -> pick(List.of("\"str\"", "~\"sigil\"", "<<\"bin\">>", "\"\"\"\n      triple\n      \"\"\"", "~B[raw]"));
        case 5 -> myScope.isEmpty() ? "[]" : pick(myScope);
        case 6 -> "{" + expr(d - 1) + ", " + expr(d - 1) + "}";
        case 7 -> r.nextBoolean() ? "[" + expr(d - 1) + ", " + expr(d - 1) + "]" : "[" + expr(d - 1) + " | " + expr(d - 1) + "]";
        case 8 -> myScope.isEmpty() || r.nextBoolean() ? "#{" + atom() + " => " + expr(d - 1) + "}"
                                                       : pick(myScope) + "#{" + atom() + " => " + expr(d - 1) + "}";
        case 9 -> pick(List.of("#r{a = " + expr(d - 1) + "}", "(" + expr(d - 1) + ")#r.b", "#r.c",
                               "(" + expr(d - 1) + ")#r{c = " + expr(d - 1) + "}"));
        case 10 -> "<<(" + expr(d - 1) + "):8, \"s\", (" + expr(d - 1) + ")/binary>>";
        case 11 -> call(d);
        case 12 -> pick(List.of("lists:reverse(" + expr(d - 1) + ")", "erlang:element(1, " + expr(d - 1) + ")",
                                "maps:get(a, " + expr(d - 1) + ", " + expr(d - 1) + ")"));
        case 13 -> {
          int f = r.nextInt(myArity.length);
          yield pick(List.of("fun() -> " + body(d - 1) + " end", "fun f" + f + "/" + myArity[f], "fun lists:map/2",
                             "fun Loop(0) -> 0; Loop(N) -> Loop(N - 1) end", "fun(" + clause(d) + ") end"));
        }
        case 14 -> "case " + expr(d - 1) + " of\n      " + clause(d) + ";\n      _ -> " + expr(d - 1) + "\n    end";
        case 15 -> "if " + (myScope.isEmpty() ? "true" : guard()) + " -> " + expr(d - 1) + "; true -> " + expr(d - 1) + " end";
        case 16 -> "receive " + clause(d) + " after " + r.nextInt(100) + " -> " + expr(d - 1) + " end";
        case 17 -> "try " + body(d - 1) + " of " + clause(d) + " catch C:R:S -> {C, R, S} after ok end";
        case 18 -> "begin " + body(d - 1) + " end";
        case 19 -> {
          List<String> saved = new ArrayList<>(myScope);
          String gen = fresh();
          String e = "[" + expr(d - 1) + " || " + gen + " <- " + expr(d - 1) + ", " + gen + " =/= a]";
          myScope = saved;
          yield e;
        }
        case 20 -> "<< <<X:8>> || <<X:8>> <= " + expr(d - 1) + " >>";
        case 21 -> "#{K => V || K := V <- " + expr(d - 1) + "}";
        case 22 -> {
          List<String> saved = new ArrayList<>(myScope);
          String p = pattern(1);
          String e = "maybe {ok, " + p + "} ?= " + expr(d - 1) + ", " + expr(d - 1) + " else _ -> " + atom() + " end";
          myScope = saved;
          yield e;
        }
        case 23 -> "(" + expr(d - 1) + " " + pick(List.of("+", "-", "*", "/", "div", "rem", "band", "bor", "bxor", "bsl",
                                                          "bsr", "and", "or", "xor", "andalso", "orelse", "==", "/=",
                                                          "=:=", "=/=", "<", ">", "=<", ">=", "++", "--", "!")) + " " + expr(d - 1) + ")";
        case 24 -> pick(List.of("-", "not ", "bnot ", "+")) + "(" + expr(d - 1) + ")";
        case 25 -> pick(List.of("?M(" + expr(d - 1) + ")", "?C", "?MODULE", "?LINE", "?FUNCTION_NAME"));
        default -> "catch " + expr(d - 1);
      };
    }
  }
}
