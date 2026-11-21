package org.intellij.erlang.macrolayer.fuzz;

import org.jetbrains.annotations.NotNull;

import java.util.*;

/**
 * Generates small Erlang projects full of macros: a main module {@code m.erl}, often a second includer {@code m2.erl},
 * headers ({@code h1.hrl}, a chain {@code h2.hrl} including {@code h1.hrl}), a header of a generated application reached
 * with {@code -include_lib}, and {@code eunit.hrl} of the mock SDK. The macros are constants, macros with arguments and
 * several arities, nested, stringified, fragments (argument lists, list tails, {@code begin}/{@code end}, clause heads,
 * operators, module prefixes), macros that make whole forms, records and exports, conditionals, predefined macros, and
 * some wrong uses (circular, wrong arity, undefined, unterminated). Deterministic for a given {@link Random}.
 */
public final class MacroCaseGenerator {
  public static final String[] PREDEFINED = {"?MODULE", "?LINE", "?FILE", "?MACHINE", "?FUNCTION_NAME", "?FUNCTION_ARITY",
    "?MODULE_STRING", "?OTP_RELEASE", "?FEATURE_AVAILABLE(maybe_expr)", "?FEATURE_ENABLED(maybe_expr)"};

  private final Random r;
  private final List<String> myDefines = new ArrayList<>();
  /** name -> arities (-1: no parentheses) of the macros visible in the main module. */
  private final Map<String, Set<Integer>> myMacros = new LinkedHashMap<>();
  private final List<String> myFunctions = new ArrayList<>();
  private final Set<String> myMadeFunctions = new LinkedHashSet<>();
  private int myNames;

  public MacroCaseGenerator(@NotNull Random r) {
    this.r = r;
  }

  private boolean chance(int percent) {
    return r.nextInt(100) < percent;
  }

  private <T> T pick(List<T> l) {
    return l.get(r.nextInt(l.size()));
  }

  private String pick(String... l) {
    return l[r.nextInt(l.length)];
  }

  private String fresh(String prefix) {
    return prefix + (myNames++);
  }

  /** A project: path -> text; the main module is {@code m.erl}. */
  public @NotNull Map<String, String> project() {
    Map<String, String> files = new LinkedHashMap<>();
    StringBuilder h1 = new StringBuilder("%% generated header\n");
    int nh = r.nextInt(4);
    for (int i = 0; i < nh; i++) h1.append(define("H", true)).append('\n');
    if (chance(30)) h1.append("-ifndef(H1_GUARD).\n-define(H1_GUARD, true).\n").append(define("HG", true)).append("\n-endif.\n");
    files.put("h1.hrl", h1.toString());
    if (chance(50)) {
      files.put("h2.hrl", "-include(\"h1.hrl\").\n" + define("HH", true) + "\n");
    }
    if (chance(40)) {
      files.put("lib/applib/ebin/applib.app", "{application, applib, [{vsn, \"1\"}]}.\n");
      files.put("lib/applib/include/app.hrl", define("APP", true) + "\n" + define("APP", true) + "\n");
    }
    String main = module("m", files);
    files.put("m.erl", main);
    if (chance(50)) {
      Map<String, Set<Integer>> keep = new LinkedHashMap<>(myMacros);
      myMacros.clear();
      myFunctions.clear();
      files.put("m2.erl", module("m2", files));
      myMacros.clear();
      myMacros.putAll(keep);
    }
    return files;
  }

  public @NotNull Set<String> macroNames() {
    return myMacros.keySet();
  }

  private String module(String name, Map<String, String> files) {
    StringBuilder sb = new StringBuilder();
    if (chance(5)) sb.append("%% -*- coding: utf-8 -*-\n%% ünïcödé comment\n");
    sb.append("-module(").append(name).append(").\n");
    if (files.containsKey("h2.hrl") && chance(60)) sb.append("-include(\"h2.hrl\").\n");
    else if (chance(70)) sb.append("-include(\"h1.hrl\").\n");
    if (files.containsKey("lib/applib/include/app.hrl") && chance(70)) sb.append("-include_lib(\"applib/include/app.hrl\").\n");
    if (chance(15)) sb.append("-include_lib(\"eunit/include/eunit.hrl\").\n");
    // the header macros are visible (approximately: the generator does not track the -ifndef guard)
    for (String d : myDefines) noteDefine(d);
    int nd = 2 + r.nextInt(7);
    List<String> forms = new ArrayList<>();
    for (int i = 0; i < nd; i++) forms.add(define("M", false));
    int nf = 1 + r.nextInt(4);
    for (int i = 0; i < nf; i++) forms.add(function());
    if (chance(40)) forms.add(conditional());
    if (chance(30)) forms.add(madeForm());
    if (chance(20)) forms.add(wrongUse());
    // keep -define before use most of the time, sometimes shuffle (use before define is an error epp reports)
    if (chance(10)) Collections.shuffle(forms, r);
    if (chance(60) && !myFunctions.isEmpty()) {
      StringBuilder ex = new StringBuilder("-export([");
      for (int i = 0; i < myFunctions.size(); i++) ex.append(i > 0 ? ", " : "").append(myFunctions.get(i));
      sb.append(ex).append("]).\n");
    }
    for (String f : forms) sb.append(f).append('\n');
    if (chance(5)) return sb.toString().replace("\n", "\r\n");
    return sb.toString();
  }

  private void noteDefine(String d) {
    // -define(NAME, ...) or -define(NAME(A, B), ...)
    int open = d.indexOf('(');
    int i = open + 1;
    while (i < d.length() && (Character.isLetterOrDigit(d.charAt(i)) || d.charAt(i) == '_')) i++;
    String n = d.substring(open + 1, i);
    int arity = -1;
    if (i < d.length() && d.charAt(i) == '(') {
      int close = d.indexOf(')', i);
      String params = d.substring(i + 1, close).trim();
      arity = params.isEmpty() ? 0 : params.split(",").length;
    }
    myMacros.computeIfAbsent(n, k -> new LinkedHashSet<>()).add(arity);
  }

  /** One {@code -define}; records it as visible. */
  private String define(String prefix, boolean header) {
    String n = fresh(prefix);
    String d;
    switch (r.nextInt(16)) {
      case 0 -> d = "-define(" + n + ", " + r.nextInt(100) + ").";
      case 1 -> d = "-define(" + n + "(A, B), A + B).";
      case 2 -> d = "-define(" + n + "(X), {X, ?MODULE}).\n-define(" + n + "(X, Y), [X | Y]).";
      case 3 -> d = "-define(" + n + ", " + (myMacros.isEmpty() ? "1" : use(true)) + " + 1).";
      case 4 -> d = "-define(" + n + "(X), ??X).";
      case 5 -> d = "-define(" + n + ", (1, 2)).";                       // an argument list
      case 6 -> d = "-define(" + n + ", | T]).";                          // a list tail
      case 7 -> d = "-define(" + n + ", begin).\n-define(" + n + "_END, end).";
      case 8 -> d = "-define(" + n + "(Name), Name(X) ->).";             // a clause head
      case 9 -> d = "-define(" + n + ", +).";                             // an operator
      case 10 -> d = "-define(" + n + ", lists).";                        // a module prefix
      case 11 -> d = "-define(" + n + "(F), F() -> ok.).";                // a whole function
      case 12 -> d = "-define(" + n + ", -record(" + n.toLowerCase(Locale.ROOT) + "_rec, {a = 1, b}).).";
      case 13 -> d = "-define(" + n + "(V), V = 42).";                    // binds a variable
      case 14 -> d = "-define(" + n + "(), zero_args).";
      default -> d = "-define(" + n + ", " + pick(PREDEFINED) + ").";
    }
    if (header) myDefines.add(d);
    // note every define of the snippet (case 2 and 7 have two)
    for (String line : d.split("\n")) noteDefine(line);
    if (d.contains("_END, end")) myMacros.computeIfAbsent(n + "_END", k -> new LinkedHashSet<>()).add(-1);
    return d;
  }

  /** A use of a visible macro (or a predefined one), with a fitting number of arguments most of the time. */
  private String use(boolean expressionOnly) {
    if (myMacros.isEmpty() || chance(15)) return pick(PREDEFINED);
    List<String> names = new ArrayList<>(myMacros.keySet());
    String n = pick(names);
    int arity = pick(new ArrayList<>(myMacros.get(n)));
    if (chance(5)) arity = arity + 1;                                     // wrong arity
    StringBuilder sb = new StringBuilder("?").append(n);
    if (arity >= 0) {
      sb.append('(');
      for (int i = 0; i < arity; i++) sb.append(i > 0 ? ", " : "").append(argument());
      sb.append(')');
    }
    return sb.toString();
  }

  private String argument() {
    return pick("1", "X", "foo", "{a, b}", "[1, 2]", "\"str\"", "fun() -> ok end", "X + 1", "?MODULE", "<<1>>", "#{k => v}", "f(X)");
  }

  private String function() {
    String name = fresh("f");
    int arity = r.nextInt(3);
    myFunctions.add(name + "/" + arity);
    StringBuilder params = new StringBuilder();
    for (int i = 0; i < arity; i++) params.append(i > 0 ? ", " : "").append(i == 0 ? "X" : "T" + i);
    StringBuilder body = new StringBuilder();
    if (arity == 0) body.append("X = 1,\n    ");
    int n = 1 + r.nextInt(4);
    for (int i = 0; i < n; i++) {
      body.append(statement());
      body.append(i + 1 < n ? ",\n    " : "");
    }
    return name + "(" + params + ") ->\n    " + body + ".";
  }

  private String statement() {
    String u = use(true);
    return switch (r.nextInt(14)) {
      case 0 -> "Y" + (myNames++) + " = " + u;
      case 1 -> "io:format(\"~p~n\", [" + u + "])";
      case 2 -> "case X of " + u + " -> ok; _ -> " + u + " end";
      case 3 -> "[" + u + " || _ <- [1, 2]]";
      case 4 -> "{" + u + ", " + use(true) + "}";
      case 5 -> fragmentUse();
      case 6 -> "fun(" + "A) -> A + " + u + " end";
      case 7 -> "begin " + u + " end";
      case 8 -> "<<" + "(" + u + "):8>>";
      case 9 -> "#{key => " + u + "}";
      case 10 -> "try " + u + " catch _:_ -> error end";
      case 11 -> pick(PREDEFINED);
      case 12 -> myMadeFunctions.isEmpty() ? u : pick(new ArrayList<>(myMadeFunctions)) + "()";
      default -> u;
    };
  }

  /** A use of a fragment macro in a position where the expansion parses, if such a macro is visible. */
  private String fragmentUse() {
    for (Map.Entry<String, Set<Integer>> e : myMacros.entrySet()) {
      String n = e.getKey();
      if (!chance(30)) continue;
      if (n.endsWith("_END")) return "?" + n.substring(0, n.length() - 4) + " 1 ?" + n;
    }
    return pick("lists:reverse([1, 2])", "?MODULE:module_info()", "X");
  }

  private String conditional() {
    String n = myMacros.isEmpty() ? "UNDEFINED_ONE" : pick(new ArrayList<>(myMacros.keySet()));
    String f = fresh("cond");
    myFunctions.add(f + "/0");
    return switch (r.nextInt(5)) {
      case 0 -> "-ifdef(" + n + ").\n" + f + "() -> ?" + n + "_maybe.\n-else.\n" + f + "() -> undefined.\n-endif.";
      case 1 -> "-ifndef(" + n + ").\n-define(" + n + ", 0).\n-endif.\n" + f + "() -> ok.";
      case 2 -> "-if(?OTP_RELEASE >= 27).\n" + f + "() -> new.\n-elif(?OTP_RELEASE >= 25).\n" + f + "() -> mid.\n-else.\n" + f +
                "() -> old.\n-endif.";
      case 3 -> "-undef(" + n + ").\n" + f + "() -> ok.";
      default -> "-ifdef(TEST).\n-define(" + f.toUpperCase(Locale.ROOT) + ", test).\n-else.\n-define(" + f.toUpperCase(Locale.ROOT) +
                 ", prod).\n-endif.\n" + f + "() -> ?" + f.toUpperCase(Locale.ROOT) + ".";
    };
  }

  /** A top-level form made by a macro: a function, an export, a record. */
  private String madeForm() {
    String m = fresh("GEN");
    String fn = fresh("gen");
    myMadeFunctions.add(fn);
    return switch (r.nextInt(3)) {
      case 0 -> "-define(" + m + "(F), F() -> ok.).\n?" + m + "(" + fn + ").";
      case 1 -> "-define(" + m + "(F, V), F() -> V.).\n?" + m + "(" + fn + ", 42).\n-export([" + fn + "/0]).";
      default -> "-define(" + m + ", -record(" + fn + "_rec, {a, b})).\n?" + m + ".\n" + fn + "() -> #" + fn + "_rec{a = 1}.";
    };
  }

  private String wrongUse() {
    String f = fresh("bad");
    return switch (r.nextInt(5)) {
      case 0 -> "-define(LOOP_A" + myNames + ", ?LOOP_B" + myNames + ").\n-define(LOOP_B" + myNames + ", ?LOOP_A" + myNames + ").\n" + f +
                "() -> ?LOOP_A" + myNames + ".";
      case 1 -> f + "() -> ?NOT_DEFINED_" + myNames + ".";
      case 2 -> f + "() -> ?" + (myMacros.isEmpty() ? "X" : pick(new ArrayList<>(myMacros.keySet()))) + "(1, 2, 3, 4).";
      case 3 -> f + "() -> ?" + (myMacros.isEmpty() ? "X" : pick(new ArrayList<>(myMacros.keySet()))) + "(1, (2.";
      default -> f + "() -> ?.";
    };
  }
}
