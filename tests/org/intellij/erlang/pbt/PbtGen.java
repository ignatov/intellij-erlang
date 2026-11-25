package org.intellij.erlang.pbt;

import org.intellij.erlang.pbt.PbtNode.Cat;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.function.BiFunction;

/**
 * Generator v0 of the property-based tests: a random module that erlc accepts without a syntax error, built as a
 * {@link PbtNode} tree. The same {@code (seed, index)} gives the same module every time.
 * <p>
 * It is valid by construction: every variable gets a name that is new in the module, a variable is bound only by a
 * pattern of a clause head, a generator or a statement {@code Pattern = Expr} of a body, and the variables bound inside
 * a case, receive, try, fun or comprehension are never used after it. On purpose it also makes the things erlc warns
 * about: variables that are bound and never used, unused local functions and types, unbound variables ({@code UnboundN})
 * and calls of undefined local functions ({@code missing_N}).
 * <p>
 * To add a construct, add one entry to {@link #EXPRS}, {@link #PATS}, {@link #GUARDS} or {@link #TYPES}: a name (it is
 * the key of the usage counts in the report), a weight, whether it nests (a nesting construct is not chosen at depth
 * 0) and the method that builds it. A method returns {@code null} when it does not apply, for example a variable
 * reference with no variable in scope.
 */
final class PbtGen {
  static final String MODULE = "pbt";

  private final Random r;
  private final Map<String, Integer> myUsage;
  private int myVars;
  private int myMissing;
  private int myUnbound;
  private int myIgnored;
  private final List<Fn> myFns = new ArrayList<>();
  private final List<Rec> myRecs = new ArrayList<>();
  private final List<Ty> myTypes = new ArrayList<>();

  record Fn(String name, int arity) {
  }

  record Rec(String name, List<String> fields) {
  }

  record Ty(String name, int arity) {
  }

  /** The variables a clause body can use. Immutable. */
  static final class Scope {
    final List<String> vars;

    Scope(List<String> vars) {
      this.vars = List.copyOf(vars);
    }

    Scope with(Collection<String> more) {
      List<String> all = new ArrayList<>(vars);
      all.addAll(more);
      return new Scope(all);
    }
  }

  /** One construct of the generator. */
  record Choice<G>(String name, int weight, boolean nests, G gen) {
  }

  @FunctionalInterface
  interface PatGen {
    @Nullable PbtNode gen(Scope scope, List<String> binds, int depth);
  }

  private final List<Choice<BiFunction<Scope, Integer, PbtNode>>> EXPRS = new ArrayList<>();
  private final List<Choice<PatGen>> PATS = new ArrayList<>();
  private final List<Choice<BiFunction<Scope, Integer, PbtNode>>> GUARDS = new ArrayList<>();
  private final List<Choice<BiFunction<List<String>, Integer, PbtNode>>> TYPES = new ArrayList<>();

  PbtGen(long seed, int index, Map<String, Integer> usage) {
    r = new Random(seed * 1_000_003L + index * 7_919L + 17);
    myUsage = usage;

    // ------------------------------------------------------------------------------------------------ expressions
    expr("expr.var", 14, false, (s, d) -> s.vars.isEmpty() ? null : leaf("expr.var", Cat.EXPR, pick(s.vars)));
    expr("expr.unbound_var", 1, false, (s, d) -> leaf("expr.unbound_var", Cat.EXPR, "Unbound" + ++myUnbound));
    expr("expr.atom", 5, false, (s, d) -> leaf("expr.atom", Cat.EXPR, pick("ok", "error", "foo", "bar", "undefined", "true", "false")));
    expr("expr.quoted_atom", 1, false, (s, d) -> leaf("expr.quoted_atom", Cat.EXPR, pick("'Quoted'", "'hello world'", "'a-b'", "'case'")));
    expr("expr.int", 5, false, (s, d) -> leaf("expr.int", Cat.EXPR, pick("0", "1", "2", "42", "1000")));
    expr("expr.based_int", 1, false, (s, d) -> leaf("expr.based_int", Cat.EXPR, pick("16#FF", "2#1010", "8#17", "1_000", "36#Zz")));
    expr("expr.char", 1, false, (s, d) -> leaf("expr.char", Cat.EXPR, pick("$a", "$\\n", "$ ", "$\\x{41}")));
    expr("expr.float", 1, false, (s, d) -> leaf("expr.float", Cat.EXPR, pick("1.5", "0.0", "2.5e3", "1.0e-2")));
    expr("expr.string", 2, false, (s, d) -> leaf("expr.string", Cat.EXPR, pick("\"abc\"", "\"\"", "\"a\\nb\"", "\"x\" \"y\"")));
    expr("expr.nil", 2, false, (s, d) -> leaf("expr.nil", Cat.EXPR, "[]"));
    expr("expr.macro_module", 1, false, (s, d) -> leaf("expr.macro_module", Cat.EXPR, "?MODULE"));
    expr("expr.tuple", 4, true, (s, d) -> seq("expr.tuple", Cat.EXPR, "{", ", ", "}", 0, exprs(s, d - 1, 0, 3)));
    expr("expr.list", 3, true, (s, d) -> seq("expr.list", Cat.EXPR, "[", ", ", "]", 0, exprs(s, d - 1, 1, 3)));
    expr("expr.cons", 1, true, (s, d) -> node("expr.cons", Cat.EXPR, "[", expr(s, d - 1), " | ", expr(s, d - 1), "]"));
    expr("expr.arith", 3, true, (s, d) -> binop("expr.arith", s, d, "+", "-", "*", "div", "rem", "/", "band", "bor", "bxor", "bsl", "bsr"));
    expr("expr.compare", 2, true, (s, d) -> binop("expr.compare", s, d, "==", "/=", "=<", "<", ">=", ">", "=:=", "=/="));
    expr("expr.bool", 1, true, (s, d) -> binop("expr.bool", s, d, "andalso", "orelse", "and", "or", "xor"));
    expr("expr.unary", 1, true, (s, d) -> node("expr.unary", Cat.EXPR, "(", pick("-", "+", "not ", "bnot "), expr(s, d - 1), ")"));
    expr("expr.listop", 1, true, (s, d) -> binop("expr.listop", s, d, "++", "--"));
    expr("expr.send", 1, true, (s, d) -> binop("expr.send", s, d, "!"));
    expr("expr.call", 4, true, (s, d) -> {
      Fn f = pick(myFns);
      return node("expr.call", Cat.EXPR, f.name, seq("args", Cat.OTHER, "(", ", ", ")", f.arity, exprs(s, d - 1, f.arity, f.arity)));
    });
    expr("expr.call_undefined", 1, true, (s, d) -> {
      int arity = r.nextInt(3);
      return node("expr.call_undefined", Cat.EXPR, "missing_" + ++myMissing,
                  seq("args", Cat.OTHER, "(", ", ", ")", arity, exprs(s, d - 1, arity, arity)));
    });
    expr("expr.call_bif", 2, true, (s, d) -> {
      String[] bif = pick(new String[]{"length", "1"}, new String[]{"element", "2"}, new String[]{"tuple_size", "1"},
                          new String[]{"abs", "1"}, new String[]{"hd", "1"}, new String[]{"tl", "1"},
                          new String[]{"atom_to_list", "1"}, new String[]{"self", "0"}, new String[]{"max", "2"},
                          new String[]{"is_atom", "1"}, new String[]{"integer_to_list", "1"});
      int arity = Integer.parseInt(bif[1]);
      return node("expr.call_bif", Cat.EXPR, bif[0], seq("args", Cat.OTHER, "(", ", ", ")", arity, exprs(s, d - 1, arity, arity)));
    });
    expr("expr.fun_ref", 1, false, (s, d) -> {
      Fn f = pick(myFns);
      return leaf("expr.fun_ref", Cat.EXPR, "fun " + f.name + "/" + f.arity);
    });
    expr("expr.fun_ref_undefined", 1, false, (s, d) -> leaf("expr.fun_ref_undefined", Cat.EXPR, "fun missing_" + ++myMissing + "/" + r.nextInt(3)));
    expr("expr.fun", 2, true, (s, d) -> {
      int arity = r.nextInt(3);
      List<PbtNode> clauses = new ArrayList<>();
      for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) clauses.add(funClause(s, d - 1, arity, null));
      return seq("expr.fun", Cat.EXPR, "fun", "; ", " end", 1, clauses);
    });
    expr("expr.named_fun", 1, true, (s, d) -> {
      int arity = r.nextInt(3);
      String name = fresh();
      List<PbtNode> clauses = new ArrayList<>();
      for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) clauses.add(funClause(s, d - 1, arity, name));
      return seq("expr.named_fun", Cat.EXPR, "fun ", "; ", " end", 1, clauses);
    });
    expr("expr.case", 3, true, (s, d) -> {
      List<PbtNode> clauses = new ArrayList<>();
      for (int i = 0, n = 1 + r.nextInt(3); i < n; i++) clauses.add(crClause("case_clause", s, d - 1));
      return node("expr.case", Cat.EXPR, "case ", expr(s, d - 1), " of ", seq("clauses", Cat.OTHER, "", "; ", "", 1, clauses), " end");
    });
    expr("expr.if", 2, true, (s, d) -> {
      List<PbtNode> clauses = new ArrayList<>();
      for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) {
        clauses.add(node("if_clause", Cat.CLAUSE, guard(s, d - 1, false), " -> ", body(s, d - 1, ", ")));
      }
      if (r.nextBoolean()) clauses.add(node("if_clause", Cat.CLAUSE, "true -> ", body(s, d - 1, ", ")));
      return node("expr.if", Cat.EXPR, "if ", seq("clauses", Cat.OTHER, "", "; ", "", 1, clauses), " end");
    });
    expr("expr.receive", 1, true, (s, d) -> {
      List<PbtNode> clauses = new ArrayList<>();
      for (int i = 0, n = r.nextInt(3); i < n; i++) clauses.add(crClause("receive_clause", s, d - 1));
      List<PbtNode> after = new ArrayList<>();
      if (clauses.isEmpty() || r.nextBoolean()) {
        after.add(node("after", Cat.OTHER, expr(s, d - 1), " -> ", body(s, d - 1, ", ")));
      }
      return node("expr.receive", Cat.EXPR, "receive", seq("clauses", Cat.OTHER, " ", "; ", "", 0, clauses),
                  PbtNode.optional("after", " after ", "", "", after), " end");
    });
    expr("expr.try", 2, true, (s, d) -> {
      List<PbtNode> of = new ArrayList<>();
      if (r.nextInt(3) == 0) for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) of.add(crClause("try_clause", s, d - 1));
      List<PbtNode> catches = new ArrayList<>();
      List<PbtNode> after = new ArrayList<>();
      int shape = r.nextInt(4);
      if (shape != 0) for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) catches.add(catchClause(s, d - 1));
      if (shape == 0 || shape == 3) after.add(body(s, d - 1, ", "));
      return node("expr.try", Cat.EXPR, "try ", body(s, d - 1, ", "),
                  PbtNode.optional("try_of", " of ", "; ", "", of),
                  PbtNode.optional("try_catch", " catch ", "; ", "", catches),
                  PbtNode.optional("try_after", " after ", "", "", after), " end");
    });
    expr("expr.catch", 1, true, (s, d) -> node("expr.catch", Cat.EXPR, "(catch ", expr(s, d - 1), ")"));
    expr("expr.block", 1, true, (s, d) -> node("expr.block", Cat.EXPR, "begin ", body(s, d - 1, ", "), " end"));
    expr("expr.lc", 2, true, (s, d) -> {
      List<String> binds = new ArrayList<>();
      List<PbtNode> qs = qualifiers(s, d - 1, binds);
      return node("expr.lc", Cat.EXPR, "[", expr(s.with(binds), d - 1), " || ", seq("qualifiers", Cat.OTHER, "", ", ", "", 1, qs), "]");
    });
    expr("expr.bc", 1, true, (s, d) -> {
      List<String> binds = new ArrayList<>();
      List<PbtNode> qs = qualifiers(s, d - 1, binds);
      return node("expr.bc", Cat.EXPR, "<< ", node("bc_value", Cat.OTHER, "<<", segValue(s.with(binds), d - 1), ":8>>"), " || ",
                  seq("qualifiers", Cat.OTHER, "", ", ", "", 1, qs), " >>");
    });
    expr("expr.mc", 1, true, (s, d) -> {
      List<String> binds = new ArrayList<>();
      List<PbtNode> qs = qualifiers(s, d - 1, binds);
      Scope in = s.with(binds);
      return node("expr.mc", Cat.EXPR, "#{", expr(in, d - 1), " => ", expr(in, d - 1), " || ",
                  seq("qualifiers", Cat.OTHER, "", ", ", "", 1, qs), "}");
    });
    expr("expr.map", 2, true, (s, d) -> {
      List<PbtNode> assocs = new ArrayList<>();
      for (int i = 0, n = r.nextInt(3); i < n; i++) assocs.add(node("map_assoc", Cat.OTHER, mapKey(s, d - 1), " => ", expr(s, d - 1)));
      return seq("expr.map", Cat.EXPR, "#{", ", ", "}", 0, assocs);
    });
    expr("expr.map_update", 1, true, (s, d) -> {
      List<PbtNode> assocs = new ArrayList<>();
      for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) {
        assocs.add(node("map_assoc", Cat.OTHER, mapKey(s, d - 1), pick(" => ", " := "), expr(s, d - 1)));
      }
      return node("expr.map_update", Cat.EXPR, primary(s, d - 1), seq("assocs", Cat.OTHER, "#{", ", ", "}", 1, assocs));
    });
    expr("expr.record", 2, true, (s, d) -> {
      if (myRecs.isEmpty()) return null;
      Rec rec = pick(myRecs);
      return node("expr.record", Cat.EXPR, "#" + rec.name, recordFields(rec, s, d - 1));
    });
    expr("expr.record_field", 1, true, (s, d) -> {
      if (myRecs.isEmpty()) return null;
      Rec rec = pick(myRecs);
      return node("expr.record_field", Cat.EXPR, primary(s, d - 1), "#" + rec.name + "." + pick(rec.fields));
    });
    expr("expr.record_update", 1, true, (s, d) -> {
      if (myRecs.isEmpty()) return null;
      Rec rec = pick(myRecs);
      return node("expr.record_update", Cat.EXPR, primary(s, d - 1), "#" + rec.name, recordFields(rec, s, d - 1));
    });
    expr("expr.record_index", 1, false, (s, d) -> {
      if (myRecs.isEmpty()) return null;
      Rec rec = pick(myRecs);
      return leaf("expr.record_index", Cat.EXPR, "#" + rec.name + "." + pick(rec.fields));
    });
    expr("expr.binary", 2, true, (s, d) -> {
      List<PbtNode> segs = new ArrayList<>();
      for (int i = 0, n = r.nextInt(3); i < n; i++) {
        segs.add(switch (r.nextInt(5)) {
          case 0 -> node("bin_seg", Cat.OTHER, segValue(s, d - 1), pick(":8", ":16/little", ":4", "/integer", ":1/unit:8"));
          case 1 -> node("bin_seg", Cat.OTHER, segValue(s, d - 1), pick("/binary", "/bits", "/bitstring"));
          case 2 -> node("bin_seg", Cat.OTHER, segValue(s, d - 1), pick("/utf8", "/utf16", "/utf32"));
          case 3 -> leaf("bin_seg", Cat.OTHER, pick("\"abc\"", "\"x\"/utf8", "1.5/float", "$z"));
          default -> node("bin_seg", Cat.OTHER, segValue(s, d - 1));
        });
      }
      return seq("expr.binary", Cat.EXPR, "<<", ", ", ">>", 0, segs);
    });

    // --------------------------------------------------------------------------------------------------- patterns
    pat("pat.var", 10, false, (s, b, d) -> {
      String v = fresh();
      b.add(v);
      return leaf("pat.var", Cat.PAT, v);
    });
    pat("pat.bound_var", 2, false, (s, b, d) -> s.vars.isEmpty() ? null : leaf("pat.bound_var", Cat.PAT, pick(s.vars)));
    pat("pat.wildcard", 3, false, (s, b, d) -> leaf("pat.wildcard", Cat.PAT, "_"));
    pat("pat.underscore_var", 1, false, (s, b, d) -> leaf("pat.underscore_var", Cat.PAT, "_Ignored" + ++myIgnored));
    pat("pat.atom", 3, false, (s, b, d) -> leaf("pat.atom", Cat.PAT, pick("ok", "error", "foo", "'Quoted'", "true")));
    pat("pat.int", 3, false, (s, b, d) -> leaf("pat.int", Cat.PAT, pick("0", "1", "42", "-1", "16#10", "$a")));
    pat("pat.float", 1, false, (s, b, d) -> leaf("pat.float", Cat.PAT, pick("1.5", "-0.5")));
    pat("pat.string", 1, false, (s, b, d) -> leaf("pat.string", Cat.PAT, pick("\"abc\"", "\"\"")));
    pat("pat.nil", 1, false, (s, b, d) -> leaf("pat.nil", Cat.PAT, "[]"));
    pat("pat.tuple", 3, true, (s, b, d) -> seq("pat.tuple", Cat.PAT, "{", ", ", "}", 0, pats(s, b, d - 1, 0, 3)));
    pat("pat.list", 2, true, (s, b, d) -> seq("pat.list", Cat.PAT, "[", ", ", "]", 0, pats(s, b, d - 1, 1, 3)));
    pat("pat.cons", 2, true, (s, b, d) -> node("pat.cons", Cat.PAT, "[", pat(s, b, d - 1), " | ", pat(s, b, d - 1), "]"));
    pat("pat.string_prefix", 1, true, (s, b, d) -> node("pat.string_prefix", Cat.PAT, "\"ab\" ++ ", pat(s, b, d - 1)));
    pat("pat.map", 1, true, (s, b, d) -> {
      List<PbtNode> assocs = new ArrayList<>();
      for (int i = 0, n = r.nextInt(3); i < n; i++) {
        assocs.add(node("map_pat_assoc", Cat.OTHER, pick("a", "b", "1", "\"k\""), " := ", pat(s, b, d - 1)));
      }
      return seq("pat.map", Cat.PAT, "#{", ", ", "}", 0, assocs);
    });
    pat("pat.record", 1, true, (s, b, d) -> {
      if (myRecs.isEmpty()) return null;
      Rec rec = pick(myRecs);
      List<PbtNode> fields = new ArrayList<>();
      for (String f : rec.fields) if (r.nextBoolean()) fields.add(node("record_pat_field", Cat.OTHER, f + " = ", pat(s, b, d - 1)));
      return node("pat.record", Cat.PAT, "#" + rec.name, seq("fields", Cat.OTHER, "{", ", ", "}", 0, fields));
    });
    pat("pat.binary", 1, true, (s, b, d) -> {
      List<PbtNode> segs = new ArrayList<>();
      for (int i = 0, n = r.nextInt(3); i < n; i++) {
        if (r.nextBoolean()) {
          String v = fresh();
          b.add(v);
          segs.add(leaf("bin_pat_seg", Cat.OTHER, v + pick(":8", ":16", "/integer", ":4/little")));
        }
        else {
          segs.add(leaf("bin_pat_seg", Cat.OTHER, pick("1", "$a", "\"ab\"", "_:8")));
        }
      }
      if (r.nextBoolean()) {
        String v = fresh();
        b.add(v);
        segs.add(leaf("bin_pat_seg", Cat.OTHER, v + pick("/binary", "/bits")));
      }
      return seq("pat.binary", Cat.PAT, "<<", ", ", ">>", 0, segs);
    });
    pat("pat.alias", 1, true, (s, b, d) -> node("pat.alias", Cat.PAT, pat(s, b, d - 1), " = ", pat(s, b, d - 1)));

    // ----------------------------------------------------------------------------------------------------- guards
    GUARDS.add(new Choice<>("guard.type_test", 5, false, (s, d) -> node("guard.type_test", Cat.GUARD,
      pick("is_integer(", "is_atom(", "is_list(", "is_tuple(", "is_map(", "is_binary(", "is_function(", "is_number("), guardOperand(s), ")")));
    GUARDS.add(new Choice<>("guard.compare", 4, false, (s, d) -> node("guard.compare", Cat.GUARD,
      guardOperand(s), pick(" > ", " =:= ", " =/= ", " =< ", " == "), pick("0", "ok", "[]", "1.5", "\"s\""))));
    GUARDS.add(new Choice<>("guard.bif_compare", 2, false, (s, d) -> node("guard.bif_compare", Cat.GUARD,
      pick("length(", "tuple_size(", "map_size(", "byte_size(", "abs("), guardOperand(s), ")", pick(" > 0", " =:= 2", " >= 1"))));
    GUARDS.add(new Choice<>("guard.true", 1, false, (s, d) -> leaf("guard.true", Cat.GUARD, "true")));
    GUARDS.add(new Choice<>("guard.bool", 1, true, (s, d) -> r.nextInt(3) == 0
      ? node("guard.bool", Cat.GUARD, "not (", guardTest(s, d - 1), ")")
      : node("guard.bool", Cat.GUARD, "(", guardTest(s, d - 1), pick(" andalso ", " orelse "), guardTest(s, d - 1), ")")));
    GUARDS.add(new Choice<>("guard.is_record", 1, false, (s, d) -> myRecs.isEmpty() ? null
      : node("guard.is_record", Cat.GUARD, "is_record(", guardOperand(s), ", " + pick(myRecs).name + ")")));
    GUARDS.add(new Choice<>("guard.record_field", 1, false, (s, d) -> {
      if (myRecs.isEmpty() || s.vars.isEmpty()) return null;
      Rec rec = pick(myRecs);
      return leaf("guard.record_field", Cat.GUARD, pick(s.vars) + "#" + rec.name + "." + pick(rec.fields) + " =:= 1");
    }));
    GUARDS.add(new Choice<>("guard.map_key", 1, false, (s, d) -> node("guard.map_key", Cat.GUARD,
      pick("is_map_key(a, ", "map_get(a, "), guardOperand(s), pick(")", ") =:= 1"))));

    // ------------------------------------------------------------------------------------------------------ types
    type("type.builtin", 6, false, (vars, d) -> leaf("type.builtin", Cat.TYPE,
      pick("integer()", "atom()", "binary()", "boolean()", "term()", "pid()", "non_neg_integer()", "float()", "list()", "map()", "tuple()", "string()")));
    type("type.literal", 2, false, (vars, d) -> leaf("type.literal", Cat.TYPE, pick("ok", "42", "'Quoted'", "[]", "-1")));
    type("type.range", 1, false, (vars, d) -> leaf("type.range", Cat.TYPE, pick("1..10", "0..255", "-1..1")));
    type("type.var", 3, false, (vars, d) -> vars.isEmpty() ? null : leaf("type.var", Cat.TYPE, pick(vars)));
    type("type.user", 3, true, (vars, d) -> {
      if (myTypes.isEmpty()) return null;
      Ty t = pick(myTypes);
      List<PbtNode> args = new ArrayList<>();
      for (int i = 0; i < t.arity; i++) args.add(type(vars, d - 1));
      return node("type.user", Cat.TYPE, t.name, seq("type_args", Cat.OTHER, "(", ", ", ")", t.arity, args));
    });
    type("type.list", 2, true, (vars, d) -> node("type.list", Cat.TYPE, "[", type(vars, d - 1), pick("]", ", ...]")));
    type("type.tuple", 2, true, (vars, d) -> {
      List<PbtNode> items = new ArrayList<>();
      for (int i = 0, n = r.nextInt(3); i < n; i++) items.add(type(vars, d - 1));
      return seq("type.tuple", Cat.TYPE, "{", ", ", "}", 0, items);
    });
    type("type.union", 3, true, (vars, d) -> {
      List<PbtNode> items = new ArrayList<>();
      for (int i = 0, n = 2 + r.nextInt(2); i < n; i++) items.add(type(vars, d - 1));
      return seq("type.union", Cat.TYPE, "", " | ", "", 1, items);
    });
    type("type.map", 1, true, (vars, d) -> node("type.map", Cat.TYPE, "#{", pick("atom() => ", "a := ", "integer() => "), type(vars, d - 1), "}"));
    type("type.fun", 1, true, (vars, d) -> {
      int n = r.nextInt(3);
      List<PbtNode> args = new ArrayList<>();
      for (int i = 0; i < n; i++) args.add(type(vars, d - 1));
      return node("type.fun", Cat.TYPE, "fun(", seq("fun_type_args", Cat.OTHER, "(", ", ", ")", 0, args), " -> ", type(vars, d - 1), ")");
    });
    type("type.record", 1, false, (vars, d) -> myRecs.isEmpty() ? null : leaf("type.record", Cat.TYPE, "#" + pick(myRecs).name + "{}"));
    type("type.binary", 1, false, (vars, d) -> leaf("type.binary", Cat.TYPE, pick("<<>>", "<<_:8>>", "<<_:_*8>>", "<<_:4, _:_*2>>")));
  }

  private void expr(String name, int weight, boolean nests, BiFunction<Scope, Integer, PbtNode> gen) {
    EXPRS.add(new Choice<>(name, weight, nests, gen));
  }

  private void pat(String name, int weight, boolean nests, PatGen gen) {
    PATS.add(new Choice<>(name, weight, nests, gen));
  }

  private void type(String name, int weight, boolean nests, BiFunction<List<String>, Integer, PbtNode> gen) {
    TYPES.add(new Choice<>(name, weight, nests, gen));
  }

  // =================================================================================================== module

  PbtNode module() {
    for (int i = 0, n = r.nextInt(3); i < n; i++) {
      List<String> fields = new ArrayList<>(List.of("a", "b", "c").subList(0, 1 + r.nextInt(3)));
      myRecs.add(new Rec("r" + (i + 1), fields));
    }
    for (int i = 0, n = r.nextInt(4); i < n; i++) myTypes.add(new Ty("t" + (i + 1), r.nextInt(4) == 0 ? 1 : 0));
    for (int i = 0, n = 1 + r.nextInt(5); i < n; i++) myFns.add(new Fn("f" + (i + 1), r.nextInt(4)));

    List<PbtNode> exports = new ArrayList<>();
    for (Fn f : myFns) if (r.nextInt(5) < 3) exports.add(leaf("export_entry", Cat.OTHER, f.name + "/" + f.arity));
    if (exports.isEmpty()) exports.add(leaf("export_entry", Cat.OTHER, myFns.get(0).name + "/" + myFns.get(0).arity));
    List<PbtNode> forms = new ArrayList<>();
    forms.add(node("form.export", Cat.FORM, "-export(", seq("exports", Cat.OTHER, "[", ", ", "]", 0, exports), ")."));

    List<PbtNode> typeExports = new ArrayList<>();
    for (Ty t : myTypes) if (r.nextInt(4) == 0) typeExports.add(leaf("export_type_entry", Cat.OTHER, t.name + "/" + t.arity));
    if (!typeExports.isEmpty()) {
      forms.add(node("form.export_type", Cat.FORM, "-export_type(", seq("exports", Cat.OTHER, "[", ", ", "]", 0, typeExports), ")."));
    }
    for (Rec rec : myRecs) forms.add(recordDef(rec));
    for (Ty t : myTypes) forms.add(typeDef(t));
    for (Fn f : myFns) {
      if (r.nextInt(3) == 0) forms.add(spec(f));
      forms.add(function(f));
    }
    return node("module", Cat.OTHER, "-module(" + MODULE + ").\n", seq("forms", Cat.OTHER, "", "\n", "\n", 0, forms));
  }

  private PbtNode recordDef(Rec rec) {
    List<PbtNode> fields = new ArrayList<>();
    for (String f : rec.fields) {
      fields.add(switch (r.nextInt(4)) {
        case 0 -> node("record_field_def", Cat.OTHER, f + " = ", leaf("expr.int", Cat.EXPR, pick("0", "1", "ok", "[]", "\"s\"")));
        case 1 -> node("record_field_def", Cat.OTHER, f + " :: ", type(List.of(), 1));
        case 2 -> node("record_field_def", Cat.OTHER, f + " = 0 :: ", type(List.of(), 1));
        default -> leaf("record_field_def", Cat.OTHER, f);
      });
    }
    return node("form.record", Cat.FORM, "-record(" + rec.name + ", ", seq("record_fields", Cat.OTHER, "{", ", ", "}", fields.size(), fields), ").");
  }

  private PbtNode typeDef(Ty t) {
    String kind = r.nextInt(5) == 0 ? "opaque" : "type";
    List<String> vars = t.arity == 1 ? List.of("A") : List.of();
    PbtNode body = type(vars, 2);
    // a parameter must occur in the body, or erlc rejects the definition
    if (t.arity == 1) body = node("type.union", Cat.TYPE, "A | ", body);
    return node("form." + kind, Cat.FORM, "-" + kind + " " + t.name + "(" + String.join(", ", vars) + ") :: ", body, ".");
  }

  private PbtNode spec(Fn f) {
    List<PbtNode> args = new ArrayList<>();
    for (int i = 0; i < f.arity; i++) args.add(type(List.of(), 1));
    return node("form.spec", Cat.FORM, "-spec " + f.name, seq("spec_args", Cat.OTHER, "(", ", ", ")", f.arity, args), " -> ",
                type(List.of(), 1), ".");
  }

  private PbtNode function(Fn f) {
    List<PbtNode> clauses = new ArrayList<>();
    for (int i = 0, n = 1 + r.nextInt(3); i < n; i++) {
      List<String> binds = new ArrayList<>();
      Scope empty = new Scope(List.of());
      List<PbtNode> args = new ArrayList<>();
      for (int a = 0; a < f.arity; a++) args.add(pat(empty, binds, 2));
      Scope s = empty.with(binds);
      clauses.add(node("function_clause", Cat.CLAUSE, f.name, seq("args", Cat.OTHER, "(", ", ", ")", f.arity, args),
                       optionalGuard(s, 2), " ->\n    ", body(s, 3, ",\n    ")));
    }
    return seq("form.function", Cat.FORM, "", ";\n", ".", 1, clauses);
  }

  // =================================================================================================== bodies

  /** One to three statements; a statement {@code Pattern = Expr} binds its new variables for the next ones. */
  private PbtNode body(Scope s, int depth, String sep) {
    List<PbtNode> stmts = new ArrayList<>();
    Scope cur = s;
    for (int i = 0, n = 1 + r.nextInt(3); i < n; i++) {
      if (r.nextInt(3) == 0) {
        PbtNode value = expr(cur, depth);
        List<String> binds = new ArrayList<>();
        PbtNode p = pat(cur, binds, Math.min(depth, 2));
        stmts.add(node("stmt.match", Cat.OTHER, p, " = ", value));
        cur = cur.with(binds);
      }
      else {
        stmts.add(expr(cur, depth));
      }
    }
    return seq("body", Cat.OTHER, "", sep, "", 1, stmts);
  }

  private PbtNode crClause(String construct, Scope s, int depth) {
    List<String> binds = new ArrayList<>();
    PbtNode p = pat(s, binds, Math.min(depth, 2));
    Scope in = s.with(binds);
    return node(construct, Cat.CLAUSE, p, optionalGuard(in, depth), " -> ", body(in, depth, ", "));
  }

  private PbtNode catchClause(Scope s, int depth) {
    List<String> binds = new ArrayList<>();
    String cls = pick("error", "throw", "exit", "_");
    PbtNode reason = pat(s, binds, 1);
    String stack = "";
    if (r.nextInt(3) == 0) {
      stack = fresh();
      binds.add(stack);
      stack = ":" + stack;
    }
    Scope in = s.with(binds);
    PbtNode head = r.nextInt(4) == 0 ? node("catch_head", Cat.OTHER, reason) : node("catch_head", Cat.OTHER, cls + ":", reason, stack);
    return node("catch_clause", Cat.CLAUSE, head, optionalGuard(in, depth), " -> ", body(in, depth, ", "));
  }

  private PbtNode funClause(Scope s, int depth, int arity, @Nullable String name) {
    List<String> binds = new ArrayList<>();
    List<PbtNode> args = new ArrayList<>();
    for (int a = 0; a < arity; a++) args.add(pat(s, binds, Math.min(depth, 2)));
    Scope in = s.with(binds);
    if (name != null) in = in.with(List.of(name));
    return node("fun_clause", Cat.CLAUSE, name == null ? "" : name, seq("args", Cat.OTHER, "(", ", ", ")", arity, args),
                optionalGuard(in, depth), " -> ", body(in, depth, ", "));
  }

  /** Qualifiers of a comprehension; the variables of their patterns go to {@code binds}. */
  private List<PbtNode> qualifiers(Scope s, int depth, List<String> binds) {
    List<PbtNode> qs = new ArrayList<>();
    for (int i = 0, n = 1 + r.nextInt(2); i < n; i++) {
      Scope cur = s.with(binds);
      int kind = i == 0 ? r.nextInt(3) : r.nextInt(4);
      qs.add(switch (kind) {
        case 0 -> node("lc.generator", Cat.OTHER, pat(cur, binds, Math.min(depth, 1)), " <- ", expr(cur, depth));
        case 1 -> node("lc.bin_generator", Cat.OTHER, binPat(binds), " <= ", expr(cur, depth));
        case 2 -> {
          PbtNode k = pat(cur, binds, 0);
          PbtNode v = pat(cur, binds, 0);
          yield node("lc.map_generator", Cat.OTHER, k, " := ", v, " <- ", expr(cur, depth));
        }
        default -> node("lc.filter", Cat.OTHER, expr(cur, depth));
      });
    }
    return qs;
  }

  private PbtNode binPat(List<String> binds) {
    String v = fresh();
    binds.add(v);
    return leaf("bin_gen_pat", Cat.OTHER, "<<" + v + pick(":8", ":16", "/utf8") + ">>");
  }

  private PbtNode optionalGuard(Scope s, int depth) {
    List<PbtNode> g = new ArrayList<>();
    if (r.nextInt(3) == 0) g.add(guard(s, depth, true));
    return PbtNode.optional("guard", " when ", "", "", g);
  }

  /** A guard sequence: guards separated by {@code ;}, each of tests separated by {@code ,}. */
  private PbtNode guard(Scope s, int depth, boolean shrinkable) {
    List<PbtNode> alts = new ArrayList<>();
    for (int i = 0, n = r.nextInt(4) == 0 ? 2 : 1; i < n; i++) {
      List<PbtNode> tests = new ArrayList<>();
      for (int j = 0, m = 1 + (r.nextInt(3) == 0 ? 1 : 0); j < m; j++) tests.add(guardTest(s, depth));
      alts.add(seq("guard_tests", Cat.OTHER, "", ", ", "", 1, tests));
    }
    return seq(shrinkable ? "guard_seq" : "if_guard", Cat.OTHER, "", "; ", "", 1, alts);
  }

  private PbtNode guardTest(Scope s, int depth) {
    return choose(GUARDS, depth, c -> c.gen.apply(s, depth));
  }

  private String guardOperand(Scope s) {
    if (r.nextInt(25) == 0) {
      count("guard.unbound_var");
      return "Unbound" + ++myUnbound;
    }
    return s.vars.isEmpty() || r.nextInt(5) == 0 ? pick("0", "ok", "[]", "{}") : pick(s.vars);
  }

  // =================================================================================================== helpers

  PbtNode expr(Scope s, int depth) {
    return choose(EXPRS, depth, c -> c.gen.apply(s, depth));
  }

  private List<PbtNode> exprs(Scope s, int depth, int min, int max) {
    List<PbtNode> out = new ArrayList<>();
    for (int i = 0, n = min + r.nextInt(max - min + 1); i < n; i++) out.add(expr(s, depth));
    return out;
  }

  private PbtNode pat(Scope s, List<String> binds, int depth) {
    return choose(PATS, depth, c -> c.gen.gen(s, binds, depth));
  }

  private List<PbtNode> pats(Scope s, List<String> binds, int depth, int min, int max) {
    List<PbtNode> out = new ArrayList<>();
    for (int i = 0, n = min + r.nextInt(max - min + 1); i < n; i++) out.add(pat(s, binds, depth));
    return out;
  }

  private PbtNode type(List<String> vars, int depth) {
    return choose(TYPES, depth, c -> c.gen.apply(vars, depth));
  }

  /** An expression that can stand before {@code #}: a variable or a parenthesized expression. */
  private PbtNode primary(Scope s, int depth) {
    if (!s.vars.isEmpty() && r.nextInt(3) != 0) return leaf("expr.var", Cat.EXPR, pick(s.vars));
    return node("expr.paren", Cat.OTHER, "(", expr(s, depth), ")");
  }

  /** The value of a binary segment: a variable, a literal or a parenthesized expression. */
  private PbtNode segValue(Scope s, int depth) {
    return switch (r.nextInt(3)) {
      case 0 -> s.vars.isEmpty() ? leaf("expr.int", Cat.EXPR, "1") : leaf("expr.var", Cat.EXPR, pick(s.vars));
      case 1 -> leaf("expr.int", Cat.EXPR, pick("1", "255", "$a"));
      default -> node("expr.paren", Cat.OTHER, "(", expr(s, depth), ")");
    };
  }

  private PbtNode mapKey(Scope s, int depth) {
    return r.nextInt(3) == 0 ? expr(s, depth) : leaf("map_key", Cat.OTHER, pick("a", "b", "1", "\"k\"", "{x, 1}"));
  }

  private PbtNode recordFields(Rec rec, Scope s, int depth) {
    List<PbtNode> fields = new ArrayList<>();
    for (String f : rec.fields) if (r.nextBoolean()) fields.add(node("record_field", Cat.OTHER, f + " = ", expr(s, depth)));
    return seq("record_fields", Cat.OTHER, "{", ", ", "}", 0, fields);
  }

  private PbtNode binop(String construct, Scope s, int depth, String... ops) {
    return node(construct, Cat.EXPR, "(", expr(s, depth - 1), " " + pick(ops) + " ", expr(s, depth - 1), ")");
  }

  private String fresh() {
    return pick("X", "Y", "Acc", "Val", "N", "L", "Key", "T", "B", "S") + ++myVars;
  }

  private <G> PbtNode choose(List<Choice<G>> choices, int depth, java.util.function.Function<Choice<G>, @Nullable PbtNode> run) {
    for (int attempt = 0; attempt < 50; attempt++) {
      int total = 0;
      for (Choice<G> c : choices) if (depth > 0 || !c.nests) total += c.weight;
      int x = r.nextInt(total);
      for (Choice<G> c : choices) {
        if (depth <= 0 && c.nests) continue;
        x -= c.weight;
        if (x < 0) {
          PbtNode n = run.apply(c);
          if (n != null) return n;
          break;
        }
      }
    }
    throw new IllegalStateException("no construct applies");
  }

  private PbtNode node(String construct, Cat cat, Object... parts) {
    count(construct);
    return PbtNode.of(construct, cat, parts);
  }

  private PbtNode leaf(String construct, Cat cat, String text) {
    count(construct);
    return PbtNode.leaf(construct, cat, text);
  }

  private PbtNode seq(String construct, Cat cat, String open, String sep, String close, int min, List<PbtNode> items) {
    count(construct);
    return PbtNode.seq(construct, cat, open, sep, close, min, items);
  }

  private void count(String construct) {
    if (construct.contains(".")) myUsage.merge(construct, 1, Integer::sum);
  }

  @SafeVarargs
  private <T> T pick(T... options) {
    return options[r.nextInt(options.length)];
  }

  private <T> T pick(List<T> options) {
    return options.get(r.nextInt(options.size()));
  }

  /** The names of every construct the generator knows, for the usage counts of the report. */
  List<String> constructs() {
    List<String> names = new ArrayList<>();
    for (Choice<?> c : EXPRS) names.add(c.name);
    for (Choice<?> c : PATS) names.add(c.name);
    for (Choice<?> c : GUARDS) names.add(c.name);
    for (Choice<?> c : TYPES) names.add(c.name);
    return names;
  }

  static Map<String, Integer> newUsage() {
    return new TreeMap<>();
  }
}
