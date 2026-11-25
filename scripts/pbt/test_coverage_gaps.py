"""Unit tests of coverage_gaps.py: python3 -m unittest discover -s scripts/pbt -p 'test_*.py' -v"""

import contextlib
import copy
import io
import json
import os
import tempfile
import textwrap
import unittest

import coverage_gaps

# A cut of grammars/erlang.bnf. The rule in the comment and the names in the strings and attributes are not rules.
BNF = textwrap.dedent("""\
    // A cut of the grammar for the tests: comment_rule ::= forms
    {
      parserClass="org.intellij.erlang.parser.ErlangParser"
      tokens = [
        OP_PLUS = "+"
        OP_MINUS = "-"
        PAR_LEFT = "("
        PAR_RIGHT = ")"
      ]
    }

    forms ::= item *
    private item ::= call | add_op | dead
    add_op ::= '+' | '-'
    call ::= '(' args? ')' {pin=1 extends=orphan}
    private args ::= atom
    atom ::= atom_name
    /* hidden ::= a block comment */
    dead ::= ghost
    ghost ::= '-'
    fake marker ::= atom
    orphan ::= '+'
    """)

RAN = None  # a line without code


def never(name, call):
    """A rule method that never ran."""
    return [
        (RAN, "  /* ********************************************************** */"),
        (RAN, "  // %s" % name),
        (RAN, "  public static boolean %s(PsiBuilder b, int l) {" % name),
        ((4, 0, 2, 0), '    if (!recursion_guard_(b, l, "%s")) return false;' % name),
        (RAN, "    boolean r;"),
        ((3, 0, 0, 0), "    r = %s;" % call),
        ((2, 0, 0, 0), "    return r;"),
        (RAN, "  }"),
        (RAN, ""),
    ]


# The generated parser of BNF, each line with its JaCoCo counts (missed and covered instructions, missed and covered
# branches). forms and item ran; add_op ran, but '+' never matched; call ran but never got past '('; atom, dead,
# ghost and orphan never ran; args has no method (inlined), marker is fake.
JAVA = [
    (RAN, "package org.intellij.erlang.parser;"),
    (RAN, ""),
    (RAN, "public class ErlangParser implements PsiParser, LightPsiParser {"),
    (RAN, ""),
    (RAN, "  /* ********************************************************** */"),
    (RAN, "  // item *"),
    (RAN, "  static boolean forms(PsiBuilder b, int l) {"),
    ((0, 4, 1, 1), '    if (!recursion_guard_(b, l, "forms")) return false;'),
    (RAN, "    while (true) {"),
    ((0, 3, 0, 0), "      int c = current_position_(b);"),
    ((0, 4, 0, 2), "      if (!item(b, l + 1)) break;"),
    ((0, 5, 1, 1), '      if (!empty_element_parsed_guard_(b, "forms", c)) break;'),
    (RAN, "    }"),
    ((0, 2, 0, 0), "    return true;"),
    (RAN, "  }"),
    (RAN, ""),
    (RAN, "  /* ********************************************************** */"),
    (RAN, "  // call | add_op | dead"),
    (RAN, "  static boolean item(PsiBuilder b, int l) {"),
    ((0, 4, 1, 1), '    if (!recursion_guard_(b, l, "item")) return false;'),
    (RAN, "    boolean r;"),
    ((0, 3, 0, 0), "    r = call(b, l + 1);"),
    ((0, 5, 0, 2), "    if (!r) r = add_op(b, l + 1);"),
    ((3, 2, 1, 1), "    if (!r) r = dead(b, l + 1);"),
    ((0, 2, 0, 0), "    return r;"),
    (RAN, "  }"),
    (RAN, ""),
    (RAN, "  /* ********************************************************** */"),
    (RAN, "  // '+' | '-'"),
    (RAN, "  public static boolean add_op(PsiBuilder b, int l) {"),
    ((0, 4, 1, 1), '    if (!recursion_guard_(b, l, "add_op")) return false;'),
    (RAN, "    boolean r;"),
    ((0, 3, 0, 0), "    r = consumeToken(b, ERL_OP_PLUS);"),
    ((0, 5, 1, 1), "    if (!r) r = consumeToken(b, ERL_OP_MINUS);"),
    ((0, 2, 0, 0), "    return r;"),
    (RAN, "  }"),
    (RAN, ""),
    (RAN, "  /* ********************************************************** */"),
    (RAN, "  // '(' args? ')'"),
    (RAN, "  public static boolean call(PsiBuilder b, int l) {"),
    ((0, 4, 1, 1), '    if (!recursion_guard_(b, l, "call")) return false;'),
    ((0, 4, 1, 1), "    if (!nextTokenIs(b, ERL_PAR_LEFT)) return false;"),
    (RAN, "    boolean r, p;"),
    ((0, 6, 0, 0), "    Marker m = enter_section_(b, l, _NONE_, ERL_CALL, null);"),
    ((0, 3, 0, 0), "    r = consumeToken(b, ERL_PAR_LEFT);"),
    ((0, 2, 0, 0), "    p = r; // pin = 1"),
    ((7, 2, 1, 1), "    r = r && report_error_(b, call_1(b, l + 1));"),
    ((7, 2, 5, 1), "    r = p && consumeToken(b, ERL_PAR_RIGHT) && r;"),
    ((0, 7, 0, 0), "    exit_section_(b, l, m, r, p, null);"),
    ((0, 4, 1, 1), "    return r || p;"),
    (RAN, "  }"),
    (RAN, ""),
    (RAN, "  // args?"),
    (RAN, "  private static boolean call_1(PsiBuilder b, int l) {"),
    ((4, 0, 2, 0), '    if (!recursion_guard_(b, l, "call_1")) return false;'),
    ((3, 0, 0, 0), "    atom(b, l + 1);"),
    ((2, 0, 0, 0), "    return true;"),
    (RAN, "  }"),
    (RAN, ""),
] + never("atom", "consumeToken(b, ERL_ATOM_NAME)") + never("dead", "ghost(b, l + 1)") \
  + never("ghost", "consumeToken(b, ERL_OP_MINUS)") + never("orphan", "consumeToken(b, ERL_OP_PLUS)") + [(RAN, "}")]

EXTRA = "org/intellij/erlang/inspection/ErlangUnusedVariableInspection"


def report_xml(java, method_line_shift=0, extra_size=0, line_shift=0):
    """A JaCoCo XML report of the parser lines of `java` and of one inspection class."""
    methods = []
    lines = []
    tot = [0, 0, 0, 0]
    for no, (cov, text) in enumerate(java, 1):
        if text.startswith("  ") and "static boolean " in text and text.endswith("{"):
            name = text.split("static boolean ", 1)[1].split("(", 1)[0]
            methods.append('<method name="%s" desc="(Lcom/intellij/lang/PsiBuilder;I)Z" line="%d"/>'
                           % (name, no + 1 + method_line_shift))
        if cov is not None:
            lines.append('<line nr="%d" mi="%d" ci="%d" mb="%d" cb="%d"/>' % ((no + line_shift,) + cov))
            tot = [a + b for a, b in zip(tot, cov)]
    mi, ci, mb, cb = tot
    parser_counters = ('<counter type="INSTRUCTION" missed="%d" covered="%d"/>'
                       '<counter type="BRANCH" missed="%d" covered="%d"/>' % (mi, ci, mb, cb))
    return textwrap.dedent("""\
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <report name="intellij-erlang">
          <sessioninfo id="s1" start="1" dump="2"/>
          <package name="org/intellij/erlang/parser">
            <class name="org/intellij/erlang/parser/ErlangParser" sourcefilename="ErlangParser.java">
              {methods}
              {parser_counters}
            </class>
            <sourcefile name="ErlangParser.java">
              {lines}
            </sourcefile>
          </package>
          <package name="org/intellij/erlang/inspection">
            <class name="{extra}" sourcefilename="ErlangUnusedVariableInspection.java">
              <method name="buildVisitor" desc="()V" line="10">
                <counter type="INSTRUCTION" missed="0" covered="8"/>
                <counter type="LINE" missed="0" covered="3"/>
              </method>
              <method name="getDisplayName" desc="()V" line="20">
                <counter type="INSTRUCTION" missed="5" covered="0"/>
                <counter type="LINE" missed="2" covered="0"/>
              </method>
              <counter type="LINE" missed="2" covered="3"/>
            </class>
            <class name="{extra}$1" sourcefilename="ErlangUnusedVariableInspection.java">
              <method name="visitQVar" desc="()V" line="30">
                <counter type="INSTRUCTION" missed="1" covered="4"/>
                <counter type="BRANCH" missed="1" covered="1"/>
              </method>
            </class>
          </package>
          <counter type="INSTRUCTION" missed="{mi}" covered="{ci}"/>
          <counter type="BRANCH" missed="{mb}" covered="{cb}"/>
        </report>
        """).format(methods="".join(methods), parser_counters=parser_counters, lines="".join(lines), extra=EXTRA,
                    mi=mi + 6 + extra_size, ci=ci + 12, mb=mb + 1, cb=cb + 1)


class CoverageGapsTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.repo = self.dir.name
        os.makedirs(os.path.join(self.repo, "grammars"))
        self.parser_dir = os.path.join(self.repo, "gen", "org", "intellij", "erlang", "parser")
        os.makedirs(self.parser_dir)
        self.bnf = self.write("grammars/erlang.bnf", BNF)
        self.write("gen/org/intellij/erlang/parser/ErlangParser.java", "\n".join(t for _, t in JAVA) + "\n")

    def write(self, path, text):
        full = os.path.join(self.repo, path)
        with open(full, "w", encoding="utf-8") as f:
            f.write(text)
        return full

    def run_tool(self, *args, report=None):
        report = report or self.write("report.xml", report_xml(JAVA))
        out = io.StringIO()
        with contextlib.redirect_stdout(out):
            self.assertEqual(coverage_gaps.main([report, "--bnf", self.bnf] + list(args)), 0)
        return out.getvalue()

    def result(self, *args, report=None):
        target = os.path.join(self.repo, "out.json")
        text = self.run_tool("--json", target, *args, report=report)
        with open(target, encoding="utf-8") as f:
            return json.load(f), text

    def test_bnf_rules_refs_modifiers_and_tokens(self):
        rules, tokens, _ = coverage_gaps.parse_bnf(self.bnf)
        by_name = {r["name"]: r for r in rules}
        self.assertEqual([r["name"] for r in rules],
                         ["forms", "item", "add_op", "call", "args", "atom", "dead", "ghost", "marker", "orphan"])
        self.assertEqual(by_name["item"]["modifiers"], ["private"])
        self.assertEqual(by_name["marker"]["modifiers"], ["fake"])
        self.assertEqual(by_name["item"]["refs"], ["call", "add_op", "dead"])
        # names in strings and in the attribute block are not references
        self.assertEqual(by_name["call"]["refs"], ["args"])
        self.assertEqual(by_name["forms"]["line"], BNF.split("\n").index("forms ::= item *") + 1)
        self.assertEqual(tokens["ERL_OP_PLUS"], "+")
        self.assertEqual(tokens["ERL_PAR_RIGHT"], ")")

    def test_rule_of_method(self):
        names = {"type", "type_guard", "add_op", "a", "a_b"}
        self.assertEqual(coverage_gaps.rule_of_method("type_1_1_0", names), "type")
        self.assertEqual(coverage_gaps.rule_of_method("add_op_$", names), "add_op")
        self.assertEqual(coverage_gaps.rule_of_method("a_b_1", names), "a_b")
        self.assertEqual(coverage_gaps.rule_of_method("type_guard_2", names), "type_guard")
        self.assertIsNone(coverage_gaps.rule_of_method("type_x", names))

    def test_call_executed_limits(self):
        self.assertTrue(coverage_gaps.call_executed("seq", (4, 3, 0, 0)))
        self.assertFalse(coverage_gaps.call_executed("seq", (5, 3, 0, 0)))
        self.assertTrue(coverage_gaps.call_executed("alt", (0, 3, 0, 0)))
        self.assertFalse(coverage_gaps.call_executed("alt", (1, 3, 0, 0)))
        self.assertFalse(coverage_gaps.call_executed("plain", (3, 0, 0, 0)))
        self.assertTrue(coverage_gaps.call_executed("plain", (3, 1, 0, 0)))

    def test_verdicts_of_the_rules(self):
        res, _ = self.result()
        rules = {r["name"]: r for r in res["parser"]["rules"]}
        self.assertEqual({n: r["verdict"] for n, r in rules.items()}, {
            "forms": "reached", "item": "partly", "add_op": "partly", "call": "not-reached", "args": "no-code",
            "atom": "not-reached", "dead": "not-reached", "ghost": "not-reached", "marker": "no-code",
            "orphan": "not-reached"})
        self.assertEqual(rules["call"]["why"], "called, never matched its first element")
        self.assertEqual(rules["atom"]["why"], "never called")
        self.assertEqual(rules["marker"]["why"], "fake rule: only a PSI type, never parsed")
        self.assertEqual([(g["kind"], g["what"]) for g in rules["item"]["gaps"]], [("never-tried", "dead")])
        self.assertEqual([(g["kind"], g["what"]) for g in rules["add_op"]["gaps"]], [("never-matched", "'+'")])
        self.assertEqual([(g["kind"], g["method"], g["what"]) for g in rules["call"]["gaps"]],
                         [("never-tried", "call", "')'"), ("never-called", "call_1", "args?")])
        self.assertFalse(rules["orphan"]["in_erl"])
        self.assertTrue(rules["ghost"]["in_erl"])
        self.assertEqual(res["meta"]["warnings"], [])

    def test_totals(self):
        res, _ = self.result()
        t = res["parser"]["totals"]
        self.assertEqual((t["rules"], t["reached"], t["partly"], t["not_reached"], t["no_code"]), (10, 1, 2, 5, 2))
        self.assertEqual(t["erl_rules"], {"rules": 8, "reached": 1, "partly": 2, "not_reached": 4, "no_code": 1})
        branches = [c for c, _ in JAVA if c]
        covered = sum(c[3] for c in branches)
        self.assertEqual(t["parser_branches"]["covered"], covered)
        self.assertEqual(t["parser_branches"]["missed"], sum(c[2] for c in branches))
        guard_covered = sum(c[3] for c, text in JAVA if c and "recursion_guard_(" in text)
        self.assertEqual(t["parser_branches_no_guard"]["covered"], covered - guard_covered)

    def test_order_of_the_top_gaps(self):
        res, text = self.result()
        gaps = res["top_gaps"]
        self.assertEqual([g["name"] for g in gaps], ["call", "dead", "atom", "ghost", "orphan"])
        by_name = {g["name"]: g for g in gaps}
        self.assertEqual(by_name["call"]["blocked"], ["call", "atom"])
        self.assertEqual(by_name["atom"]["inside"], "call")
        self.assertEqual(by_name["ghost"]["inside"], "dead")
        self.assertFalse(by_name["orphan"]["from_root"])
        self.assertIn("[not used from forms]", text)
        self.assertIn("inside call", text)

    def test_other_contexts_are_not_in_erl_files(self):
        res, text = self.result("--other-contexts", "dead")
        rules = {r["name"]: r for r in res["parser"]["rules"]}
        self.assertFalse(rules["dead"]["in_erl"])
        self.assertFalse(rules["ghost"]["in_erl"])
        self.assertEqual([g["name"] for g in res["top_gaps"]][-3:], ["dead", "orphan", "ghost"])
        self.assertIn("[not in .erl files]", text)

    def test_extra_classes_and_missing_prefixes(self):
        res, text = self.result()
        extras = {c["class"]: c for c in res["extras"]}
        self.assertEqual(sorted(extras), [EXTRA, EXTRA + "$1"])
        zero = [m["name"] for m in extras[EXTRA]["methods"] if m["zero"]]
        self.assertEqual(zero, ["getDisplayName"])
        self.assertIn("ErlangUnusedFunctionInspection", res["meta"]["extras_missing"])
        self.assertNotIn("ErlangUnusedVariableInspection", res["meta"]["extras_missing"])
        self.assertIn("methods with zero coverage: getDisplayName", text)
        res, _ = self.result("--extra", "org/intellij/erlang/inspection/")
        self.assertEqual(res["meta"]["extras_missing"], [])

    def test_report_of_another_commit_warns(self):
        report = self.write("shifted.xml", report_xml(JAVA, method_line_shift=500))
        res, text = self.result(report=report)
        self.assertEqual(len(res["meta"]["warnings"]), 1)
        self.assertIn("different commits", res["meta"]["warnings"][0])
        self.assertTrue(text.startswith("WARNING: "))

    def test_report_longer_than_the_source_fails_with_a_message(self):
        report = self.write("longer.xml", report_xml(JAVA, method_line_shift=500, line_shift=500))
        with self.assertRaises(SystemExit) as e:
            self.run_tool(report=report)
        self.assertIn("different commits", str(e.exception.code))
        self.assertIn("--parser-java", str(e.exception.code))

    def test_recover_while_is_a_reference_of_the_rule(self):
        bnf = self.write("recover.bnf", "forms ::= form *\nform ::= '+' {pin=1 recoverWhile=form_recover}\n"
                                        "private form_recover ::= !'+'\nextra ::= '-' {extends=form}\n")
        rules, _, _ = coverage_gaps.parse_bnf(bnf)
        refs = {r["name"]: r["refs"] for r in rules}
        self.assertEqual(refs["form"], ["form_recover"])
        self.assertEqual(refs["extra"], [])

    def test_executed_token_alternatives_are_not_a_match(self):
        # the caller's next line saw r false only: no alternative of add_op ever matched
        java = [(cov if "r = dead(b, l + 1)" not in text else (0, 5, 1, 1), text) for cov, text in JAVA]
        report = self.write("fail.xml", report_xml(java))
        res, _ = self.result(report=report)
        rules = {r["name"]: r for r in res["parser"]["rules"]}
        self.assertEqual(rules["add_op"]["verdict"], "not-reached")
        self.assertEqual(rules["add_op"]["why"], "called, never matched its first element")
        # and with r true after the first alternative the fixture's verdict stays
        res, _ = self.result()
        self.assertEqual({r["name"]: r for r in res["parser"]["rules"]}["add_op"]["verdict"], "partly")

    def test_report_without_the_parser_fails(self):
        report = self.write("empty.xml", '<?xml version="1.0"?><report name="x"></report>')
        with self.assertRaises(SystemExit) as e:
            self.run_tool(report=report)
        self.assertIn("ErlangParser", str(e.exception.code))

    def test_diff_reports_changes_and_size_warnings(self):
        old, _ = self.result()
        new = copy.deepcopy(old)
        for r in new["parser"]["rules"]:
            if r["name"] == "atom":
                r["verdict"] = "reached"
        new["parser"]["totals"]["reached"] += 1
        new["parser"]["totals"]["not_reached"] -= 1
        d = coverage_gaps.diff(old, new)
        self.assertEqual([(c["rule"], c["old"], c["new"], c["better"]) for c in d["rules"]],
                         [("atom", "not-reached", "reached", True)])
        self.assertEqual((d["totals"]["reached"], d["totals"]["not_reached"]), (1, -1))
        self.assertEqual(d["warnings"], [])
        new["parser"]["totals"]["parser_size"]["branches"] += 4
        new["meta"]["report_size"]["INSTRUCTION"] += 9
        warnings = coverage_gaps.diff(old, new)["warnings"]
        self.assertEqual(len(warnings), 2)
        self.assertTrue(all("without clean" in w for w in warnings))

    def test_diff_option_of_the_command_line(self):
        first, _ = self.result()
        old = self.write("old.json", json.dumps(first))
        bigger = self.write("bigger.xml", report_xml(JAVA, extra_size=40))
        res, text = self.result("--diff", old, report=bigger)
        self.assertEqual(res["diff"]["rules"], [])
        self.assertEqual(len(res["diff"]["warnings"]), 1)
        self.assertIn("the whole report has a different size", res["diff"]["warnings"][0])
        self.assertIn("CHANGES SINCE", text)
        self.assertIn("no change in the verdicts", text)


if __name__ == "__main__":
    unittest.main()
