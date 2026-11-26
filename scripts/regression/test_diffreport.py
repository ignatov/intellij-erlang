"""Unit tests of diffreport.py: python3 -m unittest discover -s scripts/regression -p 'test_*.py' -v"""
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import diffreport  # noqa: E402

META = {"meta": True, "tools": ["ErlangUnresolvedFunction", "ErlangUnusedVariable", "ErlangSingleClauseCase"],
        "toolsOffByDefault": ["ErlangSingleClauseCase"]}


def rec(name, **kw):
    r = {"file": name, "lines": 10, "lexHash": "l", "errors": [], "treeHash": "t", "formatHash": "f",
         "formatTokensChanged": False, "problems": [], "exceptions": []}
    r.update(kw)
    return r


def problem(tool, line, descr):
    return "WARNING|%s|%d|text|%s" % (tool, line, descr)


class Base(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)

    def path(self, name):
        return os.path.join(self.dir.name, name)

    def write(self, name, lines):
        with open(self.path(name), "w", encoding="utf-8") as f:
            for line in lines:
                f.write((json.dumps(line) if isinstance(line, dict) else line) + "\n")
        return self.path(name)

    def report(self, base, cand, known=None, shards=1, end=True):
        tail = [{"end": True, "complete": True, "files": 0, "wanted": 0}] if end else []
        b = self.write("base.jsonl", [META] + base + tail)
        c = self.write("cand.jsonl", [META] + cand + tail)
        args = [b, c, self.path("report.json"), "--summary", self.path("summary.md"), "--shards", str(shards)]
        if known is not None:
            args += ["--known", self.write("known.json", [json.dumps({"entries": known})])]
        self.assertEqual(0, diffreport.main(args))
        with open(self.path("report.json"), encoding="utf-8") as f:
            return json.load(f)


class ChecksTest(Base):
    def test_same_records_give_nothing(self):
        r = self.report([rec("a.erl")], [rec("a.erl")])
        self.assertEqual([], r["regressionCandidates"])
        self.assertEqual([], r["families"])
        self.assertTrue(r["complete"])

    def test_new_parse_error_is_a_candidate_but_a_moved_one_is_not(self):
        r = self.report([rec("a.erl", errors=["3: ')' expected"]), rec("b.erl", errors=["3: ')' expected"])],
                        [rec("a.erl", errors=["5: ')' expected"]),
                         rec("b.erl", errors=["3: ')' expected", "9: ')' expected"])])
        self.assertEqual([("parser", "b.erl", ["9: ')' expected"])],
                         [(c["check"], c["file"], c["detail"]) for c in r["regressionCandidates"]])
        self.assertEqual(["parser:')' expected"], [f["key"] for f in r["families"]])
        self.assertEqual("area:parser", r["families"][0]["area"])

    def test_problems_of_inspections_off_by_default_are_counted_but_not_candidates(self):
        r = self.report([rec("a.erl")], [rec("a.erl", problems=[problem("ErlangSingleClauseCase", 1, "single")])])
        self.assertEqual([], r["regressionCandidates"])
        self.assertEqual(1, r["checks"]["inspections"]["addedOffByDefault"])

    def test_one_family_per_inspection_over_files(self):
        r = self.report([rec("a.erl"), rec("b.erl")],
                        [rec("a.erl", problems=[problem("ErlangUnresolvedFunction", 1, "Unresolved function f/0")]),
                         rec("b.erl", problems=[problem("ErlangUnresolvedFunction", 2, "Unresolved function g/1"),
                                                problem("ErlangUnusedVariable", 3, "Variable 'X' is unused")])])
        fams = {f["key"]: f for f in r["families"]}
        self.assertEqual({"inspections:ErlangUnresolvedFunction", "inspections:ErlangUnusedVariable"}, set(fams))
        self.assertEqual(2, fams["inspections:ErlangUnresolvedFunction"]["files"])
        self.assertEqual("area:psi-resolve", fams["inspections:ErlangUnresolvedFunction"]["area"])
        self.assertEqual("area:inspections", fams["inspections:ErlangUnusedVariable"]["area"])
        self.assertEqual(2, len(r["checks"]["resolve"]["filesWithNewUnresolved"]))

    def test_family_ids_are_stable(self):
        self.assertEqual(diffreport.family_id("parser:x"), diffreport.family_id("parser:x"))
        self.assertNotEqual(diffreport.family_id("parser:x"), diffreport.family_id("parser:y"))
        self.assertRegex(diffreport.family_id("inspections:ErlangUnresolvedFunction"), r"^inspections-[0-9a-f]{10}$")

    def test_formatter_and_exceptions(self):
        r = self.report([rec("a.erl"), rec("b.erl")],
                        [rec("a.erl", formatTokensChanged=True),
                         rec("b.erl", exceptions=["highlight: java.lang.NullPointerException: x is null"])])
        keys = {f["key"]: f for f in r["families"]}
        self.assertEqual({"formatter:tokens", "exceptions:java.lang.NullPointerException in highlight"}, set(keys))
        self.assertEqual("area:formatter", keys["formatter:tokens"]["area"])

    def test_timing_family_needs_several_slow_large_files(self):
        def slow(name, base, cand, lines=2000):
            return rec(name, lines=lines, highlightUs=base * 1000), rec(name, lines=lines, highlightUs=cand * 1000)
        pairs = [slow("a.erl", 1000, 3000), slow("b.erl", 1000, 1600), slow("c.erl", 100, 250),
                 slow("small.erl", 1000, 5000, lines=10)]
        r = self.report([p[0] for p in pairs], [p[1] for p in pairs])
        self.assertEqual(["a.erl", "b.erl"], sorted(c["file"] for c in r["regressionCandidates"]))
        self.assertEqual([], r["families"])  # two files are not enough
        pairs += [slow("d.erl", 2000, 9000)]
        r = self.report([p[0] for p in pairs], [p[1] for p in pairs])
        self.assertEqual(["timing:highlight"], [f["key"] for f in r["families"]])
        self.assertEqual("d.erl", r["families"][0]["examples"][0]["file"])  # the largest slowdown first

    def test_format_timing_is_never_a_candidate(self):
        r = self.report([rec("a.erl", lines=5000, formatUs=1000000)], [rec("a.erl", lines=5000, formatUs=9000000)])
        self.assertEqual([], r["regressionCandidates"])
        self.assertEqual(1, len(r["checks"]["timing"]["slowdowns"]))


class KnownTest(Base):
    def test_known_inspection_text_is_not_new(self):
        known = [{"issue": 1636, "check": "inspections", "tool": "ErlangUnresolvedFunction",
                  "text": "Unresolved function defined/1"}]
        r = self.report([rec("a.erl")],
                        [rec("a.erl", problems=[problem("ErlangUnresolvedFunction", 4, "Unresolved function defined/1"),
                                                problem("ErlangUnresolvedFunction", 5, "Unresolved function g/0")])],
                        known=known)
        self.assertEqual([["WARNING|ErlangUnresolvedFunction|5|text|Unresolved function g/0"]],
                         [c["detail"] for c in r["regressionCandidates"]])
        self.assertEqual([(1636, 1, ["a.erl"])], [(h["issue"], h["items"], h["files"]) for h in r["known"]])

    def test_known_timing_up_to_a_ratio(self):
        known = [{"issue": 1503, "check": "timing", "metric": "highlight", "maxRatio": 3.0}]
        base = [rec(n, lines=2000, highlightUs=1000000) for n in ("a.erl", "b.erl", "c.erl", "d.erl")]
        cand = [rec(n, lines=2000, highlightUs=us) for n, us in
                (("a.erl", 2000000), ("b.erl", 2500000), ("c.erl", 4000000), ("d.erl", 2900000))]
        r = self.report(base, cand, known=known)
        self.assertEqual(["c.erl"], [c["file"] for c in r["regressionCandidates"]])
        self.assertEqual(3, r["known"][0]["items"])

    def test_file_glob_limits_an_entry(self):
        known = [{"check": "formatter", "file": "otp/*", "why": "accepted"}]
        r = self.report([rec("otp/a.erl"), rec("cowboy/b.erl")],
                        [rec("otp/a.erl", formatTokensChanged=True), rec("cowboy/b.erl", formatTokensChanged=True)],
                        known=known)
        self.assertEqual(["cowboy/b.erl"], [c["file"] for c in r["regressionCandidates"]])
        self.assertIn("accepted", open(self.path("summary.md"), encoding="utf-8").read())

    def test_the_committed_allowlist_loads(self):
        entries = diffreport.load_known(os.path.join(os.path.dirname(os.path.abspath(__file__)), "known.json"))
        self.assertEqual({1636, 1503}, {e.get("issue") for e in entries})


class InputsTest(Base):
    def test_incomplete_without_every_end_line(self):
        r = self.report([rec("a.erl")], [rec("a.erl")], shards=2)
        self.assertFalse(r["complete"])
        r = self.report([rec("a.erl")], [rec("a.erl")], end=False)
        self.assertFalse(r["complete"])
        self.assertIn("incomplete", open(self.path("summary.md"), encoding="utf-8").read())

    def test_files_of_one_side_only_are_counted(self):
        r = self.report([rec("a.erl"), rec("b.erl")], [rec("a.erl")])
        self.assertEqual((1, 1, 0), (r["files"], r["onlyInBase"], r["onlyInCandidate"]))
        self.assertFalse(r["complete"])

    def test_unreadable_input_exits_with_2(self):
        b = self.write("base.jsonl", [json.dumps(META), "{not json"])
        c = self.write("cand.jsonl", [json.dumps(META)])
        self.assertEqual(2, diffreport.main([b, c, self.path("r.json")]))
        self.assertEqual(2, diffreport.main([self.path("missing.jsonl"), c, self.path("r.json")]))
        self.assertFalse(os.path.exists(self.path("r.json")))

    def test_summary_escapes_table_cells(self):
        r = self.report([rec("a|b.erl")], [rec("a|b.erl", errors=["1: <bad> | x"])])
        text = open(self.path("summary.md"), encoding="utf-8").read()
        self.assertIn("a\\|b.erl", text)
        self.assertIn("&lt;bad>", text)
        self.assertEqual(1, len(r["families"]))


if __name__ == "__main__":
    unittest.main()
