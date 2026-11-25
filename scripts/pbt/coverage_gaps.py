#!/usr/bin/env python3
"""Coverage gaps of the Erlang parser rules and of chosen plugin classes, read from a JaCoCo XML report.

Usage:
  coverage_gaps.py REPORT.xml --bnf grammars/erlang.bnf [--parser-java gen/.../ErlangParser.java]
                   [--extra PREFIX ...] [--json OUT.json] [--diff OLD.json] [--all] [--top N]

See README.md next to this file. Python 3, standard library only.
"""

import argparse
import json
import os
import re
import sys
import xml.etree.ElementTree as ET

PARSER_CLASS = "org/intellij/erlang/parser/ErlangParser"
DEFAULT_EXTRAS = [
    "ErlangUnusedVariableInspection",
    "ErlangUnusedFunctionInspection",
    "ErlangUnusedTypeInspection",
    "ErlangUnboundVariableInspection",
    "ErlangUnresolvedFunctionInspection",
    "org/intellij/erlang/parser/_ErlangLexer",
]
SCHEMA = 1
MODIFIERS = {"private", "fake", "meta", "left", "external", "inner", "upper"}


# ---------------------------------------------------------------- bnf

def _strip_bnf(text):
    """Blank out comments, string literals and attribute blocks; keep line structure and offsets."""
    out = []
    i, n = 0, len(text)
    depth = 0  # depth of unquoted {...} attribute blocks
    while i < n:
        c = text[i]
        if text.startswith("//", i):
            j = text.find("\n", i)
            j = n if j < 0 else j
            out.append(" " * (j - i))
            i = j
            continue
        if text.startswith("/*", i):
            j = text.find("*/", i + 2)
            j = n if j < 0 else j + 2
            out.append(re.sub(r"[^\n]", " ", text[i:j]))
            i = j
            continue
        if c in "'\"":
            j = i + 1
            while j < n and text[j] != c and text[j] != "\n":
                j += 1
            j = min(j + 1, n)
            out.append(("Q" if depth == 0 else " ") + " " * (j - i - 1))
            i = j
            continue
        if c == "{":
            depth += 1
            out.append(" ")
            i += 1
            continue
        if c == "}" and depth:
            depth -= 1
            out.append(" ")
            i += 1
            continue
        out.append(c if depth == 0 or c == "\n" else " ")
        i += 1
    return "".join(out)


def parse_bnf(path):
    with open(path, encoding="utf-8") as f:
        text = f.read()
    raw_lines = text.split("\n")
    stripped = _strip_bnf(text)
    lines = stripped.split("\n")
    head = re.compile(r"^((?:[a-z]+\s+)*)([A-Za-z_]\w*)\s*::=")
    rules = []  # in bnf order
    cur = None
    for no, line in enumerate(lines, 1):
        m = head.match(line)
        if m and all(w in MODIFIERS for w in m.group(1).split()):
            cur = {"name": m.group(2), "line": no, "modifiers": m.group(1).split(),
                   "body": line[m.end():], "text": raw_lines[no - 1].strip(), "raw": raw_lines[no - 1]}
            rules.append(cur)
        elif cur is not None:
            cur["body"] += "\n" + line
            cur["raw"] += "\n" + raw_lines[no - 1]
    names = {r["name"] for r in rules}
    for r in rules:
        refs = []
        for ident in re.findall(r"[A-Za-z_]\w*", r["body"]):
            if ident in names and ident not in refs:
                refs.append(ident)
        # recoverWhile=X makes the generated parser call X, so X is part of the rule although it is in an attribute block
        for ident in re.findall(r"recoverWhile\s*=\s*([A-Za-z_]\w*)", r["raw"]):
            if ident in names and ident not in refs:
                refs.append(ident)
        r["refs"] = refs
        del r["body"]
        del r["raw"]
    # tokens: NAME = "text"
    tokens = {}
    for m in re.finditer(r"^\s*([A-Z_][A-Z0-9_]*)\s*=\s*\"([^\"]*)\"", text, re.M):
        tokens["ERL_" + m.group(1)] = m.group(2)
    # elementType=... per rule (for the Pratt blocks that name an element type)
    etypes = {}
    for m in re.finditer(r"^(?:[a-z]+\s+)*([A-Za-z_]\w*)\s*::=[^\n]*?elementType\s*=\s*(\w+)", text, re.M):
        etypes[m.group(1)] = m.group(2)
    return rules, tokens, etypes


# ---------------------------------------------------------------- generated parser

METHOD_RE = re.compile(r"^  (?:public |private |protected )?static boolean ([\w$]+)\(PsiBuilder b, int l(?:, [^)]*)?\) \{\s*$")
CALL_RE = re.compile(r"(?<![\w.])([a-z_][\w$]*)\(b, l(?: \+ 1)?(?:, [^()]*)?\)")
TOKCALL_RE = re.compile(r"\b(consumeTokens?(?:Smart|Fast)?|parseTokens?(?:Smart)?|nextTokenIs(?:Smart|Fast)?)\(b, ([^()]*)\)")
PARSER_REF_RE = re.compile(r"ErlangParser::([\w$]+)")


def parse_parser_java(path):
    with open(path, encoding="utf-8") as f:
        src = f.read().split("\n")
    methods = {}
    order = []
    i = 0
    while i < len(src):
        m = METHOD_RE.match(src[i])
        if not m:
            i += 1
            continue
        name = m.group(1)
        start = i + 1
        j = i + 1
        while j < len(src) and src[j] != "  }":
            j += 1
        end = j + 1
        comment = []
        k = i - 1
        while k >= 0 and src[k].startswith("  //"):
            comment.insert(0, src[k][4:].strip())
            k -= 1
        banner = k >= 0 and src[k].startswith("  /* ****")
        methods[name] = {"name": name, "start": start, "end": end, "comment": " ".join(comment),
                         "banner": banner, "pratt": ", int g)" in src[i]}
        order.append(name)
        i = j + 1
    lambdas = []  # (line, callee) of static Parser fields
    for no, line in enumerate(src, 1):
        m = re.match(r"^  static final Parser (\w+)_parser_ = \(b, l\) -> ([\w$]+)\(", line)
        if m:
            lambdas.append((no, m.group(2)))
    return src, methods, order, lambdas


def rule_of_method(mname, rule_names):
    base = mname[:-2] if mname.endswith("_$") else mname
    if base in rule_names:
        return base
    best = None
    for r in rule_names:
        if base.startswith(r + "_") and re.fullmatch(r"(_\d+)+", base[len(r):]):
            if best is None or len(r) > len(best):
                best = r
    return best


# ---------------------------------------------------------------- JaCoCo XML

def read_report(path):
    root = ET.parse(path).getroot()
    classes = {}
    sources = {}
    for pkg in root.iter("package"):
        pname = pkg.get("name")
        for c in pkg.findall("class"):
            meths = []
            for m in c.findall("method"):
                meths.append({"name": m.get("name"), "desc": m.get("desc"), "line": int(m.get("line") or 0),
                              "counters": _counters(m)})
            classes[c.get("name")] = {"name": c.get("name"), "source": pname + "/" + (c.get("sourcefilename") or ""),
                                      "methods": meths, "counters": _counters(c)}
        for s in pkg.findall("sourcefile"):
            lines = {}
            for ln in s.findall("line"):
                lines[int(ln.get("nr"))] = (int(ln.get("mi")), int(ln.get("ci")), int(ln.get("mb")), int(ln.get("cb")))
            sources[pname + "/" + s.get("name")] = lines
    return root, classes, sources


def _counters(el):
    out = {}
    for c in el.findall("counter"):
        out[c.get("type")] = (int(c.get("missed")), int(c.get("covered")))
    return out


# ---------------------------------------------------------------- analysis of the parser

def classify(text):
    t = text.strip()
    if "recursion_guard_(" in t:
        return "guard"
    if t.startswith("if (!nextTokenIs"):
        return "shortcut"
    if t.startswith("if (!empty_element_parsed_guard_"):
        return "loopguard"
    if t.startswith("if (!r) r = "):
        return "alt"
    if re.match(r"^(else )?if \(g < \d+ && ", t):
        return "pratt_cond"
    if t.startswith("r = r && ") or t.startswith("r = p && "):
        return "seq"
    if re.match(r"^if \(!.*\) break;$", t):
        return "loop"
    if t.startswith("r = !"):
        return "not"
    return "plain"


def call_executed(kind, cov):
    mi, ci, _, _ = cov
    if ci == 0:
        return False
    if kind in ("alt", "pratt_cond"):
        return mi == 0
    if kind == "seq":
        # An unexecuted call leaves the call and the jumps after it missed (6 or more instructions);
        # an executed one leaves at most 4 (see README).
        return mi < 5
    return True


class ParserAnalysis:
    def __init__(self, bnf_path, java_path, classes, sources, other_contexts=()):
        self.rules, self.tokens, self.etypes = parse_bnf(bnf_path)
        self.other_contexts = set(other_contexts)
        self.rule_by_name = {r["name"]: r for r in self.rules}
        self.src, self.methods, self.order, self.lambdas = parse_parser_java(java_path)
        cls = classes.get(PARSER_CLASS)
        if cls is None:
            raise SystemExit("error: the report has no class %s (was the parser loaded by the tests?)" % PARSER_CLASS)
        self.cls = cls
        self.lines = sources.get(PARSER_CLASS.rsplit("/", 1)[0] + "/ErlangParser.java", {})
        self.warnings = []
        if self.lines and max(self.lines) > len(self.src):
            raise SystemExit("error: the report has coverage for line %d, but %s has %d lines: the report and the Java "
                             "source come from different commits; give the ErlangParser.java of the report's revision "
                             "with --parser-java" % (max(self.lines), java_path, len(self.src)))
        self._check_alignment()
        rule_names = set(self.rule_by_name)
        for name, m in self.methods.items():
            m["rule"] = rule_of_method(name, rule_names)
            m["calls"] = self._calls(m)
        self.line_rule = {}  # line -> rule, for lines outside ordinary methods (Pratt blocks, lambdas)
        self.pratt_blocks = {}  # rule -> {"cond": line, "body": [lines]}
        self._split_pratt()
        self.caller_evidence = self._caller_evidence()
        self.matched = self._fixpoint()

    def _check_alignment(self):
        # Every method of the report starts inside the method of the same name in the source.
        bad = 0
        for m in self.cls["methods"]:
            sm = self.methods.get(m["name"])
            if sm is not None and not (sm["start"] <= m["line"] <= sm["end"]):
                bad += 1
        if bad:
            self.warnings.append("%d methods of the report do not match the lines of ErlangParser.java: "
                                 "the report and gen/ come from different commits" % bad)

    def cov(self, line):
        return self.lines.get(line)

    def _calls(self, m):
        calls = []
        for ln in range(m["start"] + 1, m["end"]):
            text = self.src[ln - 1]
            cov = self.cov(ln)
            if cov is None:
                continue
            kind = classify(text)
            if kind == "guard":
                continue
            callees = []
            for cm in TOKCALL_RE.finditer(text):
                toks = re.findall(r"ERL_\w+|\"[^\"]*\"", cm.group(2))
                callees.append(("token", " ".join("'%s'" % self._tok(t) for t in toks)))
            for cm in CALL_RE.finditer(text):
                name = cm.group(1)
                if name in self.methods:
                    callees.append(("method", name))
                elif name not in ("enter_section_", "exit_section_", "report_error_", "current_position_",
                                  "empty_element_parsed_guard_", "recursion_guard_", "addVariant"):
                    callees.append(("external", name))
            for cm in PARSER_REF_RE.finditer(text):
                if cm.group(1) in self.methods:
                    callees.append(("method", cm.group(1)))
            calls.append({"line": ln, "kind": kind, "callees": callees, "text": text.strip()})
        return calls

    def _tok(self, t):
        if t.startswith('"'):
            return t.strip('"')
        return self.tokens.get(t, t[4:].lower())

    def _split_pratt(self):
        loop = self.methods.get("expression_0")
        if not loop:
            return
        cur = None
        for ln in range(loop["start"] + 1, loop["end"]):
            text = self.src[ln - 1].strip()
            if re.match(r"^(else )?if \(g < \d+ && ", text):
                cur = {"cond": ln, "body": []}
                continue
            if cur is not None:
                if text == "}":
                    cur = None
                    continue
                cur["body"].append(ln)
                m = re.search(r"exit_section_\(b, l, m, (ERL_\w+),", text)
                if m:
                    rname = self._rule_for_etype(m.group(1))
                    if rname:
                        self.pratt_blocks[rname] = cur
                        self.line_rule[cur["cond"]] = rname
                        for b in cur["body"]:
                            self.line_rule[b] = rname
        for no, callee in self.lambdas:
            r = rule_of_method(callee, set(self.rule_by_name))
            if r:
                self.line_rule[no] = r

    def _rule_for_etype(self, etype):
        name = etype[4:].lower()
        if name in self.rule_by_name and self.etypes.get(name, name) == name:
            return name
        for r, e in self.etypes.items():
            if e == name:
                return r
        return name if name in self.rule_by_name else None

    def _ever_true(self, call):
        """The `if (!r) r = X` line of an alternative ran with r true at least once."""
        cov = self.cov(call["line"])
        return bool(cov) and cov[1] > 0 and (cov[3] >= 2 or cov[0] > 0)

    def _caller_evidence(self):
        """The methods whose result a caller used as true at least once: the line after the call ran only for that."""
        out = set()
        for m in self.methods.values():
            calls = [c for c in m["calls"] if c["kind"] != "shortcut"]
            for idx, c in enumerate(calls):
                names = [n for k, n in c["callees"] if k == "method"]
                if not names:
                    continue
                if c["kind"] == "pratt_cond":
                    nxt = self.cov(c["line"] + 1)
                    used = bool(nxt) and nxt[1] > 0
                elif idx + 1 < len(calls) and calls[idx + 1]["kind"] == "seq":
                    used = call_executed("seq", self.cov(calls[idx + 1]["line"]))
                elif idx + 1 < len(calls) and calls[idx + 1]["kind"] == "alt":
                    used = self._ever_true(calls[idx + 1])
                else:
                    used = False
                if used:
                    out.update(names)
        return out

    def _token_alternatives_matched(self, m, calls):
        """For a method that is only token alternatives: executed tokens do not show that one matched. Some alternative
        before the last one matched when the line after it saw r true; the last one is known only from the caller."""
        alts = [c for c in calls if c["kind"] == "alt"]
        return any(self._ever_true(c) for c in alts) or m["name"] in self.caller_evidence

    # matched(method): the method returned true at least once (see README for how this is read from lines)
    def _fixpoint(self):
        matched = {name: False for name in self.methods}
        changed = True
        while changed:
            changed = False
            for name, m in self.methods.items():
                if not matched[name] and self._matched_once(m, matched):
                    matched[name] = True
                    changed = True
        return matched

    def _callee_matched(self, call, matched):
        if not call_executed(call["kind"], self.cov(call["line"])):
            return False
        if not call["callees"]:
            return True
        for kind, name in call["callees"]:
            if kind == "method":
                if matched.get(name):
                    return True
            else:
                return True
        return False

    def _matched_once(self, m, matched):
        covered = any((self.cov(ln) or (0, 0))[1] > 0 for ln in range(m["start"], m["end"]))
        if not covered:
            return False
        calls = [c for c in m["calls"] if c["kind"] != "shortcut"]
        if m["name"] == "expression_0":
            return True
        kinds = {c["kind"] for c in calls}
        if "loopguard" in kinds:
            return any((self.cov(c["line"]) or (0, 0))[1] > 0 for c in calls if c["kind"] == "loopguard")
        seq = [c for c in calls if c["kind"] == "seq"]
        if seq:
            return call_executed("seq", self.cov(seq[0]["line"]))
        with_callees = [c for c in calls if c["callees"]]
        if not with_callees:
            return True
        if any(c["kind"] == "alt" for c in with_callees) and \
                all(k == "token" for c in with_callees for k, _ in c["callees"]):
            return self._token_alternatives_matched(m, with_callees)
        return any(self._callee_matched(c, matched) for c in with_callees)

    # ------------------------------------------------------------ per rule
    def rule_report(self):
        by_rule = {}
        for name in self.order:
            r = self.methods[name]["rule"]
            if r:
                by_rule.setdefault(r, []).append(name)
        out = []
        for rule in self.rules:
            name = rule["name"]
            meths = by_rule.get(name, [])
            lines = []
            for mn in meths:
                m = self.methods[mn]
                if mn == "expression_0":
                    lines += [ln for ln in range(m["start"], m["end"]) if ln not in self.line_rule]
                else:
                    lines += list(range(m["start"], m["end"]))
            lines += [ln for ln, r in self.line_rule.items() if r == name]
            lines = sorted(set(ln for ln in lines if ln in self.lines))
            mi = sum(self.lines[ln][0] for ln in lines)
            ci = sum(self.lines[ln][1] for ln in lines)
            mb = sum(self.lines[ln][2] for ln in lines)
            cb = sum(self.lines[ln][3] for ln in lines)
            guard_lines = [ln for ln in lines if "recursion_guard_(" in self.src[ln - 1]]
            gmb = sum(self.lines[ln][2] for ln in guard_lines)
            gcb = sum(self.lines[ln][3] for ln in guard_lines)
            entry = {"name": name, "line": rule["line"], "modifiers": rule["modifiers"], "text": rule["text"],
                     "methods": meths, "instructions": {"covered": ci, "missed": mi},
                     "branches": {"covered": cb, "missed": mb},
                     "branches_no_guard": {"covered": cb - gcb, "missed": mb - gmb},
                     "gaps": []}
            if "fake" in rule["modifiers"]:
                entry.update(verdict="no-code", why="fake rule: only a PSI type, never parsed")
            elif not lines:
                entry.update(verdict="no-code", why="no method of its own: inlined into the rules that use it")
            elif ci == 0:
                entry.update(verdict="not-reached", why="never called")
            else:
                main = self._main_method(name, meths)
                if name in self.pratt_blocks:
                    blk = self.pratt_blocks[name]
                    ok = any((self.cov(b) or (0, 0))[1] > 0 for b in blk["body"])
                    sub = [mn for mn in meths if mn != "expression_0"]
                    if not ok:
                        entry.update(verdict="not-reached", why="called, never matched (operator never parsed)")
                    entry["gaps"] = self._gaps(name, sub)
                elif main and not self.matched.get(main):
                    entry.update(verdict="not-reached", why="called, never matched its first element")
                    entry["gaps"] = self._gaps(name, meths)
                else:
                    entry["gaps"] = self._gaps(name, meths)
                if "verdict" not in entry:
                    entry["verdict"] = "partly" if entry["gaps"] else "reached"
                    entry["why"] = ("%d parts never ran" % len(entry["gaps"])) if entry["gaps"] else "every part ran"
            out.append(entry)
        erl = self.reachable(skip=self.other_contexts)
        for e in out:
            e["in_erl"] = e["name"] in erl
        return out

    def _main_method(self, rule, meths):
        for mn in meths:
            if mn in (rule, rule + "_$"):
                return mn
        return meths[0] if meths else None

    def _gaps(self, rule, meths):
        gaps = []
        never_called = set()
        for mn in meths:
            m = self.methods[mn]
            if mn == "expression_0":
                continue
            if not any((self.cov(ln) or (0, 0))[1] > 0 for ln in range(m["start"], m["end"])):
                never_called.add(mn)
        for mn in meths:
            m = self.methods[mn]
            if mn in never_called:
                parent_never = False
                # report only the outermost helper that never ran
                for other in never_called:
                    if other != mn and mn.startswith(other + "_"):
                        parent_never = True
                if not parent_never:
                    gaps.append({"line": m["start"], "kind": "never-called", "what": m["comment"] or mn,
                                 "method": mn})
                continue
            if mn == "expression_0":
                continue
            gaps += self._alt_never_matched(m)
            for c in m["calls"]:
                if not c["callees"] or c["kind"] in ("shortcut", "loopguard"):
                    continue
                if call_executed(c["kind"], self.cov(c["line"])):
                    continue
                if all(k == "method" and n in never_called for k, n in c["callees"]):
                    continue  # the helper itself is listed
                gaps.append({"line": c["line"], "kind": "never-tried", "what": self._describe(c),
                             "method": mn, "in": m["comment"]})
        if rule in self.pratt_blocks:
            blk = self.pratt_blocks[rule]
            cov = self.cov(blk["cond"])
            if cov and cov[0] > 0:
                gaps.append({"line": blk["cond"], "kind": "never-tried", "what": "the operator check",
                             "method": "expression_0", "in": self.src[blk["cond"] - 1].strip()})
        return gaps

    def _alt_never_matched(self, m):
        """Token alternatives a | b | c: the line of alternative i+1 shows whether r was ever true after
        alternative i. If it never was, alternatives 1..i never matched. Only tokens and external calls are
        reported here; an alternative that is a rule or a helper has a verdict of its own."""
        calls = m["calls"]
        gaps = []
        for idx, c in enumerate(calls):
            if c["kind"] != "alt" or idx == 0:
                continue
            prev = calls[idx - 1]
            if prev["kind"] not in ("plain", "alt") or not prev["callees"]:
                continue
            cov = self.cov(c["line"])
            if not cov or cov[1] == 0:
                continue
            mi, ci, mb, cb = cov
            ever_true = cb >= 2 or mi > 0
            if ever_true or not call_executed(prev["kind"], self.cov(prev["line"])):
                continue
            if all(k != "method" for k, _ in prev["callees"]):
                gaps.append({"line": prev["line"], "kind": "never-matched", "what": self._describe(prev),
                             "method": m["name"], "in": m["comment"]})
        return gaps

    def _describe(self, call):
        parts = []
        for kind, name in call["callees"]:
            if kind == "method":
                m = self.methods[name]
                r = m["rule"]
                parts.append(r if (r and name in (r, r + "_$")) else (m["comment"] or name))
            elif kind == "token":
                parts.append(name)
            else:
                parts.append("<<%s>>" % name)
        return " ".join(parts)

    # rule graph: which not-reached rules each not-reached rule dominates (all paths from forms pass through it)
    def reachable(self, root="forms", skip=()):
        graph = {r["name"]: r["refs"] for r in self.rules}
        reach = set()
        stack = [root]
        while stack:
            n = stack.pop()
            if n in reach or n in skip:
                continue
            reach.add(n)
            stack.extend(graph.get(n, []))
        return reach

    def dominators(self, root="forms"):
        graph = {r["name"]: r["refs"] for r in self.rules}
        reach = set()
        stack = [root]
        while stack:
            n = stack.pop()
            if n in reach:
                continue
            reach.add(n)
            stack.extend(graph.get(n, []))
        preds = {n: set() for n in reach}
        for n in reach:
            for s in graph.get(n, []):
                if s in reach:
                    preds[s].add(n)
        dom = {n: set(reach) for n in reach}
        dom[root] = {root}
        changed = True
        while changed:
            changed = False
            for n in reach:
                if n == root:
                    continue
                ps = [dom[p] for p in preds[n]]
                new = set.intersection(*ps) | {n} if ps else {n}
                if new != dom[n]:
                    dom[n] = new
                    changed = True
        return reach, dom

    def totals(self, rules):
        c = self.cls["counters"]
        guard_lines = [ln for ln in self.lines if "recursion_guard_(" in self.src[ln - 1]] if self.lines else []
        gmb = sum(self.lines[ln][2] for ln in guard_lines)
        gcb = sum(self.lines[ln][3] for ln in guard_lines)
        counts = {}
        for r in rules:
            counts[r["verdict"]] = counts.get(r["verdict"], 0) + 1
        bm, bc = c.get("BRANCH", (0, 0))
        im, ic = c.get("INSTRUCTION", (0, 0))
        erl = [r for r in rules if r["in_erl"]]
        erl_counts = {}
        for r in erl:
            erl_counts[r["verdict"]] = erl_counts.get(r["verdict"], 0) + 1
        return {"rules": len(rules), "erl_rules": {"rules": len(erl), "reached": erl_counts.get("reached", 0),
                                                   "partly": erl_counts.get("partly", 0),
                                                   "not_reached": erl_counts.get("not-reached", 0),
                                                   "no_code": erl_counts.get("no-code", 0)},
                "reached": counts.get("reached", 0), "partly": counts.get("partly", 0),
                "not_reached": counts.get("not-reached", 0), "no_code": counts.get("no-code", 0),
                "parser_branches": {"covered": bc, "missed": bm, "pct": pct(bc, bc + bm)},
                "parser_branches_no_guard": {"covered": bc - gcb, "missed": bm - gmb,
                                             "pct": pct(bc - gcb, bc - gcb + bm - gmb)},
                "parser_instructions": {"covered": ic, "missed": im, "pct": pct(ic, ic + im)},
                "parser_size": {"instructions": ic + im, "branches": bc + bm}}


def pct(a, b):
    return round(100.0 * a / b, 1) if b else 0.0


# ---------------------------------------------------------------- extra classes

def extras_report(classes, prefixes):
    out = []
    for name in sorted(classes):
        simple = name.rsplit("/", 1)[-1]
        if not any(name.startswith(p) or simple.startswith(p) for p in prefixes):
            continue
        c = classes[name]
        meths = []
        for m in c["methods"]:
            lm, lc = m["counters"].get("LINE", (0, 0))
            bm, bc = m["counters"].get("BRANCH", (0, 0))
            im, ic = m["counters"].get("INSTRUCTION", (0, 0))
            meths.append({"name": m["name"], "desc": m["desc"], "line": m["line"],
                          "lines": {"covered": lc, "missed": lm}, "branches": {"covered": bc, "missed": bm},
                          "zero": ic == 0})
        lm, lc = c["counters"].get("LINE", (0, 0))
        bm, bc = c["counters"].get("BRANCH", (0, 0))
        out.append({"class": name, "source": c["source"], "lines": {"covered": lc, "missed": lm},
                    "branches": {"covered": bc, "missed": bm}, "methods": meths})
    missing = [p for p in prefixes if not any(e["class"].startswith(p) or e["class"].rsplit("/", 1)[-1].startswith(p)
                                              for e in out)]
    return out, missing


# ---------------------------------------------------------------- top gaps

def top_gaps(an, rules):
    reach, dom = an.dominators()
    erl = an.reachable(skip=an.other_contexts)
    nr = {r["name"] for r in rules if r["verdict"] == "not-reached"}
    by_name = {r["name"]: r for r in rules}
    res = []
    for r in rules:
        if r["verdict"] != "not-reached":
            continue
        name = r["name"]
        if name not in reach:
            blocked = [name]
            outer = None
        else:
            blocked = sorted((n for n in nr if n in reach and name in dom[n]), key=lambda n: by_name[n]["line"])
            # the nearest not-reached rule that dominates this one, if any: then this gap is inside that one
            doms = [d for d in dom[name] if d != name and d in nr]
            outer = max(doms, key=lambda d: len(dom[d])) if doms else None
        res.append({"name": name, "line": r["line"], "why": r["why"], "blocks": len(blocked),
                    "blocked": blocked, "inside": outer, "from_root": name in reach, "in_erl": name in erl})
    res.sort(key=lambda g: (not g["in_erl"], g["inside"] is not None, -g["blocks"], g["line"]))
    return res


# ---------------------------------------------------------------- diff

def diff(old, new):
    o = {r["name"]: r for r in old["parser"]["rules"]}
    n = {r["name"]: r for r in new["parser"]["rules"]}
    rank = {"no-code": -1, "not-reached": 0, "partly": 1, "reached": 2}
    changes = []
    for name, r in n.items():
        if name not in o:
            changes.append({"rule": name, "old": None, "new": r["verdict"], "line": r["line"]})
        elif o[name]["verdict"] != r["verdict"]:
            changes.append({"rule": name, "old": o[name]["verdict"], "new": r["verdict"], "line": r["line"],
                            "better": rank[r["verdict"]] > rank[o[name]["verdict"]]})
    for name in o:
        if name not in n:
            changes.append({"rule": name, "old": o[name]["verdict"], "new": None, "line": o[name]["line"]})
    ot, nt = old["parser"]["totals"], new["parser"]["totals"]
    tot = {}
    for k in ("reached", "partly", "not_reached"):
        tot[k] = nt[k] - ot[k]
    for k in ("parser_branches", "parser_branches_no_guard", "parser_instructions"):
        tot[k] = {"covered": nt[k]["covered"] - ot[k]["covered"], "pct": round(nt[k]["pct"] - ot[k]["pct"], 1)}
    oe = {(c["class"], m["name"], m["desc"]): m for c in old.get("extras", []) for m in c["methods"]}
    ne = {(c["class"], m["name"], m["desc"]): m for c in new.get("extras", []) for m in c["methods"]}
    meth = []
    for k, m in ne.items():
        p = oe.get(k)
        if p is None:
            continue
        if p["zero"] != m["zero"] or p["branches"]["covered"] != m["branches"]["covered"] \
                or p["lines"]["covered"] != m["lines"]["covered"]:
            meth.append({"class": k[0], "method": k[1], "desc": k[2],
                         "lines": [p["lines"]["covered"], m["lines"]["covered"]],
                         "branches": [p["branches"]["covered"], m["branches"]["covered"]],
                         "zero": [p["zero"], m["zero"]]})
    warnings = []
    if ot.get("parser_size") != nt.get("parser_size"):
        warnings.append("ErlangParser has a different size (%s -> %s): the parser changed between the runs, or one "
                        "report was made without clean after a change in src/ or gen/" % (ot.get("parser_size"),
                                                                                         nt.get("parser_size")))
    if old["meta"].get("report_size") != new["meta"].get("report_size"):
        warnings.append("the whole report has a different size (%s -> %s): plugin code changed between the runs, or "
                        "one report was made without clean after a change in src/ or gen/"
                        % (old["meta"].get("report_size"), new["meta"].get("report_size")))
    return {"old_report": old["meta"]["report"], "rules": sorted(changes, key=lambda c: c["line"]),
            "totals": tot, "methods": meth, "warnings": warnings}


# ---------------------------------------------------------------- output

def fmt_cnt(d):
    total = d["covered"] + d["missed"]
    return "%d/%d" % (d["covered"], total)


def print_text(res, show_all, top):
    p = res["parser"]
    t = p["totals"]
    w = sys.stdout.write
    for msg in res["meta"]["warnings"]:
        w("WARNING: %s\n" % msg)
    w("Report: %s\nGrammar: %s\n\n" % (res["meta"]["report"], res["meta"]["bnf"]))
    w("PARSER RULES (%d in the bnf)\n" % t["rules"])
    w("  reached %d, partly %d, not reached %d, no code of their own %d\n"
      % (t["reached"], t["partly"], t["not_reached"], t["no_code"]))
    e = t["erl_rules"]
    w("  of them used in .erl files (%d): reached %d, partly %d, not reached %d\n"
      % (e["rules"], e["reached"], e["partly"], e["not_reached"]))
    b, bg, ins = t["parser_branches"], t["parser_branches_no_guard"], t["parser_instructions"]
    w("  ErlangParser branches %s (%.1f%%), without recursion guards %s (%.1f%%), instructions %s (%.1f%%)\n\n"
      % (fmt_cnt(b), b["pct"], fmt_cnt(bg), bg["pct"], fmt_cnt(ins), ins["pct"]))

    if "diff" in res:
        d = res["diff"]
        w("CHANGES SINCE %s\n" % d["old_report"])
        for msg in d["warnings"]:
            w("  WARNING: %s\n" % msg)
        dt = d["totals"]
        w("  reached %+d, partly %+d, not reached %+d; branches %+d (%+.1f points)\n"
          % (dt["reached"], dt["partly"], dt["not_reached"], dt["parser_branches"]["covered"],
             dt["parser_branches"]["pct"]))
        for c in d["rules"]:
            w("  %-6s bnf:%-4d %-34s %s -> %s\n" % ("better" if c.get("better") else "worse" if c.get("old") and c.get("new") else "",
                                                 c["line"], c["rule"], c["old"], c["new"]))
        for m in d["methods"]:
            w("  %s.%s lines %d -> %d, branches %d -> %d\n" % (m["class"].rsplit("/", 1)[-1], m["method"],
                                                            m["lines"][0], m["lines"][1], m["branches"][0],
                                                            m["branches"][1]))
        if not d["rules"] and not d["methods"]:
            w("  no change in the verdicts of the rules or in the methods of the extra classes\n")
        w("\n")

    gaps = res["top_gaps"]
    w("TOP GAPS (not-reached rules; first those not inside another gap, by how many not-reached rules\n"
      "          can only be reached through them, then bnf order)\n")
    for g in gaps[:top]:
        extra = "" if g["in_erl"] else "  [not used from forms]" if not g["from_root"] else "  [not in .erl files]"
        inside = ("  inside %s" % g["inside"]) if g["inside"] else ""
        w("  bnf:%-4d %-34s blocks %-3d %s%s%s\n" % (g["line"], g["name"], g["blocks"], g["why"], inside, extra))
    if len(gaps) > top:
        w("  ... %d more (see the list below or --top)\n" % (len(gaps) - top))
    w("\n")

    def rules_with(verdict):
        return [r for r in p["rules"] if r["verdict"] == verdict]

    w("NOT REACHED\n")
    for r in rules_with("not-reached"):
        w("  bnf:%-4d %-34s %-9s %s%s\n" % (r["line"], r["name"], " ".join(r["modifiers"]), r["why"],
                                           "" if r["in_erl"] else "  [not in .erl files]"))
    w("\nPARTLY REACHED (parts that never ran; L = line in ErlangParser.java)\n")
    for r in rules_with("partly"):
        w("  bnf:%-4d %-34s %-9s instr %s, branches %s\n" % (r["line"], r["name"], " ".join(r["modifiers"]),
                                                          fmt_cnt(r["instructions"]), fmt_cnt(r["branches"])))
        for g in r["gaps"]:
            where = ("  in: %s" % g["in"]) if g.get("in") and g["kind"] != "never-called" else ""
            w("      L%-5d %-11s %s%s\n" % (g["line"], g["kind"], g["what"], where))
    if show_all:
        w("\nREACHED\n")
        for r in rules_with("reached"):
            w("  bnf:%-4d %-34s %-9s instr %s, branches %s\n" % (r["line"], r["name"], " ".join(r["modifiers"]),
                                                              fmt_cnt(r["instructions"]), fmt_cnt(r["branches"])))
    nc = rules_with("no-code")
    if nc:
        w("\nNO CODE OF THEIR OWN\n")
        for r in nc:
            w("  bnf:%-4d %-34s %s\n" % (r["line"], r["name"], r["why"]))

    w("\nEXTRA CLASSES (per method: lines covered/total, branches covered/total)\n")
    for c in res["extras"]:
        w("  %s  lines %s, branches %s\n" % (c["class"], fmt_cnt(c["lines"]), fmt_cnt(c["branches"])))
        for m in c["methods"]:
            if m["zero"] or show_all or m["branches"]["missed"] or m["lines"]["missed"]:
                w("    %-4s %-40s L%-5d lines %-7s branches %s\n" % ("ZERO" if m["zero"] else "", m["name"], m["line"],
                                                                  fmt_cnt(m["lines"]), fmt_cnt(m["branches"])))
        zero = [m["name"] for m in c["methods"] if m["zero"]]
        w("    methods with zero coverage: %s\n" % (", ".join(zero) if zero else "none"))
    for p_ in res["meta"]["extras_missing"]:
        w("  %s: no class in the report (never loaded, or a wrong prefix)\n" % p_)


def main(argv=None):
    ap = argparse.ArgumentParser(description="Coverage gaps of the Erlang parser rules and chosen classes.")
    ap.add_argument("report", help="JaCoCo XML report (build/reports/jacoco/coverageReport/coverageReport.xml)")
    ap.add_argument("--bnf", required=True, help="path to grammars/erlang.bnf")
    ap.add_argument("--parser-java", help="ErlangParser.java (default: gen/... next to the grammars/ directory)")
    ap.add_argument("--extra", action="append", help="class name prefix to report per method (repeatable; "
                                                     "replaces the default list)")
    ap.add_argument("--json", help="write the full result as JSON to this file ('-' for stdout)")
    ap.add_argument("--diff", help="an earlier --json result to compare with")
    ap.add_argument("--all", action="store_true", help="also list reached rules and fully covered methods")
    ap.add_argument("--other-contexts", default="application_file_expression,console_expression,config_expression",
                    help="comma-separated rules that serve other file kinds (.app, .config, the console); rules "
                         "reachable from forms only through them are marked 'not in .erl files' (default: %(default)s)")
    ap.add_argument("--top", type=int, default=15, help="length of the top gaps list (default 15)")
    a = ap.parse_args(argv)

    java = a.parser_java or os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(a.bnf))),
                                         "gen", "org", "intellij", "erlang", "parser", "ErlangParser.java")
    root, classes, sources = read_report(a.report)
    an = ParserAnalysis(a.bnf, java, classes, sources, a.other_contexts.split(",") if a.other_contexts else ())
    rules = an.rule_report()
    extras, missing = extras_report(classes, a.extra or DEFAULT_EXTRAS)
    res = {"schema": SCHEMA,
           "meta": {"report": os.path.abspath(a.report), "bnf": os.path.abspath(a.bnf), "parser_java": java,
                    "session": [s.get("id") for s in root.findall("sessioninfo")],
                    "report_size": {c.get("type"): int(c.get("missed")) + int(c.get("covered"))
                                    for c in root.findall("counter") if c.get("type") in ("INSTRUCTION", "BRANCH")},
                    "warnings": an.warnings, "extras_missing": missing,
                    "top_gaps_order": "not-reached rules not inside another not-reached rule first; then by the "
                                      "number of not-reached rules that every path from forms reaches only "
                                      "through this rule (bnf dominators); then bnf order"},
           "parser": {"totals": an.totals(rules), "rules": rules},
           "top_gaps": top_gaps(an, rules),
           "extras": extras}
    if a.diff:
        with open(a.diff, encoding="utf-8") as f:
            res["diff"] = diff(json.load(f), res)
    if a.json:
        data = json.dumps(res, indent=1)
        if a.json == "-":
            print(data)
            return 0
        with open(a.json, "w", encoding="utf-8") as f:
            f.write(data + "\n")
    print_text(res, a.all, a.top)
    return 0


if __name__ == "__main__":
    sys.exit(main())
