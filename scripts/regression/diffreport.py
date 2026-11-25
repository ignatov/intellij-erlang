#!/usr/bin/env python3
"""Usage: diffreport.py BASE.jsonl CANDIDATE.jsonl OUT.json [--known KNOWN.json] [--summary OUT.md]
                     [--base-ref R] [--cand-ref R] [--shards N]

Compares the per-file records of RegressionDiffTest for a base and a candidate build of the plugin and writes one JSON
report: a section per check (lexer, parser, inspections, resolve, formatter, exceptions, timing), the regression
candidates per file, and the families of new candidates, which are what a person or the dispatcher files as issues.
--known names the allowlist (known.json next to this script): the candidates it covers are counted under "known" and
are neither candidates nor families. --summary also writes a short Markdown summary.

A candidate is a difference that can be a regression: a new parse error, a new problem of an inspection that is on by
default, a formatter that changes a token other than white space, a new exception, or a slowdown of lexing, parsing
or highlighting over SLOWDOWN_RATIO and SLOWDOWN_MIN_MS on a file of at least SLOWDOWN_MIN_LINES lines. Formatter
timings are not candidates: one sample per file varies up to 3x between runs. A timing family needs at least
MIN_TIMING_FILES slow files, so one noisy file is not filed.

Exit code: 0 when the report was written, 2 when an input cannot be read. Findings never change the exit code.
Python 3 standard library only.
"""
import argparse
import collections
import fnmatch
import hashlib
import json
import re
import sys

SLOWDOWN_RATIO = 1.5
SLOWDOWN_MIN_LINES = 1000
SLOWDOWN_MIN_MS = 200
MIN_TIMING_FILES = 3
TIMING_METRICS = ("lex", "parse", "highlight")
CAP = 300
EXAMPLES = 10


class InputError(Exception):
    pass


def load(path):
    """(meta, records by file, ends) of one side: meta merges the meta lines of the shards, ends are their end lines."""
    meta, recs, ends = {"tools": [], "toolsOffByDefault": []}, {}, []
    try:
        with open(path, encoding="utf-8") as f:
            for number, line in enumerate(f, 1):
                if not line.strip():
                    continue
                try:
                    r = json.loads(line)
                except ValueError as e:
                    raise InputError("%s:%d: %s" % (path, number, e))
                if r.get("meta"):
                    for key in ("tools", "toolsOffByDefault"):
                        meta[key] = sorted(set(meta[key]) | set(r.get(key) or []))
                elif r.get("end"):
                    ends.append(r)
                elif isinstance(r.get("file"), str):
                    recs[r["file"]] = r
    except OSError as e:
        raise InputError(str(e))
    return meta, recs, ends


def complete(ends, shards):
    """True when every shard wrote its end line and each says it did all its files."""
    return len(ends) == shards and all(e.get("complete") is True for e in ends)


def tool(problem):
    return problem.split("|")[1]


def problem_text(problem):
    """The description of a problem `severity|tool|line|source text|description`."""
    return problem.split("|", 4)[4] if problem.count("|") >= 4 else problem


def error_text(error):
    """The description of a parse error `line: description`."""
    return error.split(": ", 1)[1] if ": " in error else error


def exception_kind(exception):
    """(phase, class) of an exception `phase: java.lang.Class: message`."""
    phase, _, rest = exception.partition(": ")
    cls = rest.split(":", 1)[0].strip().split()[0] if rest.strip() else "?"
    return phase, cls


def family_id(key):
    check = key.split(":", 1)[0]
    return "%s-%s" % (check, hashlib.sha1(key.encode("utf-8")).hexdigest()[:10])


def area_of(check, item):
    if check == "parser":
        return "area:parser"
    if check == "formatter":
        return "area:formatter"
    if check == "inspections":
        return "area:psi-resolve" if tool(item).startswith("ErlangUnresolved") else "area:inspections"
    phase = item.get("metric") if check == "timing" else exception_kind(item)[0]
    return {"lex": "area:parser", "parse": "area:parser", "format": "area:formatter"}.get(phase, "area:inspections")


def load_known(path):
    if not path:
        return []
    try:
        with open(path, encoding="utf-8") as f:
            data = json.load(f)
    except (OSError, ValueError) as e:
        raise InputError("%s: %s" % (path, e))
    entries = data.get("entries") if isinstance(data, dict) else None
    if not isinstance(entries, list) or not all(isinstance(e, dict) and e.get("check") for e in entries):
        raise InputError("%s: expected {\"entries\": [{\"check\": ...}, ...]}" % path)
    return entries


def covers(entry, check, file, item):
    """True when an allowlist entry covers one candidate item of a file. Every key an entry has must match: check;
    file (a glob); tool (inspections); text (a part of the problem, error or exception text); metric and maxRatio
    (timing)."""
    if entry.get("check") != check:
        return False
    if entry.get("file") and not fnmatch.fnmatchcase(file, entry["file"]):
        return False
    if check == "inspections":
        if entry.get("tool") and tool(item) != entry["tool"]:
            return False
        text = problem_text(item)
    elif check == "parser":
        text = error_text(item)
    elif check == "exceptions":
        text = item
    elif check == "timing":
        if entry.get("metric") and item["metric"] != entry["metric"]:
            return False
        if entry.get("maxRatio") is not None and item["ratio"] > float(entry["maxRatio"]):
            return False
        text = ""
    else:
        text = ""
    return not entry.get("text") or entry["text"] in text


def family_of(check, item):
    """(key, subject) of the family of one candidate item: one family per parse error text, per inspection, per
    exception class and phase, per timing metric, and one for the formatter."""
    if check == "parser":
        subject = re.sub(r"\s+", " ", error_text(item))[:120]
    elif check == "inspections":
        subject = tool(item)
    elif check == "exceptions":
        subject = "%s in %s" % (exception_kind(item)[1], exception_kind(item)[0])
    elif check == "timing":
        subject = item["metric"]
    else:
        subject = "tokens"
    return "%s:%s" % (check, subject), subject


def compare(b, c, off, known):
    """The sections of the checks, the candidates per file, the families and what the allowlist covered."""
    files = sorted(set(b) & set(c))
    lexer = {"filesChanged": 0}
    parser = {"filesErrorsChanged": 0, "errorsBase": 0, "errorsCandidate": 0, "filesTreeChanged": 0,
              "filesWithNewErrors": []}
    insp = {"filesChanged": 0, "added": collections.Counter(), "removed": collections.Counter(), "addedOffByDefault": 0,
            "addedProblems": []}
    resolve = {"unresolvedBase": 0, "unresolvedCandidate": 0, "filesWithNewUnresolved": []}
    fmt = {"filesChanged": 0, "filesWithNewTokenChanges": [], "filesWithFixedTokenChanges": 0}
    exc = {"new": [], "fixed": []}
    timing = {"totalsMs": collections.Counter(), "slowdowns": []}
    items = []  # (check, file, item): one candidate item each

    for f in files:
        o, n = b[f], c[f]
        if o.get("lexHash") != n.get("lexHash"):
            lexer["filesChanged"] += 1
        oe, ne = o.get("errors", []), n.get("errors", [])
        parser["errorsBase"] += len(oe)
        parser["errorsCandidate"] += len(ne)
        if oe != ne:
            parser["filesErrorsChanged"] += 1
            left = collections.Counter(error_text(e) for e in oe)  # an error that only moved to another line is old
            added = []
            for e in ne:
                if left[error_text(e)] > 0:
                    left[error_text(e)] -= 1
                else:
                    added.append(e)
            if added:
                parser["filesWithNewErrors"].append({"file": f, "base": len(oe), "candidate": len(ne),
                                                     "added": added[:5]})
                items += [("parser", f, e) for e in added]
        if o.get("treeHash") != n.get("treeHash"):
            parser["filesTreeChanged"] += 1
        oexc = any(e.startswith("highlight") for e in o.get("exceptions", []))
        nexc = any(e.startswith("highlight") for e in n.get("exceptions", []))
        op, np_ = collections.Counter(o.get("problems", [])), collections.Counter(n.get("problems", []))
        if not oexc and not nexc:
            resolve["unresolvedBase"] += sum(v for k, v in op.items() if "|ErlangUnresolved" in k)
            resolve["unresolvedCandidate"] += sum(v for k, v in np_.items() if "|ErlangUnresolved" in k)
        if op != np_ and not oexc and not nexc:
            insp["filesChanged"] += 1
            for p in (op - np_).elements():
                insp["removed"][tool(p)] += 1
            real = []
            for p in (np_ - op).elements():
                if tool(p) in off:
                    insp["addedOffByDefault"] += 1
                else:
                    insp["added"][tool(p)] += 1
                    real.append(p)
            if real:
                insp["addedProblems"].append({"file": f, "added": real[:10]})
                items += [("inspections", f, p) for p in real]
            newly = [p for p in real if "|ErlangUnresolved" in p]
            if newly:
                resolve["filesWithNewUnresolved"].append({"file": f, "added": newly[:5]})
        if o.get("formatHash") != n.get("formatHash"):
            fmt["filesChanged"] += 1
        if n.get("formatTokensChanged") and not o.get("formatTokensChanged"):
            fmt["filesWithNewTokenChanges"].append(f)
            items.append(("formatter", f, "the formatter changes a token other than white space"))
        if o.get("formatTokensChanged") and not n.get("formatTokensChanged"):
            fmt["filesWithFixedTokenChanges"] += 1
        oe_, ne_ = o.get("exceptions", []), n.get("exceptions", [])
        if ne_ and not oe_:
            exc["new"].append({"file": f, "exceptions": [e[:300] for e in ne_]})
            items += [("exceptions", f, e[:300]) for e in ne_]
        if oe_ and not ne_:
            exc["fixed"].append(f)
        for k in ("lexUs", "parseUs", "formatUs", "highlightUs"):
            ov, nv = o.get(k), n.get(k)
            if ov is None or nv is None:
                continue
            metric = k[:-2]
            timing["totalsMs"][metric + "Base"] += ov / 1000
            timing["totalsMs"][metric + "Candidate"] += nv / 1000
            if (o.get("lines", 0) >= SLOWDOWN_MIN_LINES and ov > 0 and nv / ov > SLOWDOWN_RATIO
                    and (nv - ov) / 1000 > SLOWDOWN_MIN_MS):
                slow = {"file": f, "lines": o["lines"], "metric": metric, "baseMs": round(ov / 1000),
                        "candidateMs": round(nv / 1000), "ratio": round(nv / ov, 2)}
                timing["slowdowns"].append(slow)
                if metric in TIMING_METRICS:
                    items.append(("timing", f, slow))
    timing["slowdowns"].sort(key=lambda r: -(r["candidateMs"] - r["baseMs"]))
    timing["totalsMs"] = {k: round(v) for k, v in sorted(timing["totalsMs"].items())}
    insp["added"] = dict(insp["added"].most_common())
    insp["removed"] = dict(insp["removed"].most_common())
    for section in (parser["filesWithNewErrors"], insp["addedProblems"], resolve["filesWithNewUnresolved"], exc["new"],
                    timing["slowdowns"], fmt["filesWithNewTokenChanges"]):
        del section[CAP:]
    checks = {"lexer": lexer, "parser": parser, "inspections": insp, "resolve": resolve, "formatter": fmt,
              "exceptions": exc, "timing": timing}

    # the largest slowdowns first, so that they are the examples of the timing family
    items = [i for i in items if i[0] != "timing"] + sorted(
        (i for i in items if i[0] == "timing"), key=lambda i: -(i[2]["candidateMs"] - i[2]["baseMs"]))
    known_hits = [{"entry": i, "issue": e.get("issue"), "check": e["check"], "why": e.get("why", ""), "items": 0,
                   "files": []} for i, e in enumerate(known)]
    candidates, fams = collections.OrderedDict(), collections.OrderedDict()
    for check, f, item in items:
        hit = next((h for h, e in zip(known_hits, known) if covers(e, check, f, item)), None)
        if hit is not None:
            hit["items"] += 1
            if f not in hit["files"]:
                hit["files"].append(f)
            continue
        detail = ("%s %s ms -> %s ms" % (item["metric"], item["baseMs"], item["candidateMs"])
                  if check == "timing" else item)
        key, subject = family_of(check, item)
        candidates.setdefault((check, f), {"check": check, "file": f, "family": family_id(key), "detail": []})
        candidates[(check, f)]["detail"].append(detail)
        fam = fams.setdefault(key, {"id": family_id(key), "key": key, "check": check, "subject": subject,
                                    "area": area_of(check, item), "files": 0, "items": 0, "examples": [],
                                    "_files": set()})
        fam["items"] += 1
        if f not in fam["_files"]:
            fam["_files"].add(f)
            fam["files"] += 1
            if len(fam["examples"]) < EXAMPLES:
                fam["examples"].append({"file": f, "detail": detail})
    for h in known_hits:
        del h["files"][20:]
    families = []
    for fam in fams.values():
        del fam["_files"]
        if fam["check"] == "timing" and fam["files"] < MIN_TIMING_FILES:
            continue
        families.append(fam)
    families.sort(key=lambda x: (x["check"], -x["files"], x["key"]))
    cands = list(candidates.values())
    for x in cands:
        del x["detail"][5:]
    return files, checks, cands[:CAP], families, [h for h in known_hits if h["items"]]


def summary_md(rep):
    def cell(text):
        return str(text).replace("|", "\\|").replace("\n", " ").replace("<", "&lt;")[:200]

    ch = rep["checks"]
    lines = ["## Regression check", "",
             "Base %s, candidate %s: %d files compared%s." % (
                 cell(rep["baseRef"]) or "?", cell(rep["candidateRef"]) or "?", rep["files"],
                 "" if rep["complete"] else " (incomplete: the time budget ran out or a test JVM stopped; only the "
                                            "files that both sides did are compared)"), ""]
    lines += ["| Check | Changed | New candidates |", "|---|---|---|"]
    per = collections.Counter(c["check"] for c in rep["regressionCandidates"])
    lines.append("| Lexer (token stream) | %d files | - |" % ch["lexer"]["filesChanged"])
    lines.append("| Parser (errors: %d to %d) | %d files, %d trees | %d files |" % (
        ch["parser"]["errorsBase"], ch["parser"]["errorsCandidate"], ch["parser"]["filesErrorsChanged"],
        ch["parser"]["filesTreeChanged"], per["parser"]))
    lines.append("| Inspections | %d files | %d files |" % (ch["inspections"]["filesChanged"], per["inspections"]))
    lines.append("| Formatter | %d files | %d files |" % (ch["formatter"]["filesChanged"], per["formatter"]))
    lines.append("| Exceptions | %d fixed | %d files |" % (len(ch["exceptions"]["fixed"]), per["exceptions"]))
    lines.append("| Timing | %d slowdowns | %d files |" % (len(ch["timing"]["slowdowns"]), per["timing"]))
    t = ch["timing"]["totalsMs"]
    lines += ["", "Timing totals, base to candidate: " + ", ".join(
        "%s %.1f s to %.1f s" % (m, t.get(m + "Base", 0) / 1000.0, t.get(m + "Candidate", 0) / 1000.0)
        for m in ("lex", "parse", "format", "highlight") if m + "Base" in t) + ".", ""]
    if rep["families"]:
        lines += ["### New regression candidates", "", "| Family | Area | Files | Example |", "|---|---|---|---|"]
        for fam in rep["families"]:
            ex = fam["examples"][0] if fam["examples"] else {"file": "", "detail": ""}
            lines.append("| `%s` %s | %s | %d | %s: %s |" % (fam["id"], cell(fam["key"]), fam["area"], fam["files"],
                                                            cell(ex["file"]), cell(ex["detail"])))
    else:
        lines.append("No new regression candidates.")
    if rep["known"]:
        lines += ["", "### Covered by known.json", "", "| Issue | Check | Items | Files | Why |", "|---|---|---|---|---|"]
        for h in rep["known"]:
            lines.append("| %s | %s | %d | %d | %s |" % ("#%d" % h["issue"] if h.get("issue") else "accepted",
                                                         h["check"], h["items"], len(h["files"]), cell(h["why"])))
    return "\n".join(lines) + "\n"


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[1])
    ap.add_argument("base")
    ap.add_argument("cand")
    ap.add_argument("out")
    ap.add_argument("--known", default="")
    ap.add_argument("--summary", default="")
    ap.add_argument("--base-ref", default="")
    ap.add_argument("--cand-ref", default="")
    ap.add_argument("--shards", type=int, default=1, help="test JVMs per side; each writes one end line")
    a = ap.parse_args(argv)
    try:
        bm, b, bends = load(a.base)
        cm, c, cends = load(a.cand)
        known = load_known(a.known)
    except InputError as e:
        print("diffreport: %s" % e, file=sys.stderr)
        return 2
    off = set(cm["toolsOffByDefault"]) | set(bm["toolsOffByDefault"])
    files, checks, cands, families, hits = compare(b, c, off, known)
    rep = {"version": 1, "baseRef": a.base_ref, "candidateRef": a.cand_ref, "files": len(files),
           "complete": complete(bends, a.shards) and complete(cends, a.shards) and set(b) == set(c),
           "completeBase": complete(bends, a.shards), "completeCandidate": complete(cends, a.shards),
           "onlyInBase": len(set(b) - set(c)), "onlyInCandidate": len(set(c) - set(b)),
           "newInspections": sorted(set(cm["tools"]) - set(bm["tools"])), "inspectionsOffByDefault": sorted(off),
           "checks": checks, "regressionCandidates": cands, "families": families, "known": hits}
    with open(a.out, "w", encoding="utf-8") as f:
        json.dump(rep, f, indent=1)
    if a.summary:
        with open(a.summary, "w", encoding="utf-8") as f:
            f.write(summary_md(rep))
    print(json.dumps({"files": len(files), "complete": rep["complete"], "families": len(families),
                      "regressionCandidates": len(cands), "known": sum(h["items"] for h in hits),
                      "byCheck": dict(collections.Counter(x["check"] for x in cands)),
                      "timingTotalsMs": checks["timing"]["totalsMs"]}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
