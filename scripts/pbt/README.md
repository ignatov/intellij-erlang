# coverage_gaps.py

Tells which parser rules of `grammars/erlang.bnf`, and which methods of chosen plugin classes, a test run never
reached. It reads the JaCoCo XML report of the run. Python 3, standard library only. The workflow
`property-tests.yml` runs it after a run with coverage; the generator of the property tests
(`tests/org/intellij/erlang/pbt/`) grows towards the gaps it lists.

## Run

From the root of the repository, with JDK 21:

```sh
./gradlew --no-daemon clean test --rerun --tests 'org.intellij.erlang.parser.ErlangParserTest' -Pcoverage
python3 scripts/pbt/coverage_gaps.py build/reports/jacoco/coverageReport/coverageReport.xml \
  --bnf grammars/erlang.bnf --json build/coverage.json
```

Add `--diff old-coverage.json` (the JSON of an earlier run) to list what changed since that run.

- Use `clean` after any change in `src/` or `gen/` since the last coverage build. Without it the incremental
  `instrumentCode` gives wrong counts. A change only in `tests/` or `testData/` keeps the counts right.
- Use `--rerun` when the run depends on environment variables (`PBT_SEED`, `CORPUS_DIR`): they are not inputs of the
  Gradle test task, so without it Gradle can skip the test or take its result from the build cache.
- The report must come from the same commit as the `gen/` it is read with. The tool warns when the method lines of
  the report do not fit `ErlangParser.java`. `--diff` warns when the size of the parser or of the whole report
  changed between the two runs: other code, or a run without `clean`.

Options: `--parser-java` (default: `gen/.../ErlangParser.java` next to the `grammars/` directory), `--extra PREFIX`
(repeatable, replaces the default list of classes), `--all` (also list reached rules and fully covered methods),
`--top N`, `--other-contexts`, `--json FILE` (`-` for stdout instead of the text). The text goes to stdout.

Unit tests: `python3 -m unittest discover -s scripts/pbt -p 'test_*.py' -v`.

## Output

1. Totals: rules reached, partly reached and not reached, the same for the rules used in `.erl` files, and the
   branch and instruction coverage of `ErlangParser` (also the branch coverage without the `recursion_guard_` lines,
   whose `return false` never runs).
2. With `--diff`: rules whose verdict changed, the change of the totals, methods of the extra classes whose counts
   changed.
3. Top gaps: the rules not reached. First the rules used in `.erl` files, then the rules that are not inside another
   gap, then by "blocks" (how many rules not reached can be reached from `forms` only through this rule; the count
   includes the rule itself), then bnf order. `inside X`: every path from `forms` to the rule passes through the gap
   `X`.
4. The rules not reached, with the reason; the rules partly reached, with each part that never ran and its line `L`
   in `ErlangParser.java`; the rules with no code of their own (the `fake` rules, and private rules that
   Grammar-Kit inlined into their users).
5. For each extra class: line and branch counts per method; methods with zero coverage are marked `ZERO`.

`bnf:N` is the line of the rule in `grammars/erlang.bnf`. The default extra classes are
`ErlangUnusedVariableInspection`, `ErlangUnusedFunctionInspection`, `ErlangUnusedTypeInspection`,
`ErlangUnboundVariableInspection`, `ErlangUnresolvedFunctionInspection` (matched on the simple class name, so inner
classes and quick fixes are included) and `org/intellij/erlang/parser/_ErlangLexer`.

## How a rule gets its verdict

Grammar-Kit makes one method per rule and helper methods for its parts (`type_1`, `type_1_1_0`, ...). The tool folds
each helper into the rule whose name is the longest prefix followed only by `_<digits>` groups. The binary and
postfix operators of `expression` have no method; they are blocks of `expression_0`, found by the element type in
their `exit_section_` line. A `static final Parser` lambda goes to the rule it calls.

JaCoCo counts code that ran, not input that matched. The tool reads "matched" from the lines:

- **not reached**: no code of the rule ran ("never called"), or the rule was called but never got past its first
  element. For a sequence: the call of the second element never ran. For alternatives: none matched. For a loop:
  the line after the call never ran. For an operator block of `expression_0`: its body never ran.
- **partly**: the rule matched at least once, but a part never ran: a helper method that never ran
  (`never-called`), a call that never happened because the elements before it always failed or an earlier
  alternative always matched (`never-tried`), or a token alternative that never matched (`never-matched`, known only
  for the alternatives before the last one).
- **reached**: every part ran.

Error paths (`r || p`, `report_error_`) are not counted as parts. How a call is judged from the counts of one line:
for `if (!r) r = X(...)` and the operator checks of `expression_0`, the call ran when the line has no missed
instruction. For `r = r && X(...)` and `r = p && X(...) && r`, a call that never ran leaves at least 6 instructions
missed, one that ran at most 4, so the limit is 5. Other lines ran when they have a covered instruction.

`forms` also parses `.app` and `.config` files and the console. Rules reachable from `forms` only through
`--other-contexts` (by default `application_file_expression`, `console_expression`, `config_expression`) are marked
`[not in .erl files]` and come last in the top gaps. Rules that no other rule uses are marked
`[not used from forms]`.

## Limits

- A rule made only of token alternatives (`'+' | '-'`) is "matched" when the line after an alternative saw `r` true,
  or when its caller used the result (the line after the call ran). Executed token checks alone are not evidence.
  Caller evidence is not read where the next line runs anyway: after a pin (`r = p && ...`), at the end of a
  method, or after an alternative that follows a successful one.
- A rule that only passes on the result of another rule (`r = exprs(...)`) has no branch of its own. It is matched
  when that rule matched and a caller used the result. When no call site can show the result, the rule is counted
  as reached, marked `match_unknown` in the JSON and listed under "NOT KNOWN WHETHER THEY MATCHED" (8 rules with
  today's grammar, `try_expressions_clause` among them): the other rule may have matched only for another caller.
- "Reached" means that the code ran, and for most rules that the rule matched at least once. It does not mean that
  every token alternative matched.
- The judgement from line counts depends on the code shapes of today's Grammar-Kit. After a Grammar-Kit update,
  check a few rules against the HTML report again.
- Coverage of one test JVM only.
- The rule graph comes from the names in each rule body. It does not know predicates (`&`, `!`) or the external
  `<<...>>` checks.
- The extra classes are reported per method only.
