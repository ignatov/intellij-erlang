# Property-based tests against erlc

`ErlangPropertyTest` is tooling, not a regression test. It generates random Erlang modules, asks erlc about each one and checks that the plugin agrees. On a disagreement it shrinks the module to a small one with the same failure and records it in a JSON report. It never fails on a finding; only the run of the controls (`PBT_PLANT`) can fail.

**Without `PBT_ERL` the test does nothing and passes**, so `./gradlew test` is not affected.

## Run
`--rerun` is needed because Gradle does not treat the environment as a test input.

```sh
PBT_ERL=$(command -v erl) PBT_SEED=1 PBT_CASES=500 \
./gradlew test --rerun --tests 'org.intellij.erlang.pbt.ErlangPropertyTest'
```

Rerun one case exactly, with the module, the erlc answer and the plugin answer printed to the test output:

```sh
PBT_ERL=$(command -v erl) PBT_REPLAY=1:42 ./gradlew test --rerun --tests 'org.intellij.erlang.pbt.ErlangPropertyTest'
```

The controls: `PBT_PLANT=1` runs them instead of the cases, and the run fails if one of them does not behave.

## Knobs
| Variable | Default | Meaning |
|---|---|---|
| `PBT_ERL` | none | Path to `erl`. Without it the test does nothing. |
| `PBT_SEED` | `System.nanoTime()` | Seed of the generator. `(seed, index)` gives the same module every time. |
| `PBT_CASES` | `200` | Number of generated modules. |
| `PBT_INDICES` | none | Run only these case indices of the seed instead of `0..PBT_CASES-1`: `3,10-12`, or `@file` with such a list, for example the cases of the signatures of an earlier report. |
| `PBT_BUDGET_MINUTES` | `0` (none) | Stop before the next case when the time is over; the report has `complete: false`. |
| `PBT_PROPS` | all | Subset of `parse,parse-reject,warnings`. |
| `PBT_REPORT` | `build/pbt/report.json` | The report. |
| `PBT_JOURNAL` | report + `.journal.jsonl` | One JSON line per case, discard, violation, shrink and crash, written as they happen (a `violation` before its shrinking, a `shrink` after it), so it survives a crash. |
| `PBT_OUT` | `build/pbt/out` | The first two shrunk inputs of each signature, `<prop>-<hash>-<n>.erl`, with the signature and the case in a comment. |
| `PBT_WORK` | `build/pbt/work` | Where the file for erlc (`pbt.erl`) and the SDK of the run are written. |
| `PBT_REPLAY` | none | `<seed>:<index>`: run exactly that one case, verbose. |
| `PBT_SDK_RELEASE` | the OTP release of `PBT_ERL` | Release of the mock SDK: 25, 27, 28 or 29; anything else gets 29. |
| `PBT_SDK_HOME` | see below | Home of the SDK of the run. |
| `PBT_FILES` | none | A `.erl` file or a directory of them: check these instead of generated cases, verbose, for triage. The module must be `pbt`. |
| `PBT_PLANT` | none | `1`: run the controls instead of the cases. |
| `PBT_SHRINK_TESTS` | `400` | Most candidates the shrinker tries for one violation. |
| `PBT_GEN_OFF` | none | Constructs of the generator (comma-separated names of `generator.constructs`, and the forms `form.import`, `form.behaviour`, `form.callback`, `form.on_load`, `form.nominal`, `form.spec_variants`) that it never makes. Off costs no random number, so `PBT_GEN_OFF` with the constructs of round 1 makes the modules of generator v0 again, for example to replay a case of run r0. |
| `PBT_KNOWN` | none | A file of `signature<TAB>label` lines (blank lines and `#` comments are skipped). A violation with a listed signature is counted and marked with the label, not shrunk, and listed apart in the report, so that the new signatures stand out. A `*` in a signature matches any text; an exact line wins over a pattern, and patterns are tried in the order of the file. |
| `PBT_REVISION` | `GITHUB_SHA`, else `git rev-parse HEAD` | Plugin revision in the report. |

The SDK: from OTP R16 on the plugin resolves the auto-imported BIFs (`self/0`, `length/1`) only to the `erlang.erl` of the SDK, and `testData/mockSdk-R29` has none. So the run copies `lib/erts-*/src/erlang.erl` of the `PBT_ERL` installation, into `PBT_WORK/sdk-R<release>/`, writes there stubs `lists.erl` and `maps.erl` with the functions that the generator calls remotely and imports (stubs, so that indexing them adds nothing to the coverage of the parser), and uses that directory as the home of the mock SDK. The report has the home in `run.sdk_home`. Without the file, the bare mock SDK is used and every BIF call is an unresolved function.

## The oracle
`testData/pbt/pbt_oracle.erl` runs in one long-lived `erl -noshell` process per run; it is compiled from source at the start. It reads one file path per line and writes one JSON line per file: every error and warning of `compile:file/2` with `[return_errors, return_warnings, binary, strong_validation]`, each with the module that reports it, line, column, tag (the first element of the descriptor, for example `unused_var`), name (a variable, a record or `Name/Arity`) and the formatted message. Errors of `erl_scan`, `erl_parse` and `epp` are syntax errors. The process is started again only when it dies or gives no answer in 60 s (`run.oracle_restarts`). It needs the `json` module (OTP 27 or newer).

## Properties
- `parse`: erlc has no syntax error, so the plugin's PSI has no `PsiErrorElement`. Signature: `parse @ <PSI parent of the error> : <error text>`.
- `parse-reject` (the reverse, weaker, reported separately): each case also makes one mutant (a token deleted, duplicated, swapped or inserted). When erlc has a syntax error for it, the plugin must have a `PsiErrorElement` or an error highlight of some inspection. Signature: `parse-reject @ <erlc module> : <erlc message> @ <PSI parent of the plugin's token at the place of the erlc error>`, for example `parse-reject @ erl_parse : syntax error before: ')' @ ParenthesizedExpression`. A mutant that erlc accepts is checked by `parse` (`from_mutants`).
- `warnings`: only for generated modules without a syntax error in erlc and in the plugin. The erlc items `unused_var`, `unused_function`, `unused_type`, `unbound_var` and `undefined_function` are compared with the highlights of `ErlangUnusedVariableInspection`, `ErlangUnusedFunctionInspection`, `ErlangUnusedTypeInspection`, `ErlangUnboundVariableInspection`, `ErlangUnresolvedFunctionInspection` and `ErlangUnresolvedExportFunctionInspection` (erlc reports an undefined function in `-export` as `undefined_function` too). A type has no arity in the key because the plugin's message has none. The key is (kind, line, column, name); the column only for variables, because erlc and the plugin put a warning about a function or a type at different columns. Warnings with the same key pair first; then the rest of one kind, line and name pair one to one at any column (erlc puts an unused name of a named fun at `fun`, the plugin at the name). erlc reports an unbound variable at its first use only, so the plugin's other unbound warnings of that name are not extra. Both directions are violations: `warnings:extra <kind> @ <place><relation>` (the plugin only, a false positive) and `warnings:missing <kind> @ <place><relation>` (erlc only, a false negative).
  - The place is the innermost scope (case, if, receive, try, maybe, catch, fun, comprehension, begin, function clause, type, spec, record definition, attribute) with the part of it: `head` (the patterns of a clause), `name` (of a named fun), `guard`, `body`, `subject` (of a case), `timeout` (of an after), `gen-pattern`, `gen-expr`, `filter` and `template` (of a comprehension); a try has `body`, `of-head`, `catch-head`, `catch-body`, `after` and so on. A pattern inside `P = Q` in a head gets `-alias`, the left side of a match in a body `-match`. The innermost map, record or binary between them follows: `FunctionClause/head in Binary`.
  - The relation, for variables only, says what the plugin thinks of the variable: for `missing unused_var` what it resolves to (`resolves to <place>`), else its first use (`used at <place>`), else `no use`; for `extra unused_var` what the next occurrence of the name resolves to (`next resolves to <place>`, `next resolves to it`, `next unresolved`, `no next`); for `extra unbound_var` the first occurrence of the name in the function clause (`first at <place>`, `no earlier`); for `missing unbound_var` what it resolves to. Example: `warnings:missing unused_var @ FunctionClause/head (used at ListComprehension/gen-pattern)`: erlc says the argument is unused, and the plugin takes the pattern of a generator, which binds a new variable, for a use of it.
  - erlc 29 reports no unused variable at all in a function with a `maybe` inside a clause of a case, if, receive or try. In such a function the signature of an `extra unused_var` ends with ` [erlc: maybe in a clause]`, so that this erlc behaviour has signatures of its own.

Names, numbers and strings in error texts are replaced by `<var>`, `<atom>`, `<number>`, `<string>`, so one bug has one signature.

## Generator v0
`PbtGen` builds a `PbtNode` tree that prints to a module `pbt`: `-export`, `-export_type`, records, types and opaques, specs, functions with several clauses, patterns, guards, case, if, receive, try/catch/after, catch, begin, funs and named funs, list, binary and map comprehensions, maps, records, binaries, operators, BIF and local calls. No macros but `?MODULE`. It is valid by construction (every variable name is new in the module; variables bound inside a case, receive, try, fun or comprehension are not used after it) and it makes signal for `warnings` on purpose: unused variables, functions and types, `UnboundN` variables and calls of `missing_N` functions.

Round 1 added, each kept because it raised the coverage of a short run (300 cases, seed 2) over generator v0: `maybe ... else ... end` with `?=` (OTP 27 and later enable it by default); remote calls and remote fun references (`erlang:`, `?MODULE:`, `pbt:`, `lists:`, `maps:`); `-import(lists, [...])` and calls of the imported functions; `-behaviour`, `-callback` and `-optional_callbacks`; `-on_load`; types: record types with field types, integer type expressions, remote types, `fun()` types, `-nominal`, and specs with several clauses, `when` constraints, annotated arguments or parentheses; binaries with bit type lists (`/little-signed-integer`, `-unit:8`) in expressions and patterns. Sigils and triple-quoted strings were tried and dropped: no gain. `PBT_GEN_OFF` with all of them makes the modules of generator v0.

To add a construct, add one entry to the lists in the constructor of `PbtGen` (expressions, patterns, guards or types): a name, a weight, whether it nests and the code that builds it. The report counts every construct in `generator.constructs` and lists the ones never generated in `generator.never_generated`.

A generated module that erlc rejects with a syntax error is a bug of the generator: it is counted in `discards` (with the first five texts) and not checked.

## Shrinking
A violation is shrunk for the first two cases of each signature. The tree is reduced greedily: remove chunks of sequence items (forms, clauses, statements, arguments of tuples and lists), replace an expression by one of its subexpressions or by `ok`, a pattern by `_`, a guard test by `true`, a type by one of its parts or by `any()`. Then the text is reduced as below. A candidate is kept only when erlc still accepts it the same way (no syntax error, no erlc error tag outside the five kinds that the original has not, and no `_` used as an expression) and the property still fails with the same signature. A mutant, or a file of `PBT_FILES`, is shrunk on its text only, with three times the tests.

The text reduction: chunks of lines, then rounds of a structured reduction and of chunks of tokens until a round removes nothing. The structured reduction removes whole forms, bracketed groups and `case`/`fun`/`begin`/... `end` blocks (with a `,` or `;` next to them, or alone), items between the separators of a group, and the inside of a group, and replaces a group by `ok`, largest first. For `parse-reject` a candidate must keep the signature (the erlc message and the plugin's PSI at its place) and must not have more erlc syntax errors than the mutant, so that no error in another form takes the place of the one that was found.

## Controls
`PBT_PLANT=1`:
- negative: each module of `testData/pbt/controls/` must give no violation. A `%% agree: N` line asks that erlc and the plugin agree on at least N warnings, so that the control cannot pass by both saying nothing.
- positive, one per property: a planted fault must give a violation within 40 cases and shrink to at most 8 non-blank lines. `parse`: a fake plugin error at every `receive`. `parse-reject`: a fake plugin without errors. `warnings`: a fake erlc that never reports an unused variable.

## Report
`run`: seed, cases asked and run, `complete`, revision, OTP release, SDK release and home, elapsed time, the time and calls of erlc and of the plugin. `discards`. `properties.<prop>`: `checked`, `from_mutants`, `skipped` (`warnings` only: the plugin had a syntax error), `violations` (`known_violations` and `new_violations` of them), and the signatures with count, first case, the cases (up to 10), the shrunk texts and files: the new ones in `signatures`, the known ones of `PBT_KNOWN` with their label in `known_signatures`. `crashes`: exceptions and logged errors of the plugin during the checks. `generator`: the construct counts. `controls` with `PBT_PLANT`.

## What it does not check
One file at a time: no includes and no other modules; remote calls go only to `erlang`, to the module itself and to the stubs of `lists` and `maps`. No macros but `?MODULE`. For functions and types the key has no column: two such warnings of one kind and name on one line are one key. erlc's `unused_function` and `unused_type` follow reachability from the exports (a function called only by an unused one is unused too); the plugin looks for any reference, so those differences are expected in the report.
