# Robustness sweep

`ErlangRobustnessSweepTest` is tooling for the robustness hunt, not a regression test. It runs the plugin's IDE features over a corpus of Erlang files, seeded mutations of them and synthetic stress inputs, and records every exception, logged error, hang and consistency violation in a JSON report. It never fails on a finding.

**Without `SWEEP_CORPUS`, `SWEEP_SYNTH`, `SWEEP_SCALE` and `SWEEP_PLANT` the test does nothing and passes**, so `./gradlew test` is not affected. The scheduled runs are started by `.github/workflows/robustness-sweep.yml`; the choice of what to run next and the triage of the findings live outside this repository.

## Run
The knobs are environment variables; `--rerun` is needed because Gradle does not treat the environment as a test input.

```sh
SWEEP_CORPUS=/path/to/otp SWEEP_CORPUS_ID=OTP-29.1 SWEEP_CORPUS_SET=main \
SWEEP_SEED=1 SWEEP_SHARD=0/8 SWEEP_BUDGET_MINUTES=40 \
SWEEP_REPORT=build/sweep/report.json SWEEP_LEDGER=build/sweep/ledger.jsonl SWEEP_DUMP_DIR=build/sweep/dumps \
./gradlew test --rerun --tests 'org.intellij.erlang.sweep.ErlangRobustnessSweepTest'
```

The test JVM halts with exit code 3 after a hang and 4 after an `OutOfMemoryError`; run the same command again with `SWEEP_RESUME=1` to continue after the input that caused it.
`SWEEP_PLANT=1` runs the positive controls instead: faults planted through the platform's extension points, one per detector and per operation. That run fails if a required control does not fire.

## Knobs
| Variable | Default | Meaning |
|---|---|---|
| `SWEEP_CORPUS` | none | Directory with `.erl`/`.hrl` files: a checkout of `erlang/otp`, an installed Erlang/OTP, or a folder of snippets. |
| `SWEEP_CORPUS_ID` | directory name | Name of the corpus in the report, for example `OTP-29.1`. |
| `SWEEP_CORPUS_SET` | `all` | `main`: `lib/*/src`, `lib/*/include`, `lib/*/examples`, `erts/preloaded/src`. `test`: `lib/*/test`, `erts/emulator/test`, `erts/test`. `all`: every `.erl`/`.hrl`. `leexinc.hrl` (a leex template) is never an input. |
| `SWEEP_SDK_RELEASE` | from `SWEEP_CORPUS_ID`, else 29 | Mock SDK of the run: 27, 28 or 29, so the release-gated syntax matches the corpus. |
| `SWEEP_SEED` | `System.nanoTime()` | Seed of every random choice; it is in the report and in every failure. |
| `SWEEP_MODE` | `normal` | `normal`, `dumb` (operations run in dumb mode), `smallide` (PyCharm platform prefix). |
| `SWEEP_OPS` | all of the mode | Subset of `stubs,highlight,resolve,structure,folding,completion,usages,rename,format,reparse,typing,resolve-bg,structure-bg,folding-bg`. |
| `SWEEP_ORIGINALS` | `1` | `0` skips the unchanged files. |
| `SWEEP_MUTATIONS` | `0` | Mutations per file. |
| `SWEEP_MUTATION_KINDS` | all | Subset of `truncate,delete-token,dup-token,swap-tokens,garbage,bracket,unterminated-string,unterminated-atom`. |
| `SWEEP_SHARD` | `0/1` | `i/n`: the files whose CRC32 of the path is `i` modulo `n` (`lib/stdlib-7.1/` counts as `lib/stdlib/`). The order inside a shard depends on the seed. |
| `SWEEP_LIMIT` | `0` | First N files of the shard, for smoke runs. |
| `SWEEP_ONLY` | none | Comma-separated substrings of the input id (`path#orig`, `path#m0`, `synthetic/name`, `scale/name`). |
| `SWEEP_SYNTH` | none | `1` adds the synthetic inputs, `only` runs just them (see Synthetic inputs). |
| `SWEEP_SCALE`, `SWEEP_SCALE_RATIO` | none, `8` | The scaling probe: each generator at n and 4n; a time ratio over the limit is a finding. |
| `SWEEP_PLANT` | none | `1`: the positive controls. `hang`: the hang control too (use `SWEEP_HANG_SECONDS=30`). |
| `SWEEP_BUDGET_MINUTES` | `0` | Stop before the next input when the time is over; the report has `complete: false`. |
| `SWEEP_SLOW_MS`, `SWEEP_HANG_SECONDS`, `SWEEP_OP_BUDGET_MS` | `10000`, `300` (`60` with `SWEEP_SYNTH=only`), `30000` | Slow-operation threshold; hang threshold; time limit of the resolve loop. |
| `SWEEP_INPUT_BUDGET_SECONDS` | `120` | After this much time on one input, its next operations are skipped (property `input-capped`); `0` is no limit. |
| `SWEEP_COMPLETIONS`, `SWEEP_PICKS` | `3`, `3` | Completion offsets per input; named elements per kind for find usages and rename. |
| `SWEEP_REPORT`, `SWEEP_JOURNAL` | `build/sweep/report.json`, report + `.journal.jsonl` | Outputs. The journal is written line by line, so it survives a halt. |
| `SWEEP_LEDGER` | none | A JSONL file; one record of what the run covered is appended. |
| `SWEEP_KNOWN` | none | A file of `signature<TAB>issue` lines; a failure with a listed signature is marked with it instead of `new`. |
| `SWEEP_DUMP_DIR` | none | The first two failing mutated or synthetic inputs of each signature are written here. |
| `SWEEP_RESUME` | none | Continue from the journal. |
| `SWEEP_REVISION` | `GITHUB_SHA`, else `git rev-parse HEAD` | Plugin revision in the report. |
| `SWEEP_MINIMIZE`, `SWEEP_MIN_OUT` | none, `build/sweep/min` | Reduce every file of `SWEEP_CORPUS` while some failure signature still contains the given text. |

## Synthetic inputs
In this order: the fixed small inputs of the runner (empty files, BOMs, line ends, cycles of includes, records, types and macros); the generated inputs of `SweepGenerated`; every prefix of a small module (a typing session); the large stress inputs; with `SWEEP_SCALE`, the scaling probe last. A run that stops on its time budget cuts the slow ones first.
The ids of the generated inputs are `synthetic/<family>-<name>`:
- the same in every run: `deep-*` (each construct nested 100 deep), `long-*` (long lines and terms, huge binaries, maps and numbers), `unicode-*` (odd Unicode and invalid UTF-8 in atoms, strings, comments and file names), `odd-*` (records and macros in unusual places), `inc-*` (includes with broken or odd header files), `maybe-*` (`maybe`/`else` and `-feature`), `sigil-*` (sigils and triple-quoted strings), `attr-*` (broken attributes);
- from the seed, so each run has new ones: `cut-*` (a construct of each grammar rule cut at a token boundary), `soup-*` (random tokens of the lexer's token set), `gen-*` (random, mostly valid modules).

## What runs
Each input is written into the light project as bytes and opened in the editor. Every operation runs on its own, from the original text, and its failure does not stop the others: stub/text consistency, highlighting with every inspection of the plugin (the Dialyzer one excepted), resolve of every reference, structure view, folding, completion at seeded offsets, find usages and rename of seeded named elements, reformat twice, reparse after seeded edits, a small function typed char by char. `resolve-bg`, `structure-bg` and `folding-bg` repeat three of them on a pooled thread in a read action.
Detectors: an exception of the operation; every `LOG.error` during it, from any thread; a hang (the JVM is halted); wall time; the scaling probe; the properties of the formatter (the code is unchanged, the second reformat changes nothing).
A failure has a signature, `<kind> @ <first plugin frame>`, that does not depend on the line number or on the operation. Without a plugin frame the signature has the first differing stub (stub mismatches) or the first platform frame and the operation.

## What it does not prove
The report lists them as `unproven_detectors`:
- a missing read action in plugin code: the harness supplies the read access of every operation;
- the slow-operation assertion on the EDT, tree access, the PSI/text check, the `OutOfMemoryError` halt: no control fires for them;
- in dumb mode an `IndexNotReadyException` inside a folding builder or a completion contributor reaches no detector (the ledger record has `credit: false` for these operations).
It opens one file at a time, so cross-module resolve and most `-include`s are not exercised. Quick fixes, intentions, documentation, run configurations, the debugger, rebar and JPS are not covered.
