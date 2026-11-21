# AGENTS.md

This is the IntelliJ Platform plugin for Erlang: lexer, parser, PSI, stubs, inspections, completion, run/debug, rebar and eunit.
Plugin id `org.jetbrains.erlang`. One archive serves IntelliJ IDEA 2026.2 and 2026.3 (platform branches 262 and 263). It is built against the older of them, 2026.2, with Java 21.

## Layout
- `src/`: the plugin sources (`org.intellij.erlang.*`).
- `gen/`: the generated lexer, parser, `ErlangTypes` and PSI. Never edit it by hand; see Generated code.
- `grammars/erlang.bnf`: the Grammar-Kit grammar. `src/org/intellij/erlang/parser/Erlang.flex`: the JFlex lexer.
- `tests/`: the tests. `testData/<area>/`: their fixtures. `testData/mockSdk-*`: the mock Erlang SDKs.
- `resources/META-INF/ErlangPlugin.xml`: every extension registration. `plugin/resources/META-INF/plugin.xml`: the id, the dependencies and `<change-notes>`; it `xi:include`s `ErlangPlugin.xml`.
- `jps-plugin/`: the external build (JPS) plugin, loaded by the compile server. It is compiled to Java 11 bytecode, with its tests in `jps-plugin/tests/`.
- `grammar-generator/`: a Gradle module that only hosts the runner of the parser generator. It is not part of the plugin.
- `libs/OtpErlang.jar`: a vendored library. `resources/debugger/beams/*`: committed `.beam` files built from `resources/debugger/src`.

## Commands
Use JDK 21 (CI uses Temurin 21) and the Gradle wrapper. Building or testing against 2026.3 (`-PplatformVersion=2026.3`) needs JDK 25.
The first build downloads IntelliJ IDEA 2026.2 (several GB) from JetBrains hosts. Running even one test class takes minutes.

| Purpose | Command |
|---|---|
| Same as CI | `./gradlew --no-daemon clean test buildPlugin verifyPlugin` |
| All tests (mock SDKs, no Erlang needed) | `./gradlew test` |
| One class | `./gradlew test --tests 'org.intellij.erlang.parser.ErlangAppParserTest'` |
| One method | `./gradlew test --tests 'org.intellij.erlang.parser.ErlangAppParserTest.testRtbCoordinator'` |
| Plugin Verifier | `./gradlew verifyPlugin` |
| Compilation tests (need Erlang/OTP and rebar3) | `./gradlew testCompilation -Derlang.sdk.path="$(erl -noshell -eval 'io:put_chars(code:root_dir()), halt().')" -Drebar.path="$(command -v rebar3)"` |
| JPS Java 11 check | `./gradlew :jps-plugin:verifyJpsJava11Compatibility` |
| Regenerate `gen/` | `./gradlew generateLexerAndParser` |
| Check that `gen/` is up to date (CI runs it) | `./gradlew verifyGeneratedSources` |
| Build, test and verify against another IDE | `./gradlew test buildPlugin verifyPlugin -PplatformVersion=263.7307.44` (or `2026.3.1`, `263-EAP-SNAPSHOT`) |
| Parse every `.erl`/`.hrl` under a directory (corpus check) | `CORPUS_DIR=/usr/local/lib/erlang CORPUS_REPORT=/tmp/corpus.json ./gradlew test --tests 'org.intellij.erlang.parser.ErlangCorpusParsingTest' --rerun` |
| Unit tests of the compatibility check scripts | `python3 -m unittest discover -s scripts/compat -p 'test_*.py' -v` |
| Compatibility check on GitHub | `gh workflow run compat.yml -f ide=263.7307.44 -f otp=28.0` |
| Release smoke, machine half (CI job `release-smoke`, JDK 25) | `scripts/smoke/run.sh` (phases and settings in its header) |
| Unit tests of the release smoke scripts | `python3 -m unittest discover -s scripts/smoke -p 'test_*.py' -v` |
| Refresh the parser baseline of the release smoke | `scripts/smoke/run.sh inputs parser-baseline` |
| Refresh the expected inspection problems of the release smoke | `scripts/smoke/run.sh inputs build inspect-baseline` |

- `test` excludes the `org/intellij/erlang/build/**` and `org/intellij/erlang/jps/**` packages. Those tests run only in `testCompilation`.
- The CI job `compilation-tests` runs `testCompilation` on `ubuntu-24.04` with the pinned Erlang/OTP 27 from builds.hex.pm and the pinned rebar3 escript (versions and sha256 are in `gradle.yml`, the same as in `copilot-setup-steps.yml`). It fails if any test was skipped.
- The rebar tests skip themselves when `rebar.path` is unset. The Copilot setup steps put `rebar3` on `PATH`, so `command -v rebar3` finds it.
- `verifyPlugin` checks only against the IDE that is built against. It fails on compatibility problems, missing dependencies, an invalid plugin and override-only API usages.
- The compatibility check, `.github/workflows/compat.yml` with `scripts/compat/`, answers whether the plugin works with one IDE build (job `ide`: compile, `test`, `verifyPlugin` with `-PplatformVersion`, and the Plugin Verifier on the archive of the published release, `v<version>.<N>` with the largest N) and one Erlang/OTP release (job `otp`: `testCompilation` and `ErlangCorpusParsingTest` on that installation, and whether `ErlangSdkRelease` and `testData/mockSdk-R<NN>` know the release).
- `ref` is empty (the head of the default branch), the tag `v<digits and dots>` of a published GitHub release (resolved as exactly `refs/tags/<name>`), or the full SHA of a commit of the default branch; branches and tags without a release are refused. Only commits of the default branch from the merge of the compatibility check on, and release tags made after it, have what the checks need (`platformVersion` in `build.gradle`, `ErlangCorpusParsingTest`); with an older commit the checks it cannot run are `not-run` with the reason "the checked commit predates the compatibility check" and the result is `incomplete`. On `pull_request` the plugin is the merge commit that the scripts come from.
- Each job uploads one JSON result (`compat-ide`, `compat-otp`); a job that was not asked for is `skipped` and uploads none. A run without the result failed for infrastructure reasons (the IDE or Erlang/OTP could not be set up). A result that is `incomplete` (a check did not run or did not work: no test ran, a missing Gradle task, no report or an unreadable one, a step that timed out or was cancelled, a commit that predates the check) makes the job red on every event. Findings of checks that ran make the job red on `workflow_dispatch` only; on `pull_request` it is green. `ErlangCorpusParsingTest.testCorpus` does nothing without `CORPUS_DIR`.

## Generated code
- Grammar-Kit generates `gen/org/intellij/erlang/{ErlangTypes.java,parser/ErlangParser.java,psi/**}` from `grammars/erlang.bnf`.
- JFlex 1.9.2, the IntelliJ-patched one that Grammar-Kit bundles, generates `gen/org/intellij/erlang/parser/_ErlangLexer.java` from `Erlang.flex`.
- `./gradlew generateLexerAndParser` regenerates the lexer, parser, `ErlangTypes` and PSI in place from `Erlang.flex` and `grammars/erlang.bnf`. It reads `psiImplUtilClass`, the mixins and the stub classes from the sources, as "Generate Parser Code" does in the IDE, and does not need the plugin to compile.
- `./gradlew verifyGeneratedSources` generates them into the build directories and fails if the committed `gen/` differs. CI runs it.
- After deleting `gen/`, or when a generated signature depends on a PSI type that does not exist yet, run `generateLexerAndParser` a second time and check that the second run changes nothing.
- Never hand-edit `gen/`. A grammar or lexer change ships in the same commit as the regenerated `gen/` and the updated `testData/parser/*.txt` golden files. If you cannot run the generator, do not change the grammar; stop and say so.
- Handwritten PSI logic goes in `ErlangPsiImplUtil` (the bnf `psiImplUtilClass`) or in mixins under `src/`. Stubbed rules are wired through `stubClass` and `ErlangElementTypeFactory` in the bnf.

### Parser and lexer fixes
You may change `grammars/erlang.bnf` and `Erlang.flex` to fix a parser or lexer bug: valid Erlang that the plugin marks as a syntax error, or a wrong PSI tree. `erlc` decides what is valid Erlang.
- Start from a minimal module that `erlc` compiles and the plugin's parser gets wrong. Add it as a parser test (`testData/parser/X.erl` with its PSI dump `X.txt`) and see the test fail before you change the grammar or the lexer.
- Regenerate with `./gradlew generateLexerAndParser` and commit `gen/` together with the grammar. `./gradlew verifyGeneratedSources` must pass.
- An existing `testData/parser/*.txt` dump may change only where the new tree is the intended one. List every changed dump in the PR description and say why its new tree is right.
- Syntax that exists only from some OTP release stays gated on `ErlangSdkRelease` (see Platform rules). Test it with the matching mock SDK.
- If the change turns the same text into different stubbed elements, bump the stub version as Stubs and indices says (append an entry to `ErlangStubChanges`), and name the bump in the PR description.
- Run `./gradlew test`. Resolve, highlighting, completion and the formatter all read the tree the parser builds, so a grammar change can break any of them.
- Still stop and ask for: the header attributes of the bnf (class names, `psiImplUtilClass`, `generate`), renaming or removing a rule or a token that `src/` uses, a new or changed `stubClass`, and anything in `grammar-generator/`.

## Stubs and indices
- The stub version of `.erl` files, returned by `getStubVersion()` of `stubs/types/ErlangFileElementType`, is the number of entries of `ENTRIES` in `src/org/intellij/erlang/stubs/ErlangStubChanges.java`. Bump it by appending one entry at the end, in the same change, when either of these changes:
  - the bytes written per stub: `serialize`, `deserialize` or `createStub` of a type in `stubs/types/`;
  - which elements get stubs: `shouldCreateStub`, `stubClass` in the bnf, or a parser change that turns the same text into different stubbed elements. b857bcb7 bumped it after the OTP 28/29 grammar changes.
- An entry is the issue number and a short phrase, on its own line ending with a comma: `"#1145 macros and integer expressions as binary type sizes",`. Never edit, reorder or remove an entry, and never write the version as a number.
- Two pull requests that each append an entry conflict on purpose; with a version number, two that both changed `7` to `8` merged cleanly and one bump was lost. Resolve the conflict by keeping both entries, master's first. The `build` job fails a pull request whose merge does not keep the entries of master as a prefix (`scripts/stubs/stub_changes.py`).
- Stub indices are `stubs/index/ErlangAllNameIndex` and `ErlangBehaviourModuleIndex` (their `VERSION`). File-based indices are `index/ErlangModuleIndex`, `ErlangApplicationIndex` and `ErlangAtomIndex` (`INDEX_VERSION`). Bump an index version when its keys, values or externalizers change.
- Code that reads the stub (for example `ErlangFileImpl`, which tries the stub first and falls back to PSI) must give the same answer as the AST path.

## Tests
- Tests are JUnit 3 style: `TestCase` subclasses with `public void testXxx()` methods, run by the JUnit vintage engine. JUnit 4 is used only for `@Ignore` and for `Assume`, which skips a test when a tool is missing. A `_testXxx` method is disabled. Follow that style.
- Base classes:
  - `utils/ErlangLightPlatformCodeInsightFixtureTestCase`, which extends `BasePlatformTestCase`. `isSmallIde=true` runs with the PyCharm platform prefix; `setUpProjectSdk()` installs the descriptor's SDK.
  - `parser/ErlangParserTestBase`, which extends `ParsingTestCase`.
  - Per area: `ErlangHighlightingTestBase`, `ErlangCompletionTestBase`, `ErlangQuickFixTestBase`, `ErlangIncludeResolveTestBase`.
  - Heavy, with a real Erlang: `build/ErlangCompilationTestBase` (the external build process) and `jps-plugin/tests/.../jps/ErlangJpsBuildTestCase` (JPS).
- `getTestDataPath()` points at `testData/<area>/`. The expected results are files:
  - parser: `X.erl` with the PSI dump in `X.txt`;
  - formatter, quick fixes, intentions and templates: `X.erl` and `X-after.erl`;
  - markup: `<caret>`, `<selection>`, `<error descr="...">`, `<warning descr="...">`.
- A missing parser `.txt` is written on the first run and the test fails. Read the dump before keeping it.
- Mock SDKs come from `ErlangSdkType.createMockSdk(home, ErlangSdkRelease)`. `testData/mockSdk-R15B02` is the default. `mockSdk-R16B` through `mockSdk-R29` exist for release-gated syntax, tested by `ErlangNNHighlightingTest` and its `SmallIde` variant.
- Only two classes are `@Ignore`d: `bif/ErlangBifParser` (a generator utility, not a test) and `performance/ErlangPerformanceTest`.
- `test` never needs Erlang. `testCompilation` reads `erlang.sdk.path` (default `/usr/lib/erlang/`) and `rebar.path` (the rebar3 escript; without it the rebar tests are skipped).

## Platform rules
- Target 262 and 263. `since-build` is derived from `intellijIdea("2026.2")` and `until-build` is `263.*` (`supportedUntilBuild` in `build.gradle`). Do not use APIs newer than 262, and no Java language features or JDK APIs newer than 21. Do not add internal, experimental or override-only API usages; `verifyPlugin` reports them.
- CI job `ide-2026.3` compiles and tests the same sources against the newer IDE and runs the Plugin Verifier on the archive there. Code must compile against both: when an API differs between them, use the form both have. A Copilot cloud session has only 2026.2 (the firewall blocks the downloads of the other IDEs), so do not run `-PplatformVersion` there: push and read the CI job `ide-2026.3`. A test that needs an API only one of them has looks it up by reflection (`ErlangMethodSeparatorProviderTest`).
- The test runner and the structure view are separate plugins of the IDE. `plugin.xml` depends on them by their 2026.2 ids, which 2026.3 keeps as aliases, and `build.gradle` adds them per IDE.
- `until-build` is raised only after the tests and the Plugin Verifier pass on the new IDE: add the IDE to the `other-ides` job of `gradle.yml` and to `release.yml`, then raise `supportedUntilBuild`.
- `jps-plugin` must stay Java 11 bytecode (`options.release = 11`) and use only JPS and JDK APIs, no IDE classes. 828089b1 replaced `GeneralCommandLine` with `ProcessBuilder` for this reason.
- Read PSI, VFS and indices under a read action. Modify PSI and documents in `WriteCommandAction`, which is the pattern in the quick fixes and refactorings.
- Never block the EDT. Process launches, file or network I/O and index queries go to a background thread (`executeOnPooledThread`, `@RequiresBackgroundThread`). Come back with `invokeLater` and an explicit `ModalityState`. Do not add new `SlowOperations.allowSlowOperations`.
- In loops over modules, files or index results, call `ProgressManager.checkCanceled()`. Code that queries indices must cope with dumb mode (`DumbService`).
- Cache derived data with `CachedValuesManager` or `ResolveCache` and the correct dependencies. Never keep PSI, `Project` or `Module` in static fields.
- Resolve other files through stubs and indices (`StubIndex`, `ErlangModuleIndex`), not by loading their AST.
- Gate syntax that depends on the OTP release on `ErlangSdkRelease`, as the lexer does for `maybe` (OTP 25+). Add an `ErlangNNSyntax` inspection where needed, with an HTML description in `resources/inspectionDescriptions/`.

## Definition of done
- A bug fix comes with a test that fails before the fix and passes after it. Run it both ways.
- A behaviour change or new feature comes with tests and `testData` for the new behaviour.
- `./gradlew test` passes. Run `testCompilation` too when you touch `build/`, `jps/` or `jps-plugin/` and Erlang is available.
- Keep the diff small and about the task only. No drive-by refactors, reformatting, import reshuffles, renames, dependency or tool bumps, and no documentation churn.
- Add no author or date comments to classes (CONTRIBUTING.md).

## Stop and ask the maintainer
Change these only when the task explicitly asks for it, and name them at the top of the PR description:
- anything under `.github/`, including workflows;
- releases and versioning: `version` in `gradle.properties`, `<change-notes>` in `plugin.xml`, tags. For a user-facing fix, put a one-line change note in the PR description instead;
- build files: `build.gradle`, `settings.gradle`, `jps-plugin/build.gradle`, `grammar-generator/`, `gradle.properties`, the wrapper, `libs/`;
- `grammars/`, `Erlang.flex` and `gen/`, except a parser or lexer fix made by the rules in Generated code;
- stub element types, stub or index versions and the indexers, except the stub version bump (an `ErlangStubChanges` entry) that a parser fix requires;
- the plugin descriptors `plugin.xml`, `ErlangPlugin.xml` and `java-deps.xml`. Registering a new extension in `ErlangPlugin.xml` is fine when the task asks for that feature; say so in the PR;
- `jps-plugin/` and the committed `.beam` files;
- deleting tests, disabling them (`@Ignore`, a `_test` rename, a new `Assume`), or weakening assertions;
- changing existing golden test data (`*.txt`, `*-after.erl`, markup) so that a test passes. Explain every changed expectation.

## Pull requests
Commit subjects are imperative sentences. The body explains the root cause and ends with `Fixes #N` when there is an issue.
The PR description must state:
1. What failed: the symptom, the issue link and how to reproduce it.
2. The root cause and the fix.
3. The test that proves it, as `Class.testMethod`, and that you saw it fail without the fix.
4. The commands you ran and their results.
5. What was not verified, for example `testCompilation` without Erlang, `verifyPlugin`, or a manual check in the IDE.
6. Any area from the stop-and-ask list that the PR touches, and the proposed change note.

### Small and big pull requests
Every pull request is small or big. The maintainer decides a small one with one click and reads a big one in full, so keep a fix small and about one thing.
- Source lines are the added plus the deleted lines in files outside `gen/`, `testData/` and `tests/`.
- The fix area is `src/`, `tests/`, `testData/`, `gen/`, `grammars/erlang.bnf`, `resources/inspectionDescriptions/` and `resources/intentionDescriptions/`.
- A pull request is big when it changes more than 150 source lines, or more than 50 source lines outside the fix area (workflows, build files, descriptors, documentation, specs), or when the maintainer says so. Otherwise it is small. The maintainer can also call a big one small.
- A small pull request is merged without the maintainer reading the diff only when the review of the head commit says it is ready, CI is green on that commit, and it changes no file outside the fix area. The one exception is `resources/META-INF/ErlangPlugin.xml` when the change only adds whole `<localInspection .../>` or `<intentionAction>` elements, which is how a new inspection or intention is registered. Everything else waits for the maintainer.
- Do not split one change into several pull requests, or move logic into tests or test data, to stay under the limits. If a fix cannot be small, say so at the top of the description and explain why.
