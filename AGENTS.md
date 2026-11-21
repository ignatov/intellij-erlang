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

## Specs
The project uses [OpenSpec](https://github.com/Fission-AI/OpenSpec) 2.0.0. The rules for the artifacts are in `openspec/config.yaml`.
The specs grow from the work that is done: `openspec/specs/<capability>/spec.md` says what the plugin does in one area, and every scenario names the test that proves it and says what decides the answer. `openspec/specs/navigation/spec.md` is the example to copy.
- `openspec` is on `PATH` in agent sessions; the Copilot setup steps install it. When `command -v openspec` prints nothing, stop the spec work and say so: without the tool, do not do by hand what its commands do (creating a change, archiving it, merging a delta into a spec). With the tool, the delta of a change is written by hand and the `rm` and `mv` steps of the procedures below are run as written. Without it, a fix is still made and tested, and its pull request says at the top of the description that the spec step is missing. A proposal or a baseline is not started.
- A capability is a lasting feature area, named as `openspec/config.yaml` says (the `feat:*` labels and the folders of `testData/`). Its spec is created by the first change that touches the area. A spec without requirements cannot exist: `openspec validate` refuses it ("Spec must have at least one requirement").
- `openspec validate --all --strict --no-interactive` must pass. CI runs it in the `specs` job, followed by the steps "Check that archiving would not refuse a delta" and "Check for repeated scenario names". Validate checks the form of the open changes and of the specs, not whether a scenario is true: that is checked by running the named test and the thing that decides.
- `openspec/specs/` changes only through `openspec archive <name> --yes`. Never archive by the by-hand path that the generated `openspec-archive-change` skill offers ("Archive without syncing", merging the delta into the main spec yourself, moving the folder with `mv`): it skips the checks with which the command refuses a delta that does not fit the spec. Refactoring, tooling and documentation need no change and no spec.
- `.agents/skills/openspec-*` and `.claude/skills/openspec-*` are generated by `openspec init`. Never edit them by hand; `openspec update` rewrites them after a version change.

### What a scenario says
- The snippet or the files, what the plugin does, what decides that answer, and in its last step the test, as `Class.testMethod`. This holds for a fix, a baseline and a proposal alike, so that the archived spec carries the test names. In a fix the test is the one the fix adds, in a baseline one that exists and passes, in a proposal the one the implementation will add.
- Say what erlc does only where erlc decides the answer: what is valid syntax, what is an error or a warning, what a name refers to in one module (resolve). State that behaviour as "the same as erlc", and run erlc on the snippet.
- Resolve decides Find Usages and Rename: they list and change the references that resolve to the declaration. A scenario about Find Usages or Rename says that it follows from which declaration the references resolve to, and what decides that resolve: erlc in one module, the build tool between files. It names no platform convention.
- Elsewhere name what does decide, and check it there:
  - the build tool: which files rebar3 compiles and what it puts on the code path;
  - the platform convention: what an IntelliJ action such as Go to Class or Reformat Code does in other languages;
  - the code style setting, for the formatter.
- A fix and a proposal specify the behaviour that they change, and the boundary cases that their own tests assert: behaviour next to the change that stays as it is. Nothing else.
- The snippet of a scenario holds every file that the answer depends on, as its test does: an `-import` of a module, for example, comes with that module. In a fix and in a proposal, the snippet of a scenario that is not a boundary gives another result on the code before the change than the scenario states. The reviewer probes that for every scenario of a fix.
- A boundary scenario is named `Boundary: ...`. Its first line is exactly "Unchanged by the fix; pinned by its test." (in a proposal "Unchanged by the change; pinned by its test."), also when it names several tests. It names a test that asserts it. That test may assert changed behaviour as well and then fails without the change, so a test run does not prove that the behaviour is unchanged: the reviewer checks that with a probe of the scenario's own snippet on the code before the change.
- A requirement name occurs once in the main spec and the open changes together. `openspec archive` refuses a delta that adds a name that the spec has. `openspec validate` only prints an INFO line for it, "Archive would refuse this delta", and exits 0; the step "Check that archiving would not refuse a delta" of the `specs` job fails on that line. Locally, `openspec validate --all --strict --no-interactive 2>&1 | grep -F 'Archive would refuse this delta'` must print nothing.
- A scenario name occurs once in a spec. `openspec validate` does not check this; the step "Check for repeated scenario names" of the `specs` job does. Locally, `grep -h '^#### Scenario:' openspec/specs/<capability>/spec.md | sort | uniq -d` must print nothing.
- Parser behaviour that depends on macros or on the preprocessor is not proposed as new behaviour one special case at a time: the maintainer's direction is a lexer that substitutes macros, and such a proposal waits for that design. This does not apply to a bug fix that "Parser and lexer fixes" allows: it records its scenario as any other fix.

### A bug fix
The pull request of a fix also records the corrected behaviour in the spec of its area. It needs no proposal, no approval before the fix and no pull request of its own. After the test and the fix:
1. `openspec new change fix-<issue>-<what>`, and add `x-issue: <issue>` to its `.openspec.yaml`.
2. Write `openspec/changes/fix-<issue>-<what>/specs/<capability>/spec.md` and nothing else: no `proposal.md`, `design.md` or `tasks.md`.
   - First touch of the capability (`openspec list --specs` does not show it): `## Purpose`, then `## ADDED Requirements` with the requirement and its scenarios. The Purpose has at least 50 characters and covers the whole area, not the fix. The tool accepts a shorter one in the delta and refuses the spec after archiving ("Purpose section is too brief"); without a Purpose, archiving writes "TBD", which fails the same way.
   - A new scenario for a requirement that exists: `## MODIFIED Requirements` with the whole requirement block copied from `openspec/specs/<capability>/spec.md` and the scenario added at its end. No `## Purpose`. A block that leaves out an existing scenario is refused.
   - A new requirement in a capability that exists: `## ADDED Requirements`, no `## Purpose`. Archiving refuses a requirement name that exists. A name that an open change adds is taken too (step 4).
3. Write the scenarios as "What a scenario says" asks: the changed behaviour, and the boundary cases that the tests of the fix assert, marked as such.
4. Before archiving, in every case: `grep -rlF --exclude-dir=archive '### Requirement: <name>' openspec/changes` for every requirement that the fix touches or adds. It lists the fix's own change. When it lists another one, follow "A fix on a requirement that an open change names" first. Do not wait for a failing validate: it does not show every case.
5. `openspec validate --all --strict --no-interactive`, then `openspec archive fix-<issue>-<what> --yes`, then validate again. Archive in every case: only archiving shows that the delta fits the spec. Commit the result: the scenario in `openspec/specs/<capability>/spec.md` and the change under `openspec/changes/archive/`.
6. When the validate after archiving fails in another change ("omits scenario(s) the current spec still has"), step 4 was left out: do it now. When it passes and prints `Archive would refuse this delta: ... already exists`, step 4 was left out too and an open change adds a requirement of the name that the fix added: undo what the spec step changed under `openspec/` and leave the spec step out, as "A fix on a requirement that an open change names" says. When it fails for the Purpose, undo and repeat: `rm -r openspec/specs/<capability>`, `mv openspec/changes/archive/<date>-fix-<issue>-<what> openspec/changes/fix-<issue>-<what>`, correct the Purpose, archive again.

### A fix on a requirement that an open change names
An open change (a merged proposal that is not archived yet) may name the requirement that the fix touches, in a `## MODIFIED Requirements`, a `## REMOVED Requirements` or a `## RENAMED Requirements` block. When that change is archived, its block replaces or removes the requirement as the spec has it then. What the fix wrote there is lost, and validate passes before and after: a changed requirement text when the block is `MODIFIED`, the whole requirement with the new scenario when it is `REMOVED`. An open change may also add, in a `## ADDED Requirements` block, a requirement of the name that the fix wants to add. Requirement names are unique across the main spec and all open changes: once the fix has archived that name, archiving the open change aborts with "already exists". Nothing is lost, but master is left with a change that cannot be archived, and validate still exits 0. So the fix looks before it archives (step 4 of "A bug fix") and does one of two things.
- **The open change modifies the requirement and the fix only adds a scenario.** The fix pull request copies its scenario, unchanged, to the end of that `MODIFIED` block, then archives. It changes nothing else in the open change. It names this edit at the top of its description: the change, the requirement and the scenario. A fix that left the copy out fails the validate after archiving in the other change: `✗ change/<other> ... MODIFIED "<name>" omits scenario(s) the current spec still has: "<scenario>"`.
- **The fix needs another text of a requirement that an open change modifies, it touches a requirement that an open change removes or renames, or it adds a requirement of a name that an open change adds.** The fix does not do the spec step: it creates and archives no `fix-...` change and edits nothing in the open change. It does not pick another name to get past the check. The fix is still made and tested. Its pull request says at the top of the description that the spec step is missing, names the open change and the requirement, and gives the text or the scenario that it would have written. The maintainer decides. A fix may change the text of a requirement that no open change names.
- A proposal pull request must pass `specs` on top of the current master before it is merged: merge master into the branch and see the check pass. A fix that was merged after the proposal was opened touches other files, so git reports no conflict.
- When `specs` fails on master after two merges with the message above, the repair is the same copy of the scenario, in a pull request of its own. When it fails there in the step "Check that archiving would not refuse a delta", two changes added one requirement name; the maintainer decides which one keeps it.

### When master's spec changed under an archived change
Another pull request may have changed `openspec/specs/<capability>/spec.md` in the meantime: it created the capability (git reports `CONFLICT (add/add)`), or it added a scenario to the same requirement. Never resolve the conflict by hand; keeping both sides gives a spec with two `## Purpose` and two `## Requirements` sections, which validate refuses. Archive again on top of master's spec:
1. Merge master. `git checkout origin/master -- openspec/specs/<capability>/spec.md` takes master's spec.
2. `mv openspec/changes/archive/<date>-<name> openspec/changes/<name>` reopens the change.
3. Fit the delta to the spec as it is now: delete `## Purpose` when the capability exists; copy a `MODIFIED` block again from master's spec and add the scenario at its end; when the requirement name under `ADDED` exists now, use `MODIFIED` or another name.
4. Validate, `openspec archive <name> --yes`, validate, check for a repeated scenario name, and commit the merge.

### A feature or a change of user-visible behaviour
- It starts with a proposal: a change folder `openspec/changes/<name>/` with `proposal.md`, delta specs, `design.md` and `tasks.md`. Write it with the `openspec-propose` skill (`.agents/skills/`, linked from `.claude/skills/`), or by hand: `openspec new change <name>`, then `openspec instructions <artifact> --change <name>` for `proposal`, `specs`, `design` and `tasks`, and `openspec status --change <name>`.
- Every scenario of the delta names its test. `tasks.md` refers to the scenarios by name and lists the `testData` files; it does not repeat the test names as a second source.
- The proposal and the implementation are separate pull requests. The proposal pull request contains only the change folder. The implementation starts only after the maintainer has merged the proposal; it ticks the boxes of `tasks.md`.
- After the implementation is merged, archive the change in a pull request of its own: `openspec archive <name> --yes`. It merges the delta specs into `openspec/specs/` and moves the folder to `openspec/changes/archive/`.

### A baseline of existing behaviour
Existing behaviour is written down from the code, one area or one slice of an area at a time, in pull requests of their own that the maintainer reviews. Never mix it into a feature or a fix.
- Keep the slice small enough to be read in one sitting: what one test class proves, or a part of it, and at most about five requirements. The first slice of an area is one family of requirements, for example the resolve of local function calls. A spec that nobody reads is worse than none.
- `openspec new change baseline-<capability>-<slice>`, then write two files: the delta spec `specs/<capability>/spec.md` as for a bug fix, and `design.md`.
- Every requirement names, in its scenarios, an existing test that proves it and what decides the answer. Run the tests and, where erlc or the build tool decides, run it.
- Behaviour that the code has but no test proves is not specified. List it in `design.md` under "Not covered by tests", one line each with the class that has it, as input for later test work.
- Validate, `openspec archive baseline-<capability>-<slice> --yes`, validate again, and commit. The pull request changes nothing outside `openspec/`.

## Definition of done
- A bug fix comes with a test that fails before the fix and passes after it. Run it both ways.
- A bug fix records the corrected behaviour in the spec of its area, as "A bug fix" in Specs says.
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
- `openspec/config.yaml`, the generated OpenSpec skills, the OpenSpec version pinned in the workflows, `openspec/specs/` other than by archiving a change, and an open change of another pull request other than the scenario sync that Specs describes;
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
