# AGENTS.md

This is the IntelliJ Platform plugin for Erlang: lexer, parser, PSI, stubs, inspections, completion, run/debug, rebar and eunit.
Plugin id `org.jetbrains.erlang`. It is built against IntelliJ IDEA 2026.3 (platform branch 263) with Java 25.

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
Use JDK 25 (CI uses Temurin 25) and the Gradle wrapper.
The first build downloads IntelliJ IDEA 2026.3 (several GB) from JetBrains hosts. Running even one test class takes minutes.

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

- `test` excludes the `org/intellij/erlang/build/**` and `org/intellij/erlang/jps/**` packages. Those tests run only in `testCompilation`.
- The CI job `compilation-tests` runs `testCompilation` on `ubuntu-24.04` with the pinned Erlang/OTP 27 from builds.hex.pm and the pinned rebar3 escript (versions and sha256 are in `gradle.yml`, the same as in `copilot-setup-steps.yml`). It fails if any test was skipped.
- The rebar tests skip themselves when `rebar.path` is unset. The Copilot setup steps put `rebar3` on `PATH`, so `command -v rebar3` finds it.
- `verifyPlugin` checks only against the IDE that is built against. It fails on compatibility problems, missing dependencies, an invalid plugin and override-only API usages.

## Generated code
- Grammar-Kit generates `gen/org/intellij/erlang/{ErlangTypes.java,parser/ErlangParser.java,psi/**}` from `grammars/erlang.bnf`.
- JFlex 1.9.2, the IntelliJ-patched one that Grammar-Kit bundles, generates `gen/org/intellij/erlang/parser/_ErlangLexer.java` from `Erlang.flex`.
- `./gradlew generateLexerAndParser` regenerates the lexer, parser, `ErlangTypes` and PSI in place from `Erlang.flex` and `grammars/erlang.bnf`. It reads `psiImplUtilClass`, the mixins and the stub classes from the sources, as "Generate Parser Code" does in the IDE, and does not need the plugin to compile.
- `./gradlew verifyGeneratedSources` generates them into the build directories and fails if the committed `gen/` differs. CI runs it.
- After deleting `gen/`, or when a generated signature depends on a PSI type that does not exist yet, run `generateLexerAndParser` a second time and check that the second run changes nothing.
- Never hand-edit `gen/`. A grammar or lexer change ships in the same commit as the regenerated `gen/` and the updated `testData/parser/*.txt` golden files. If you cannot run the generator, do not change the grammar; stop and say so.
- Handwritten PSI logic goes in `ErlangPsiImplUtil` (the bnf `psiImplUtilClass`) or in mixins under `src/`. Stubbed rules are wired through `stubClass` and `ErlangElementTypeFactory` in the bnf.

## Stubs and indices
- The stub version of `.erl` files is `VERSION` in `src/org/intellij/erlang/stubs/types/ErlangFileElementType.java`, returned by `getStubVersion()`. Bump it by one in the same change when either of these changes:
  - the bytes written per stub: `serialize`, `deserialize` or `createStub` of a type in `stubs/types/`;
  - which elements get stubs: `shouldCreateStub`, `stubClass` in the bnf, or a parser change that turns the same text into different stubbed elements. b857bcb7 bumped it after the OTP 28/29 grammar changes.
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
- Target only 263. `since-build` is derived from `intellijIdea("2026.3")` and there is no `until-build`. Do not use APIs newer than 263. Do not add internal, experimental or override-only API usages; `verifyPlugin` reports them.
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
- build files: `build.gradle`, `settings.gradle`, `jps-plugin/build.gradle`, `gradle.properties`, the wrapper, `libs/`;
- `grammars/`, `Erlang.flex` and `gen/`;
- stub element types, stub or index versions and the indexers;
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
