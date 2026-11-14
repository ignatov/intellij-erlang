---
applyTo: "**"
excludeAgent: "cloud-agent"
---

# Code review: intellij-erlang

The project rules are in `AGENTS.md` at the repository root. Review against them; this file adds only what is specific to review.

## Check every pull request for
1. **Hand-edited generated code.** A change in `gen/` must match a change in `grammars/erlang.bnf` or `src/org/intellij/erlang/parser/Erlang.flex` in the same PR.
2. **Grammar or lexer changed without regeneration.** If the `.bnf` or `.flex` changed but `gen/` did not, or the changes don't correspond, that is a defect. Also flag a rule that generates PSI added or renamed without its `gen/` PSI class, or a token without its `ErlangTypes` constant. `private` rules generate only a parser method and no PSI class; do not flag those.
3. **Stub shape changed without a version bump.** Changes to `stubs/types/*` (`serialize`, `deserialize`, `createStub`, `shouldCreateStub`), to bnf `stubClass` or stubbed rules, or to a parser change that alters stubbed elements need `VERSION` in `ErlangFileElementType` bumped. Changed index keys, values or externalizers need their index `VERSION` or `INDEX_VERSION` bumped. Also flag code where the stub path and the AST path return different results.
4. **Threading.** Look for:
   - PSI, VFS or index access outside a read action;
   - PSI or document changes outside `WriteCommandAction`;
   - process launches, I/O or index queries on the EDT;
   - new `SlowOperations.allowSlowOperations`;
   - `invokeLater` without a deliberate `ModalityState`;
   - long loops without `ProgressManager.checkCanceled()`;
   - index queries that break in dumb mode.
5. **Caching and leaks.** Look for PSI, `Project` or `Module` kept in static fields or long-lived caches, and `CachedValue` entries with missing or wrong dependencies.
6. **API compatibility.** Flag APIs that are not in platform 263 (IntelliJ IDEA 2026.3), and internal, experimental or override-only APIs. In `jps-plugin/`, flag anything that is not Java 11 bytecode or that uses IDE-only classes.
7. **Tests.**
   - A bug fix must include a test that would fail without the fix. Say which test is missing, or why the added one would pass without the fix.
   - Tests must be JUnit 3 style and must not need a real Erlang install outside the `build/` and `jps/` packages.
8. **Golden data.** Flag any of these that the PR does not explain:
   - `testData/**/*.txt` or `*-after.erl` rewritten wholesale;
   - expected output changed in files unrelated to the fix;
   - markup removed so that a test passes;
   - tests deleted or disabled (`@Ignore`, a `_test` rename, a new `Assume`), or assertions weakened.
9. **Scope.** Flag changes unrelated to the stated purpose: refactors, reformatting, renames, dependency bumps or documentation churn.
10. **Stop-and-ask areas** (listed in `AGENTS.md`): `.github/`, versioning and change notes, build files, `grammars/` and `gen/`, stub and index versions, the plugin descriptors and `jps-plugin/`. If the PR touches one and its description does not call it out, say so. A parser or lexer fix may change `grammars/erlang.bnf`, `Erlang.flex` and `gen/`: check it against "Parser and lexer fixes" in `AGENTS.md` instead. It needs a parser test that fails without the change, an `erlc` result for the snippet, and a reason for every changed `testData/parser/*.txt` dump.
11. **Specs** ("Specs" in `AGENTS.md`).
    - A bug fix must add a scenario for the corrected behaviour to `openspec/specs/<capability>/spec.md`, through a change `fix-<issue>-<what>` that the same PR archives under `openspec/changes/archive/`. Flag a fix without one, a fix change left open, and a change in `openspec/specs/` that no archived delta of the PR explains. Accept a fix without one in two cases, when the top of the PR description says so: `openspec` was missing in the session, or an open change stands in the way (see the open-change check below). Then check that the PR changes nothing under `openspec/`. This includes a parser or lexer fix that depends on macros or on the preprocessor; only a proposal for new behaviour of that kind is refused.
    - The scenario must match the test: the test it names exists in the PR, and its `testData` shows the behaviour that the scenario states. In a proposal the test does not exist yet; flag a scenario that names none.
    - The scenario must match what decides the answer. Where that is erlc, run erlc on the snippet when Erlang is available and compare; where it is the build tool, run the tool; otherwise report that it is not checked. Flag an erlc statement in a scenario where erlc does not decide. Resolve decides Find Usages and Rename: such a scenario says that it follows from which declaration the references resolve to and what decides that resolve (erlc in one module, the build tool between files). Flag one that names a platform convention instead, and one that gives no reason.
    - Flag spec text for behaviour that the PR does not change, unless it is a boundary scenario: named `Boundary: ...`, first line exactly "Unchanged by the fix; pinned by its test." (in a proposal "Unchanged by the change; pinned by its test."), with a test of the PR that asserts it. That test may assert changed behaviour too and then fails without the change; do not flag that. Check the boundary claim with a probe: run the scenario's own snippet on the code before the change, and flag the scenario when the result differs from what it states. Flag a boundary scenario without the mark or with another first line, one whose named test does not assert it, an unmarked scenario whose test passes without the change, and a requirement that has boundary scenarios only.
    - Flag scenarios of the requirement that disappeared, and a scenario name that occurs twice in the spec (the `specs` job fails on it in the step "Check for repeated scenario names").
    - Open changes. For every requirement that the fix touches, run `grep -rlF --exclude-dir=archive '### Requirement: <name>' openspec/changes`, also when validate passes: it does not show every case.
      - An open change modifies the requirement and the fix adds a scenario: the PR must also copy the scenario, unchanged, into the `MODIFIED` block of that change and name the edit at the top of its description. A missing copy shows in `openspec validate --all --strict --no-interactive` as "omits scenario(s) the current spec still has".
      - Flag a fix that changes the text of a requirement that an open change modifies, and a fix that adds a scenario to a requirement that an open change removes or renames. Validate passes, and the text or the scenario is lost when the open change is archived. Such a fix leaves the spec step out and says so at the top of its description, with the open change, the requirement and the text it would have written.
      - Flag any other edit in a change that the PR did not create.
    - Flag a main spec with two `## Purpose` or two `## Requirements` sections, or a `## Purpose` shorter than 50 characters: a conflict resolved by hand, or a short first-touch Purpose.
    - A baseline PR (`baseline-<capability>-<slice>`) changes nothing outside `openspec/`. Every requirement names an existing test; flag a requirement that no test proves, and a missing "Not covered by tests" list in its `design.md`.

## How to report
- Report only concrete defects. For each one give the file and line, what goes wrong, and a scenario that triggers it: the input, the thread, or the IDE state.
- No style nitpicks, no praise, no restating the diff, and no generic advice.
- When you are unsure whether something is wrong, say what you would need to confirm it. Do not present it as a finding.
- If you find nothing wrong, say which of the checks above you performed. Do not just approve.
