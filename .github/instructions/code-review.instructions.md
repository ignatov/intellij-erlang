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
10. **Stop-and-ask areas** (listed in `AGENTS.md`): `.github/`, versioning and change notes, build files, `grammars/` and `gen/`, stub and index versions, the plugin descriptors and `jps-plugin/`. If the PR touches one and its description does not call it out, say so.

## How to report
- Report only concrete defects. For each one give the file and line, what goes wrong, and a scenario that triggers it: the input, the thread, or the IDE state.
- No style nitpicks, no praise, no restating the diff, and no generic advice.
- When you are unsure whether something is wrong, say what you would need to confirm it. Do not present it as a finding.
- If you find nothing wrong, say which of the checks above you performed. Do not just approve.
