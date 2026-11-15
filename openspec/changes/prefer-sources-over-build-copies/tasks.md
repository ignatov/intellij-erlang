# Tasks

The scenarios of `specs/navigation/spec.md` name the tests; a task names its scenario and the files.
Tests are light fixture tests on plain copies, with the files created by `myFixture.addFileToProject` in the
order given; task 3.14 is the one heavy test. `<T>` stands for the test name with a lower-case first letter, the folder that
`getTestName(true)` gives.
Each test is seen failing before its implementation task, for the reason given in brackets. A task marked
"boundary" has a test that asserts only its boundary scenario, so it passes before and after.

## 1. Shared order

- [ ] 1.1 Add the comparator with the build-output rank next to `ErlangModuleIndex` and use it in `getVirtualFilesByName`; verify with `ErlangModuleIndexOrderTest.testRanks` (new class, `tests/org/intellij/erlang/resolve/`) that the order of `getVirtualFilesByName` is source, `_build/default/lib`, `_build/test/lib`, `_build/prod/rel`, with the files added in that order and in the reverse order
- [ ] 1.2 Use it in `ErlangModuleReferenceImpl` and remove `ModuleResolutionComparator`; verify that `ErlangModuleResolutionTest` passes unchanged (`testKernelModulesAppearFirst`, `testStdlibModulesAppearFirst`, `testUserModulesAppearFirst`)

## 2. Module references

- [ ] 2.1 Scenario "Application of an umbrella project and its view under _build": `testData/resolve/module/<T>/` with `apps/caller/src/caller.erl`, `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl` [today the target is the file under `_build`, first by path]
- [ ] 2.2 Scenario "Application at the project root": `src/caller.erl`, `src/dup.erl`, `_build/default/lib/myapp/src/dup.erl` [as 2.1]
- [ ] 2.3 Scenario "Test suite copied by the test profile": `apps/umb/test/caller_SUITE.erl`, `apps/umb/test/umb_SUITE.erl`, `_build/test/lib/umb/test/umb_SUITE.erl` [as 2.1]
- [ ] 2.4 Scenario "Checkout dependency": `src/caller.erl`, `_checkouts/chk/src/chk.erl`, `_build/default/checkouts/chk/src/chk.erl` [as 2.1]
- [ ] 2.5 Scenario "Release folder of a rebar2 project": `rebar.config`, `src/caller.erl`, `src/dup.erl`, `rel/mynode/lib/myapp-1.0/src/dup.erl` [today the target is the file under `rel`, first by path]
- [ ] 2.6 Scenario "Dependency under a profile and its copy in a release": `src/caller.erl`, `_build/test/lib/dep/src/dep_mod.erl`, `_build/prod/rel/myrel/lib/dep-1.0/src/dep_mod.erl` [today the target is the file under `_build/prod/rel`, first by path]
- [ ] 2.7 Scenario "Remote type": `apps/caller/src/caller.erl`, `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl`; the test asserts the file of the module reference and of the type reference [today both are the file under `_build`]
- [ ] 2.8 Boundary. Scenario "Boundary: a directory named rel that is not build output": `caller.erl`, `lib/rel/src/dup.erl`, `lib/zzz/src/dup.erl`
- [ ] 2.9 Boundary. Scenario "Boundary: dependency that exists only in build output": `src/caller.erl`, `_build/default/lib/dep/src/dep_mod.erl`
- [ ] 2.10 Boundary. Scenario "Boundary: dependency in the default profile and in a release": `src/caller.erl`, `_build/default/lib/dep/src/dep_mod.erl`, `_build/prod/rel/myrel/lib/dep-1.0/src/dep_mod.erl`

## 3. Remote calls

Every test of this section asserts the file of the module reference and the file of the function reference.

- [ ] 3.1 Make `ErlangFunctionReferenceImpl` look for the function of a reference with a module part in the file that the module name resolves to (the lookup, scope and order of the module reference); only when the module name does not resolve, `getExternalFunction` runs as today with `GlobalSearchScope.allScope`, in the shared order. Scenario "Both parts of a remote call": `testData/resolve/function/<T>/` with `apps/caller/src/caller.erl`, `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl` [today the module is the file under `_build` and the function the file under `apps`]
- [ ] 3.2 Scenario "Import entry": the files of 3.1 with `-import(dup, [f/0]).` in `caller.erl` [as 3.1]
- [ ] 3.3 Scenario "Function that only the copy has": the files of 3.1; the copy exports `g/0` [today `g` resolves to the copy]
- [ ] 3.4 Scenario "Dependency in the default and in the test profile": `src/caller.erl`, `_build/default/lib/dep/src/dep_mod.erl`, `_build/test/lib/dep/src/dep_mod.erl` [today the function is the file under `_build/test`, the shorter path, in either creation order]
- [ ] 3.5 Scenario "Dependency with the name of an OTP application": `src/caller.erl`, `_build/default/lib/eunit/src/eunit.erl`, with the mock SDK `testData/mockSdk-R15B02` as in `ErlangModuleResolutionTest` [today the function is `lib/eunit-2.2.4/src/eunit.erl` of the SDK]
- [ ] 3.6 Scenario "Two applications with a module of one name": `caller.erl`, `lib/aaaa/src/dup.erl`, `lib/zzz/src/dup.erl` [today the function is the file under `lib/zzz`, the shorter path, in either creation order]
- [ ] 3.7 Scenario "Find Usages of the module": `testData/find-usages/<T>/` with the files of 3.1 [today the module of `apps/umb/src/dup.erl` has no usage]
- [ ] 3.8 Scenario "Release copy of an OTP application": `src/caller.erl`, `_build/prod/rel/myrel/lib/eunit-2.2.4/src/eunit.erl`, with the mock SDK as in 3.5 [today the module is the release copy and the function the SDK's file]
- [ ] 3.9 Scenario "Release copy of stdlib": `src/caller.erl`, `ebin/stdlib.app` and `src/lists.erl` under `_build/prod/rel/myrel/lib/stdlib-6.2`, with the mock SDK as in 3.5 [today both are the release copy: its directory has the higher version and is taken for stdlib]
- [ ] 3.10 Scenario "File under a directory whose name starts with a dot": `src/caller.erl`, `src/dup.erl`, `.eunit/dup.erl` [today the module is `.eunit/dup.erl`, first by path]
- [ ] 3.11 Boundary. Scenario "Boundary: file under a source root and file outside every source root": `caller.erl`, `lib/aaaa/src/dup.erl`, `lib/zzz/src/dup.erl`; the test removes the source root of the fixture and adds `lib/zzz/src` with `PsiTestUtil`, and restores both in `finally`
- [ ] 3.12 Boundary. Scenario "Boundary: module that only a release copy has": `src/caller.erl`, `_build/prod/rel/myrel/lib/eunit-2.9/src/eunit_new.erl`, with the mock SDK as in 3.5
- [ ] 3.13 Boundary. Scenario "Boundary: module in a file outside every source root": `lib/zzz/src/caller.erl`, `other/dup.erl`; the source roots are changed and restored as in 3.11
- [ ] 3.14 Boundary. Scenario "Boundary: module of that name in an IDE module that is not a dependency": new heavy test class `ErlangTwoModulesResolveTest` (`JavaModuleTestCase`, `tests/org/intellij/erlang/resolve/`). The test creates `zzz_a/src/caller.erl`, `zzz_a/src/dup.erl` and `aaa_b/src/dup.erl` in one temporary directory and makes `zzz_a` and `aaa_b` Erlang modules with the content root of that name and the source root `src`, without a dependency. It is not a light fixture test and creates no `testData`. Proposal.md, open decision 8, decides whether this task stays

## 4. include_lib

- [ ] 4.1 Make the processor of `ErlangApplicationIndex` collect the candidates and choose by the shared order (a project directory outside a release, then a library, then a release), then by the existing version rule, then by path; verify that `ErlangIncludeLibResolveTest` passes unchanged (`testLatestVersionIsUsed`, `testNoVersionIsUsed`, `test375`)
- [ ] 4.2 Scenario "Header of a project application": `testData/resolve/includeLib/<T>/`, created in this order: `apps/caller/src/caller.erl`, `apps/umb/src/umb.app.src`, `apps/umb/include/dup.hrl`, then `ebin/umb.app` and `include/dup.hrl` under `_build/default/lib/umb`, then the same under `_build/test/lib/umb` [today the target is the directory created last, `_build/test/lib/umb`; in the reverse order the test would pass today]
- [ ] 4.3 Scenario "Macro and record from that header": the files and the order of 4.2 with `?DUP` and `#duprec{}` in `caller.erl` [as 4.2]
- [ ] 4.4 Scenario "Header of a fetched dependency": `src/caller.erl`, then `ebin/dep.app` and `include/dep.hrl` under `_build/default/lib/dep`, then the same under `_build/test/lib/dep`, in this order [today the target is the directory created last, `_build/test/lib/dep`; in the reverse order the test would pass today]
- [ ] 4.5 Scenario "Header of an OTP application in a release": new class `ErlangIncludeLibSdkResolveTest` (`tests/org/intellij/erlang/resolve/`) with the mock SDK as in 3.5; `src/caller.erl`, then `ebin/eunit.app` and `include/eunit.hrl` under `_build/prod/rel/myrel/lib/eunit-2.9` [today the target is the release copy, the higher version]

## 5. Whole build

- [ ] 5.1 Run `./gradlew --no-build-cache clean test` and verify that it passes and that no existing `testData` expectation changed
- [ ] 5.2 Run `testCompilation` as AGENTS.md gives it and verify that it passes: the build order receives its paths from the changed lookup
- [ ] 5.3 Put the change note "Go to definition prefers project sources over their copies under `_build` (#1040)" in the description of the implementation pull request

## Workflow follow-up

- After the implementation is merged, archive the change in a pull request of its own: `openspec archive prefer-sources-over-build-copies --yes`.
