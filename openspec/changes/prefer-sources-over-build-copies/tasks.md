# Tasks

Tests are light fixture tests on plain copies. Each test is seen failing before its implementation task.
`<T>` stands for the test name with a lower-case first letter, the folder that `getTestName(true)` gives.

## 1. Shared order

- [ ] 1.1 Add the comparator with the build-output rank next to `ErlangModuleIndex` and use it in `getVirtualFilesByName`; verify with `ErlangModuleIndexOrderTest.testRanks` (new class, `tests/org/intellij/erlang/resolve/`, files created with `myFixture.addFileToProject`) that the order of `getVirtualFilesByName` is source, `_build/default/lib`, `_build/test/lib`, `_build/prod/rel`, whatever order the files were added in
- [ ] 1.2 Use it in `ErlangModuleReferenceImpl` and remove `ModuleResolutionComparator`; verify that `ErlangModuleResolutionTest` passes unchanged (`testKernelModulesAppearFirst`, `testStdlibModulesAppearFirst`, `testUserModulesAppearFirst`)

## 2. Module references

- [ ] 2.1 Scenario "Application of an umbrella project and its view under _build": `ErlangModuleResolutionTest.testUmbrellaAppWinsOverBuild`, `testData/resolve/module/<T>/` with `apps/caller/src/caller.erl`, `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl`
- [ ] 2.2 Scenario "Application at the project root": `ErlangModuleResolutionTest.testRootAppWinsOverBuild`, `testData/resolve/module/<T>/` with `src/caller.erl`, `src/dup.erl`, `_build/default/lib/myapp/src/dup.erl`
- [ ] 2.3 Scenario "Test suite copied by the test profile": `ErlangModuleResolutionTest.testSuiteWinsOverTestProfileCopy`, `testData/resolve/module/<T>/` with `apps/umb/test/caller_SUITE.erl`, `apps/umb/test/umb_SUITE.erl`, `_build/test/lib/umb/test/umb_SUITE.erl`
- [ ] 2.4 Scenario "Checkout dependency": `ErlangModuleResolutionTest.testCheckoutWinsOverBuild`, `testData/resolve/module/<T>/` with `src/caller.erl`, `_checkouts/chk/src/chk.erl`, `_build/default/checkouts/chk/src/chk.erl`
- [ ] 2.5 Scenario "Release folder of a rebar2 project": `ErlangModuleResolutionTest.testSourceWinsOverRebar2Release`, `testData/resolve/module/<T>/` with `rebar.config`, `src/caller.erl`, `src/dup.erl`, `rel/mynode/lib/myapp-1.0/src/dup.erl`
- [ ] 2.6 Scenario "A directory named rel that is not build output": `ErlangModuleResolutionTest.testRelWithoutRebarConfigIsSource`, `testData/resolve/module/<T>/` with `caller.erl`, `lib/rel/src/dup.erl`, `lib/zzz/src/dup.erl`
- [ ] 2.7 Scenario "Fetched dependency": `ErlangModuleResolutionTest.testDependencyOnlyInBuild`, `testData/resolve/module/<T>/` with `src/caller.erl`, `_build/default/lib/dep/src/dep_mod.erl`
- [ ] 2.8 Scenario "Dependency in the default and in the test profile": `ErlangModuleResolutionTest.testDefaultProfileWinsOverTestProfile`, `testData/resolve/module/<T>/` with `src/caller.erl`, `_build/default/lib/dep/src/dep_mod.erl`, `_build/test/lib/dep/src/dep_mod.erl`
- [ ] 2.9 Scenario "Dependency in the default profile and in a release": `ErlangModuleResolutionTest.testDefaultProfileWinsOverRelease`, `testData/resolve/module/<T>/` with `src/caller.erl`, `_build/default/lib/dep/src/dep_mod.erl`, `_build/prod/rel/myrel/lib/dep-1.0/src/dep_mod.erl`

## 3. Remote calls

- [ ] 3.1 Make `ErlangFunctionReferenceImpl.getExternalFunction` look in the file that the module name resolves to. Scenario "Both parts of a remote call": `ErlangFunctionResolutionTest.testRemoteCallResolvesInSourceNotBuild`, `testData/resolve/function/<T>/` with `apps/caller/src/caller.erl`, `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl`; it asserts the file of both the module reference and the function reference
- [ ] 3.2 Scenario "Function that only the copy has": `ErlangFunctionResolutionTest.testFunctionOnlyInBuildCopyIsUnresolved`, `testData/resolve/function/<T>/` with `apps/caller/src/caller.erl`, `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl` (the copy exports `g/0`)
- [ ] 3.3 Find Usages follows: `ErlangFindUsagesTest.testRemoteCallIsUsageOfSourceFunction`, `testData/find-usages/<T>/` with the files of 3.1; the usages of `f/0` in `apps/umb/src/dup.erl` include the call in `caller.erl`

## 4. include_lib

- [ ] 4.1 Make `ErlangApplicationIndex.getApplicationDirectoryByName` collect the candidates and choose by rank, then by the existing version rule, then by path; verify that `ErlangIncludeLibResolveTest` passes unchanged (`testLatestVersionIsUsed`, `testNoVersionIsUsed`, `test375`)
- [ ] 4.2 Scenario "Header of a project application": `ErlangIncludeLibResolveTest.testProjectAppWinsOverBuild`, `testData/resolve/includeLib/<T>/` with `apps/caller/src/caller.erl`, `apps/umb/src/umb.app.src`, `apps/umb/include/dup.hrl`, and `ebin/umb.app` and `include/dup.hrl` under `_build/default/lib/umb` and `_build/test/lib/umb`
- [ ] 4.3 Scenario "Macro and record from that header": `ErlangIncludeLibResolveTest.testMacroAndRecordFromProjectApp`, the `testData` of 4.2 with `?DUP` and `#duprec{}` in `caller.erl`
- [ ] 4.4 Scenario "Header of a fetched dependency": `ErlangIncludeLibResolveTest.testDefaultProfileDependencyWinsOverTestProfile`, `testData/resolve/includeLib/<T>/` with `src/caller.erl`, and `ebin/dep.app` and `include/dep.hrl` under `_build/default/lib/dep` and `_build/test/lib/dep`

## 5. Whole build

- [ ] 5.1 Run `./gradlew --no-build-cache clean test` and verify that it passes and that no existing `testData` expectation changed
- [ ] 5.2 Put the change note "Go to definition prefers project sources over their copies under `_build` (#1040)" in the description of the implementation pull request

## Workflow follow-up

- After the implementation is merged, archive the change in a pull request of its own: `openspec archive prefer-sources-over-build-copies --yes`.
