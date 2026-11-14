# Spec Delta

## ADDED Requirements

### Requirement: Source file wins over its copy in build output
When several files have the name of the referenced module, the plugin SHALL resolve the reference to the file
that rebar3 compiles: a file outside build output. Build output is everything under a `_build` directory and
under a `rel` directory next to a `rebar.config`. Find Usages of the module follows. The boundary scenarios say
what does not count as build output and that nothing is hidden.

#### Scenario: Application of an umbrella project and its view under _build
- **GIVEN** `apps/umb/src/dup.erl` and `_build/default/lib/umb/src/dup.erl`, both `-module(dup).`
- **WHEN** the user goes to the definition of `dup` in `dup:f()` in `apps/caller/src/caller.erl`
- **THEN** the target is `apps/umb/src/dup.erl`
- **AND** rebar3 decides: it compiles `apps/umb/src/dup.erl`, and `_build/default/lib/umb/src` is a link to that folder
- **AND** the test is `ErlangModuleResolutionTest.testUmbrellaAppWinsOverBuild`

#### Scenario: Application at the project root
- **GIVEN** `src/dup.erl` and `_build/default/lib/myapp/src/dup.erl`, both `-module(dup).`
- **WHEN** the user goes to the definition of `dup` in `dup:f()` in `src/caller.erl`
- **THEN** the target is `src/dup.erl`
- **AND** rebar3 decides: it compiles `src/dup.erl`
- **AND** the test is `ErlangModuleResolutionTest.testRootAppWinsOverBuild`

#### Scenario: Test suite copied by the test profile
- **GIVEN** `apps/umb/test/umb_SUITE.erl` and its copy `_build/test/lib/umb/test/umb_SUITE.erl`
- **WHEN** the user goes to the definition of `umb_SUITE` in `umb_SUITE:all()`
- **THEN** the target is `apps/umb/test/umb_SUITE.erl`
- **AND** rebar3 decides: it compiles `apps/umb/test/umb_SUITE.erl` and keeps a copy of the file next to the beam
- **AND** the test is `ErlangModuleResolutionTest.testSuiteWinsOverTestProfileCopy`

#### Scenario: Checkout dependency
- **GIVEN** `_checkouts/chk/src/chk.erl` and `_build/default/checkouts/chk/src/chk.erl`
- **WHEN** the user goes to the definition of `chk` in `chk:f()`
- **THEN** the target is `_checkouts/chk/src/chk.erl`
- **AND** rebar3 decides: it compiles `_checkouts/chk/src/chk.erl`, and `_build/default/checkouts/chk/src` is a link to that folder
- **AND** the test is `ErlangModuleResolutionTest.testCheckoutWinsOverBuild`

#### Scenario: Release folder of a rebar2 project
- **GIVEN** `rebar.config`, `src/dup.erl` and `rel/mynode/lib/myapp-1.0/src/dup.erl`
- **WHEN** the user goes to the definition of `dup` in `dup:f()`
- **THEN** the target is `src/dup.erl`
- **AND** the build tool decides: it compiles `src/dup.erl` and assembles the release afterwards; nothing is compiled from the release
- **AND** the test is `ErlangModuleResolutionTest.testSourceWinsOverRebar2Release`

#### Scenario: Find Usages of the module
- **GIVEN** `apps/umb/src/dup.erl`, `_build/default/lib/umb/src/dup.erl` and `dup:f()` in `apps/caller/src/caller.erl`
- **WHEN** the user finds the usages of the module `dup` of `apps/umb/src/dup.erl`
- **THEN** the `dup` of `dup:f()` in `caller.erl` is one of them
- **AND** the platform convention decides: Find Usages lists the references that resolve to the element
- **AND** the test is `ErlangFindUsagesTest.testModuleUsageWithBuildCopy`

#### Scenario: Boundary: a directory named rel that is not build output
Unchanged by the change; pinned by its test.
- **GIVEN** `lib/rel/src/dup.erl` and `lib/zzz/src/dup.erl`, and no `rebar.config` in `lib`
- **WHEN** the user goes to the definition of `dup` in `dup:f()`
- **THEN** the target is `lib/rel/src/dup.erl`, the first by path, because neither file is build output
- **AND** nothing outside the plugin decides: two modules of one name are a name clash that the build does not settle
- **AND** the test is `ErlangModuleResolutionTest.testRelWithoutRebarConfigIsSource`

#### Scenario: Boundary: dependency that exists only in build output
Unchanged by the change; pinned by its test.
- **GIVEN** `_build/default/lib/dep/src/dep_mod.erl` and no other file named `dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` and of `f` in `dep_mod:f()` in `src/caller.erl`
- **THEN** both targets are in `_build/default/lib/dep/src/dep_mod.erl`
- **AND** rebar3 decides: it fetches the dependency into that directory and compiles it there
- **AND** the test is `ErlangModuleResolutionTest.testDependencyOnlyInBuild`

### Requirement: Order among copies in build output
When every file with the name of the referenced module is in build output, the plugin SHALL prefer
`_build/default/lib`, where rebar3 keeps the one real copy of a fetched dependency, then any other place under
`_build` outside a release, then a release: `_build/<profile>/rel` and a `rel` directory next to a
`rebar.config`.

#### Scenario: Dependency in the default and in the test profile
- **GIVEN** `_build/default/lib/dep/src/dep_mod.erl` and `_build/test/lib/dep/src/dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` and of `f` in `dep_mod:f()`
- **THEN** both targets are in `_build/default/lib/dep/src/dep_mod.erl`
- **AND** rebar3 decides: it fetches the dependency into `_build/default/lib/dep` and makes `_build/test/lib/dep` a link to it
- **AND** the test is `ErlangFunctionResolutionTest.testDependencyResolvesInDefaultProfile`

#### Scenario: Dependency under a profile and its copy in a release
- **GIVEN** `_build/test/lib/dep/src/dep_mod.erl` and `_build/prod/rel/myrel/lib/dep-1.0/src/dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` in `dep_mod:f()`
- **THEN** the target is `_build/test/lib/dep/src/dep_mod.erl`
- **AND** rebar3 decides: it assembles the release after compilation and compiles nothing from it
- **AND** the test is `ErlangModuleResolutionTest.testProfileLibWinsOverRelease`

#### Scenario: Boundary: dependency in the default profile and in a release
Unchanged by the change; pinned by its test.
- **GIVEN** `_build/default/lib/dep/src/dep_mod.erl` and `_build/prod/rel/myrel/lib/dep-1.0/src/dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` in `dep_mod:f()`
- **THEN** the target is `_build/default/lib/dep/src/dep_mod.erl`
- **AND** rebar3 decides: it assembles the release after compilation and compiles nothing from it
- **AND** the test is `ErlangModuleResolutionTest.testDefaultProfileWinsOverRelease`

### Requirement: Module and function of a remote call resolve in the same file
The plugin SHALL resolve the function name of `Module:Function(...)` and of an `-import` entry, and the type
name of a remote type, in the file that the module name resolves to: the code path that rebar3 builds has one
module of a name. Outside kernel and stdlib a project file wins over an SDK library for both parts. When the
two requirements above do not decide between two files, both parts take the first by path.

#### Scenario: Both parts of a remote call
- **GIVEN** `apps/umb/src/dup.erl` and `_build/default/lib/umb/src/dup.erl`, both exporting `f/0`
- **WHEN** the user goes to the definition of `dup` and then of `f` in `dup:f()`
- **THEN** both targets are in `apps/umb/src/dup.erl`
- **AND** rebar3 decides: the code path has one `dup.beam`, compiled from `apps/umb/src/dup.erl`
- **AND** the test is `ErlangFunctionResolutionTest.testRemoteCallResolvesInSourceNotBuild`

#### Scenario: Import entry
- **GIVEN** the two files of the scenario "Both parts of a remote call" and `-import(dup, [f/0]).` in `apps/caller/src/caller.erl`
- **WHEN** the user goes to the definition of `dup` and then of `f/0` in the attribute
- **THEN** both targets are in `apps/umb/src/dup.erl`
- **AND** rebar3 decides, as in the scenario "Both parts of a remote call"; erlc does not check that the imported module exists
- **AND** the test is `ErlangFunctionResolutionTest.testImportEntryResolvesInSourceNotBuild`

#### Scenario: Remote type
- **GIVEN** `apps/umb/src/dup.erl` and `_build/default/lib/umb/src/dup.erl`, both with `-type t() :: ok.` and `-export_type([t/0]).`
- **WHEN** the user goes to the definition of `dup` and then of `t` in `-spec c() -> dup:t().`
- **THEN** both targets are in `apps/umb/src/dup.erl`
- **AND** rebar3 decides, as in the scenario "Both parts of a remote call"; erlc does not check a remote type
- **AND** the test is `ErlangModuleResolutionTest.testRemoteTypeResolvesInSource`

#### Scenario: Function that only the copy has
- **GIVEN** `apps/umb/src/dup.erl` without `g/0` and a stale `_build/default/lib/umb/src/dup.erl` that exports `g/0`
- **WHEN** the user goes to the definition of `g` in `dup:g()`
- **THEN** there is no target; `g` is not taken from the copy
- **AND** rebar3 decides: the module it compiles has no `g/0`. erlc compiles the call without a diagnostic, because it does not check remote calls
- **AND** the test is `ErlangFunctionResolutionTest.testFunctionOnlyInBuildCopyIsUnresolved`

#### Scenario: Dependency with the name of an OTP application
- **GIVEN** `_build/default/lib/eunit/src/eunit.erl` that exports `test/1`, and an SDK with `lib/eunit-2.2.4/src/eunit.erl`
- **WHEN** the user goes to the definition of `eunit` and then of `test` in `eunit:test(x)` in `src/caller.erl`
- **THEN** both targets are in `_build/default/lib/eunit/src/eunit.erl`
- **AND** rebar3 decides: with a dependency named as an OTP application other than kernel and stdlib, `code:which/1` and `code:lib_dir/1` give the dependency
- **AND** the test is `ErlangFunctionResolutionTest.testDependencyWinsOverSdkLibrary`

#### Scenario: Two applications with a module of one name
- **GIVEN** `lib/aaaa/src/dup.erl` and `lib/zzz/src/dup.erl`, both exporting `f/0`, neither in build output
- **WHEN** the user goes to the definition of `dup` and then of `f` in `dup:f()`
- **THEN** both targets are in `lib/aaaa/src/dup.erl`, the first by path
- **AND** nothing outside the plugin decides: two modules of one name are a name clash that the build does not settle
- **AND** the test is `ErlangFunctionResolutionTest.testSameNameModulesResolveByPath`

### Requirement: include_lib picks the application directory by the same order
When several directories hold an application with the name used in `-include_lib("app/...")`, the plugin SHALL
take the directory outside build output, and among directories in build output the order of the requirement
"Order among copies in build output".

#### Scenario: Header of a project application
- **GIVEN** `apps/umb/include/dup.hrl`, and `_build/default/lib/umb` and `_build/test/lib/umb` each with `ebin/umb.app` and `include/dup.hrl`
- **WHEN** the user goes to the definition of the path in `-include_lib("umb/include/dup.hrl").`
- **THEN** the target is `apps/umb/include/dup.hrl`
- **AND** rebar3 decides: it puts `_build/default/lib/umb` on the code path, and `include` there is a link to `apps/umb/include`
- **AND** the test is `ErlangIncludeLibResolveTest.testProjectAppWinsOverBuild`

#### Scenario: Macro and record from that header
- **GIVEN** the files of the scenario "Header of a project application", and `dup.hrl` defines the macro `DUP` and the record `duprec`
- **WHEN** the user goes to the definition of `?DUP` and of `#duprec{}` in the including file
- **THEN** both targets are in `apps/umb/include/dup.hrl`
- **AND** erlc decides: it takes both definitions from the header that `-include_lib` reads
- **AND** the test is `ErlangIncludeLibResolveTest.testMacroAndRecordFromProjectApp`

#### Scenario: Header of a fetched dependency
- **GIVEN** `_build/default/lib/dep` and `_build/test/lib/dep`, each with `ebin/dep.app` and `include/dep.hrl`
- **WHEN** the user goes to the definition of the path in `-include_lib("dep/include/dep.hrl").`
- **THEN** the target is `_build/default/lib/dep/include/dep.hrl`
- **AND** rebar3 decides: in the default profile `code:lib_dir(dep)` is `_build/default/lib/dep`, and `_build/test/lib/dep` is a link to it
- **AND** the test is `ErlangIncludeLibResolveTest.testDefaultProfileDependencyWinsOverTestProfile`
