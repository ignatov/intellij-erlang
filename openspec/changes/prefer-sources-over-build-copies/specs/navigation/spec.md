# Spec Delta

## Purpose

Which declaration a reference resolves to, and so where "Go to definition" lands, for modules, functions, types,
records, macros and include files. Where the Erlang compiler or the build tool decides the answer, the plugin
gives the same one.

## ADDED Requirements

### Requirement: Source file wins over its copy in build output
When several files have the name of the referenced module, the plugin SHALL resolve the reference to the same
file as rebar3 gives to erlc: a file outside build output. Build output is everything under a `_build`
directory, under a `rel` or `logs` directory next to a `rebar.config`, and under `_build/<profile>/rel`.

#### Scenario: Application of an umbrella project and its view under _build
- **GIVEN** `apps/umb/src/dup.erl` and `_build/default/lib/umb/src/dup.erl`, both `-module(dup).`
- **WHEN** the user goes to the definition of `dup` in `dup:f()` in `apps/caller/src/caller.erl`
- **THEN** the target is `apps/umb/src/dup.erl`
- **AND** erlc does the same: rebar3 passes `apps/umb/src/dup.erl` to it and never the path under `_build`

#### Scenario: Application at the project root
- **GIVEN** `src/dup.erl` and `_build/default/lib/myapp/src/dup.erl`, both `-module(dup).`
- **WHEN** the user goes to the definition of `dup` in `dup:f()` in `src/caller.erl`
- **THEN** the target is `src/dup.erl`
- **AND** erlc does the same: rebar3 passes `src/dup.erl` to it

#### Scenario: Test suite copied by the test profile
- **GIVEN** `apps/umb/test/umb_SUITE.erl` and its copy `_build/test/lib/umb/test/umb_SUITE.erl`
- **WHEN** the user goes to the definition of `umb_SUITE` in `umb_SUITE:all()`
- **THEN** the target is `apps/umb/test/umb_SUITE.erl`
- **AND** erlc does the same: rebar3 passes `apps/umb/test/umb_SUITE.erl` to it and writes only the beam next to the copy

#### Scenario: Checkout dependency
- **GIVEN** `_checkouts/chk/src/chk.erl` and `_build/default/checkouts/chk/src/chk.erl`
- **WHEN** the user goes to the definition of `chk` in `chk:f()`
- **THEN** the target is `_checkouts/chk/src/chk.erl`
- **AND** erlc does the same: rebar3 passes `_checkouts/chk/src/chk.erl` to it

#### Scenario: Release folder of a rebar2 project
- **GIVEN** `rebar.config`, `src/dup.erl` and `rel/mynode/lib/myapp-1.0/src/dup.erl`
- **WHEN** the user goes to the definition of `dup` in `dup:f()`
- **THEN** the target is `src/dup.erl`
- **AND** erlc does the same: it is given `src/dup.erl`; the release folder is assembled after compilation and nothing compiles it

#### Scenario: A directory named rel that is not build output
- **GIVEN** `lib/rel/src/dup.erl` and `lib/zzz/src/dup.erl`, and no `rebar.config` in `lib`
- **WHEN** the user goes to the definition of `dup` in `dup:f()`
- **THEN** the target is `lib/rel/src/dup.erl`, the first by path, because neither file is build output
- **AND** erlc compiles each file it is given; two modules with one name are a build error outside the plugin's reach

### Requirement: Dependency that exists only in build output still resolves
The plugin SHALL resolve a reference to a module whose only file is in build output to that file, the same as
erlc, which finds the dependency through the code path `_build/default/lib/<dep>/ebin` that rebar3 gives it.

#### Scenario: Fetched dependency
- **GIVEN** `_build/default/lib/dep/src/dep_mod.erl` and no other file named `dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` in `dep_mod:f()` in `src/caller.erl`
- **THEN** the target is `_build/default/lib/dep/src/dep_mod.erl`
- **AND** erlc compiles the call: the dependency is compiled from that directory and is on the code path

### Requirement: Order among copies in build output
When every file with the name of the referenced module is in build output, the plugin SHALL prefer
`_build/default/lib`, where rebar3 keeps the fetched dependency that erlc compiles, then any other place under
`_build`, then release and log folders. Among files of equal rank it SHALL take the first by path, so that the
result does not depend on the order in which the index returns the files.

#### Scenario: Dependency in the default and in the test profile
- **GIVEN** `_build/default/lib/dep/src/dep_mod.erl` and `_build/test/lib/dep/src/dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` in `dep_mod:f()`
- **THEN** the target is `_build/default/lib/dep/src/dep_mod.erl`
- **AND** erlc does the same: rebar3 fetches the dependency into `_build/default/lib` and compiles it there

#### Scenario: Dependency in the default profile and in a release
- **GIVEN** `_build/default/lib/dep/src/dep_mod.erl` and `_build/prod/rel/myrel/lib/dep-1.0/src/dep_mod.erl`
- **WHEN** the user goes to the definition of `dep_mod` in `dep_mod:f()`
- **THEN** the target is `_build/default/lib/dep/src/dep_mod.erl`
- **AND** erlc does the same: the release is assembled after compilation and erlc never reads it

### Requirement: Module and function of a remote call resolve in the same file
The plugin SHALL resolve the function name of `Module:Function(...)`, of a `-import` entry and of a remote type
in the file that the module name resolves to, the same as erlc, for which a module name stands for one module.

#### Scenario: Both parts of a remote call
- **GIVEN** `apps/umb/src/dup.erl` and `_build/default/lib/umb/src/dup.erl`, both exporting `f/0`
- **WHEN** the user goes to the definition of `dup` and then of `f` in `dup:f()`
- **THEN** both targets are in `apps/umb/src/dup.erl`
- **AND** erlc compiles the call against one module `dup`

#### Scenario: Function that only the copy has
- **GIVEN** `apps/umb/src/dup.erl` without `g/0` and a stale `_build/default/lib/umb/src/dup.erl` that exports `g/0`
- **WHEN** the user goes to the definition of `g` in `dup:g()`
- **THEN** there is no target; `g` is not taken from the copy
- **AND** erlc compiles the call without a diagnostic, and the module that rebar3 compiles has no `g/0`

### Requirement: include_lib picks the application directory by the same order
When several directories hold an application with the name used in `-include_lib("app/...")`, the plugin SHALL
take the directory outside build output, and among directories in build output the order of the requirement
"Order among copies in build output". The file it reaches is the one erlc reads.

#### Scenario: Header of a project application
- **GIVEN** `apps/umb/include/dup.hrl`, and `_build/default/lib/umb` and `_build/test/lib/umb` each with `ebin/umb.app` and `include/dup.hrl`
- **WHEN** the user goes to the definition of the path in `-include_lib("umb/include/dup.hrl").`
- **THEN** the target is `apps/umb/include/dup.hrl`
- **AND** erlc reads `_build/default/lib/umb/include/dup.hrl`, which rebar3 makes a link to `apps/umb/include/dup.hrl`

#### Scenario: Macro and record from that header
- **GIVEN** the files of the previous scenario, and `dup.hrl` defines the macro `DUP` and the record `duprec`
- **WHEN** the user goes to the definition of `?DUP` and of `#duprec{}` in the including file
- **THEN** both targets are in `apps/umb/include/dup.hrl`
- **AND** erlc takes both definitions from the header it read in the previous scenario

#### Scenario: Header of a fetched dependency
- **GIVEN** `_build/default/lib/dep` and `_build/test/lib/dep`, each with `ebin/dep.app` and `include/dep.hrl`
- **WHEN** the user goes to the definition of the path in `-include_lib("dep/include/dep.hrl").`
- **THEN** the target is `_build/default/lib/dep/include/dep.hrl`
- **AND** erlc reads the same file: `code:lib_dir(dep)` is `_build/default/lib/dep` under rebar3
