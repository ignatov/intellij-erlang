# navigation Specification

## Purpose
Which declaration a reference resolves to, and so where "Go to definition" lands, for modules, functions, types,
records, macros and include files. Where the Erlang compiler or the build tool decides the answer, the plugin
gives the same one.

## Requirements

### Requirement: Export entry resolves only to a function of its own module
The plugin SHALL resolve an entry of an `-export` attribute the same as erlc: only to a function that the module
defines, in its own file or in a file it includes. An `-import` attribute and the auto-imported BIFs do not
satisfy the entry: it is unresolved and the plugin reports `Unresolved function <name>/<arity>`, where erlc
prints `function <name>/<arity> undefined`.

#### Scenario: Imported function
- **GIVEN** `-module(m). -import(io, [format/1]). -export([format/1]).`
- **WHEN** the entry `format/1` is resolved
- **THEN** it is unresolved and the plugin reports `Unresolved function format/1`
- **AND** erlc does the same: `function format/1 undefined`
- **AND** the tests are `ErlangHighlightingTest.test536` and `ErlangFunctionResolutionTest.testExportOfImportedFunction`

#### Scenario: Auto-imported BIF
- **GIVEN** `-module(m). -export([self/0]).`
- **WHEN** the entry `self/0` is resolved
- **THEN** it is unresolved and the plugin reports `Unresolved function self/0`
- **AND** erlc does the same: `function self/0 undefined`
- **AND** the tests are `ErlangHighlightingTest.test536` and `ErlangFunctionResolutionTest.testExportOfAutoImportedBif`

#### Scenario: Function imported by an included file
- **GIVEN** `imports.hrl` with `-import(testapp, [foo/0]).` and `-module(m). -include("imports.hrl"). -export([foo/0]).`
- **WHEN** the entry `foo/0` is resolved
- **THEN** it is unresolved and the plugin reports `Unresolved function foo/0`
- **AND** erlc does the same: `function foo/0 undefined`
- **AND** the test is `ErlangHighlightingTest.testExportFromInclusion`

#### Scenario: Function defined in an included file
- **GIVEN** `m.hrl` with `from_hrl() -> ok.` and `-module(m). -include("m.hrl"). -export([from_hrl/0]).`
- **WHEN** the entry `from_hrl/0` is resolved
- **THEN** it resolves to `from_hrl/0` in `m.hrl` and the plugin reports nothing for it
- **AND** erlc prints no `undefined` for the entry either; it rejects this module for the order of its forms, `attribute export after function definitions`, and compiles it when the `-include` follows the `-export`. This requirement does not cover the order of the forms
- **AND** the tests are `ErlangHighlightingTest.test536`, `ErlangHighlightingTest.testExportFromInclusion` and `ErlangFunctionResolutionTest.testExportOfIncludedFunction`

#### Scenario: Function that the module both imports and defines
- **GIVEN** `-module(m). -import(incl, [foo/0]). -export([foo/0]). foo() -> ok.`
- **WHEN** the entry `foo/0` is resolved
- **THEN** it resolves to `foo/0` of `m`, not to `incl:foo/0`
- **AND** erlc prints no `undefined` for the entry either; it rejects the module for the definition, `defining imported function foo/0`, which this requirement does not cover
- **AND** the test is `ErlangFunctionResolutionTest.testExportOfLocalImportedFunction`

### Requirement: Export entry may name a function that the compiler generates
The plugin SHALL accept, the same as erlc, an `-export` entry for `module_info/0`, `module_info/1` and
`record_info/2` in a module that does not define them. The same name with another arity is an ordinary entry
and is unresolved.

#### Scenario: Generated functions
- **GIVEN** `-module(m). -export([module_info/0, module_info/1, record_info/2]).`
- **WHEN** the file is highlighted
- **THEN** the plugin reports none of the three entries as unresolved
- **AND** erlc compiles the module; for `module_info/0` and `module_info/1` it adds the warning `function module_info/0 already exported`, which this requirement does not cover
- **AND** the test is `ErlangHighlightingTest.testExportPseudoFunctions`

#### Scenario: Name of a generated function with another arity
- **GIVEN** `-module(m). -export([module_info/2]).`
- **WHEN** the file is highlighted
- **THEN** the plugin reports `Unresolved function module_info/2`
- **AND** erlc does the same: `function module_info/2 undefined`
- **AND** the test is `ErlangHighlightingTest.testExportPseudoFunctions`

### Requirement: Export entry of an importing module is not a usage of the imported function
Find Usages and Rename of a function SHALL follow the resolve of export entries: the `-export` entry of another
module that imports the function is not a usage and is not renamed, because erlc reads that entry as a function
of the other module. The `-import` entry and the calls in that module are usages and are renamed.

#### Scenario: Find Usages
- **GIVEN** `a.erl` with `-module(a). -export([xxx/0]). xxx() -> ok.` and `b.erl` with `-module(b). -import(a, [xxx/0]). -export([xxx/0]). foo() -> xxx().`
- **WHEN** the user finds the usages of `xxx/0` of `a`
- **THEN** there are three: the export entry in `a.erl`, the import entry in `b.erl` and the call in `b.erl`. The export entry in `b.erl` is not one
- **AND** erlc does not take that entry for `a:xxx/0` either: for `b.erl` it prints `function xxx/0 undefined`
- **AND** the test is `ErlangFindUsagesTest.testImportedFunctionUsages`

#### Scenario: Rename
- **GIVEN** the files `a.erl` and `b.erl` of the scenario "Find Usages"
- **WHEN** the user renames `xxx/0` of `a` to `bar`
- **THEN** `a.erl` is `-module(a). -export([bar/0]). bar() -> ok.` and `b.erl` is `-module(b). -import(a, [bar/0]). -export([xxx/0]). foo() -> bar().`
- **AND** erlc prints `function xxx/0 undefined` for `b.erl` before and after the rename: the entry never named the renamed function
- **AND** the test is `ErlangRenameTest.testImportedFunctionInExport`
