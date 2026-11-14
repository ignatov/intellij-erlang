## Purpose
Which declaration a reference resolves to, for modules, functions, types, records, macros and include files,
and what resolve feeds: where "Go to definition" lands, what Find Usages lists, what Rename changes and which
references the plugin reports as unresolved. Where the Erlang compiler or the build tool decides the answer,
the plugin gives the same one.

## ADDED Requirements

### Requirement: Export entry resolves only to a function of its own module
The plugin SHALL resolve an entry of an `-export` attribute the same as erlc: only to a function of the module
itself. An `-import` attribute and the auto-imported BIFs do not satisfy the entry: it is unresolved and the
plugin reports `Unresolved function <name>/<arity>`, where erlc prints `function <name>/<arity> undefined`.
The boundary scenarios say what still counts as a function of the module.

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

#### Scenario: Boundary: function defined in an included file
Unchanged by the fix; pinned by its tests.
- **GIVEN** `m.hrl` with `from_hrl() -> ok.` and `-module(m). -include("m.hrl"). -export([from_hrl/0]).`
- **WHEN** the entry `from_hrl/0` is resolved
- **THEN** it resolves to `from_hrl/0` in `m.hrl` and the plugin reports nothing for it
- **AND** erlc prints no `undefined` for the entry either; it rejects this module for the order of its forms, `attribute export after function definitions`, and compiles it when the `-include` follows the `-export`. This requirement does not cover the order of the forms
- **AND** the tests are `ErlangHighlightingTest.test536`, `ErlangHighlightingTest.testExportFromInclusion` and `ErlangFunctionResolutionTest.testExportOfIncludedFunction`

#### Scenario: Boundary: function that the module both imports and defines
Unchanged by the fix; pinned by its test.
- **GIVEN** `-module(m). -import(incl, [foo/0]). -export([foo/0]). foo() -> ok.`
- **WHEN** the entry `foo/0` is resolved
- **THEN** it resolves to `foo/0` of `m`, not to `incl:foo/0`
- **AND** erlc prints no `undefined` for the entry either; it rejects the module for the definition, `defining imported function foo/0`, which this requirement does not cover
- **AND** the test is `ErlangFunctionResolutionTest.testExportOfLocalImportedFunction`

#### Scenario: Boundary: functions that the compiler generates
Unchanged by the fix; pinned by its test.
- **GIVEN** `-module(m). -export([module_info/0, module_info/1, record_info/2]).`
- **WHEN** the file is highlighted
- **THEN** the plugin reports none of the three entries as unresolved, although the module does not define them
- **AND** erlc compiles the module; it adds two warnings, `function module_info/0 already exported` and `function module_info/1 already exported`, which this requirement does not cover
- **AND** the test is `ErlangHighlightingTest.testExportPseudoFunctions`

#### Scenario: Boundary: module_info/2 is not generated
Unchanged by the fix; pinned by its test.
- **GIVEN** `-module(m). -export([module_info/2]).`
- **WHEN** the file is highlighted
- **THEN** the plugin reports `Unresolved function module_info/2`
- **AND** erlc does the same: `function module_info/2 undefined`
- **AND** the test is `ErlangHighlightingTest.testExportPseudoFunctions`, which checks this arity and no other

### Requirement: Export entry of an importing module is not a usage of the imported function
Find Usages and Rename of a function SHALL follow the resolve of export entries: the `-export` entry of another
module that imports the function is not a usage and is not renamed, because erlc reads that entry as a function
of the other module. The `-import` entry and the calls in that module are usages and are renamed.

#### Scenario: Find Usages
- **GIVEN** `a.erl` with `-module(a). -export([xxx/0]). xxx() -> ok.` and `b.erl` with `-module(b). -import(a, [xxx/0]). -export([xxx/0]). foo() -> xxx().`
- **WHEN** the user finds the usages of `xxx/0` of `a`
- **THEN** there are three usages: the export entry in `a.erl`, the import entry in `b.erl` and the call in `b.erl`. The export entry in `b.erl` is not one
- **AND** erlc does not take that entry for `a:xxx/0` either: for `b.erl` it prints `function xxx/0 undefined`
- **AND** the test is `ErlangFindUsagesTest.testImportedFunctionUsages`, which asserts the number of usages, three, and not which they are

#### Scenario: Rename
- **GIVEN** the files `a.erl` and `b.erl` of the scenario "Find Usages"
- **WHEN** the user renames `xxx/0` of `a` to `bar`
- **THEN** `a.erl` is `-module(a). -export([bar/0]). bar() -> ok.` and `b.erl` is `-module(b). -import(a, [bar/0]). -export([xxx/0]). foo() -> bar().`
- **AND** erlc prints `function xxx/0 undefined` for `b.erl` before and after the rename: the entry never named the renamed function
- **AND** the test is `ErlangRenameTest.testImportedFunctionInExport`
