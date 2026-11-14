# Proposal

## Why

In a rebar3 project the build keeps a second view of the sources under `_build`, and the plugin sends "Go to
definition" there instead of to the file the developer edits
([#1040](https://github.com/ignatov/intellij-erlang/issues/1040), triage note: "Needs a ranking or exclusion
policy for _build, deps, logs and rel"). A developer who lands in `_build` edits a file that the next build
overwrites, and Find Usages and Rename started from the real source miss the call sites.

Observed on master with `apps/umb/src/dup.erl`, the same file under `_build/default/lib/umb/src` and
`_build/test/lib/umb/src`, and `-include_lib("umb/include/dup.hrl")`, nothing excluded:

| Reference | Resolves to today |
|---|---|
| `dup` in `dup:f()` | `_build/default/lib/umb/src/dup.erl` |
| `f` in `dup:f()` | `apps/umb/src/dup.erl` |
| `-include_lib("umb/include/dup.hrl")`, and the macro and the record it defines | `_build/test/lib/umb/include/dup.hrl` |

The two halves of one call go to different files because two orders exist: module references sort the
candidates by path, so `_build` comes before `apps` and `src`; remote calls take the order of
`ErlangModuleIndex` (source roots, then shorter paths). `-include_lib` takes whichever application directory the
index returns last. With `_build` excluded all five resolve to `apps/umb`.

What the tools do: rebar3 3.28.0 compiles `apps/umb/src/umb_app.erl` (the `source` in the compile info of the
beam is that path), never a path under `_build`. On Linux `_build/<profile>/lib/<app>/src` and `include` are
links to the application's own folders. Real copies exist too: `_build/test/lib/<app>/test/*.erl`, and
`lib/<app>-<vsn>/include` in a production release. erlc reads `-include_lib` through the code path that rebar3
gives it, `_build/default/lib/umb/include`, which is the same file as `apps/umb/include`.

## What Changes

- One order for every lookup that can find several files for a module name or several directories for an
  application name: files outside build output first, then `_build/default/lib` (where rebar3 keeps fetched
  dependencies), then the rest of `_build`, then release and log folders. Ties are broken by path, so the
  answer does not depend on the order in which the index returns files.
- The module part and the function part of a remote call, a remote type and `-import` use that one order, so
  they end in the same file.
- `-include_lib("app/...")` picks the application directory by the same order.
- Nothing is excluded and nothing is removed from the indices. A dependency that exists only under
  `_build/default/lib` resolves as it does today.
- Not changed: kernel and stdlib first, project files before SDK libraries, the choice between versions of one
  application, and `-include`, which never searches the index.

Rejected, with the reasons in design.md:

- Excluding `_build` for every project: it hides the fetched dependencies, which the issue asks to keep.
- Ranking by `.gitignore`: ignored does not mean copied (`_checkouts` is ignored and is the source to edit).
- Treating two paths as one file when a link joins them: does not cover the real copies or Windows.
- Leaving `_build` out of `ErlangModuleIndex`: the same loss of dependencies, and an index version bump.

## Capabilities

### New Capabilities
- `navigation`: which declaration a reference resolves to and where "Go to definition" lands (issue label
  `feat:Navigation`, tests in `testData/resolve` and `testData/navigation`). This change adds the choice between
  several files with the same module name or application name.

### Modified Capabilities

None. `openspec/specs/` is empty.

## Impact

- Code: `ErlangModuleIndex` (its file comparator), `ErlangModuleReferenceImpl` (its own comparator goes away),
  `ErlangApplicationIndex.getApplicationDirectoryByName`. `ErlangFunctionReferenceImpl`, remote types and
  records, the debugger and eunit location lookups follow because they already ask these two.
- Go to definition and everything else that resolves these references follow the new target. Links inside
  quick documentation look files up by name on their own (`ErlangDocumentationProvider`) and are not changed.
- Find Usages and Rename: a call such as `dup:f()` now counts as a usage of `f/0` in `apps/umb/src/dup.erl`,
  and renaming that function updates the call. Files under `_build` are still indexed, so usages inside the
  copies are still listed and renamed when the project does not exclude `_build`.
- Completion: no change. Module names are already listed once; functions offered after `dup:` are collected
  from every file named `dup` as today.
- Go to Class and Go to Symbol still list the copies.
- Indices: no key, value or filter changes, so no index version and no stub version bump.
- Rebar import: no change. It already excludes `_build` and `.rebar3` of each imported application.
- Differences from erlc: none intended. erlc and rebar3 never see two source files for one module; the IDE
  does, and picks the one rebar3 compiles.

## Open decisions

1. **Ranking only, or exclusion too?** Proposed: ranking only in this change. It works however the project was
   opened and for projects that exist already. The issue also asks to exclude, at import, the folders of
   `_build` that are built from the project's own applications, test `logs/` and `rel/`. That changes what is
   indexed and what the import wizard offers (today it lists `_build/default/lib/<app>` as a second module with
   the same name), and deserves its own proposal under the `rebar` capability. Alternative: add it here.
2. **How `rel` and `logs` are recognised.** `_build` and `_checkouts` are rebar3's own names and are recognised
   anywhere. `rel` and `logs` are ordinary names. Proposed: only a `rel` or `logs` directory next to a
   `rebar.config`, and `rel` inside `_build/<profile>`. Alternative: any directory with that name.
3. **`deps/` (rebar2).** Proposed: no rule. It holds the only copy of a rebar2 dependency and ranks as project
   source, as today. Alternative: rank it with `_build/default/lib`, below project applications.
4. **A custom `base_dir` in `rebar.config`.** Proposed: not read; only the default name `_build` is recognised.
5. **Usages inside the copies.** Proposed: keep listing them (see Impact). Alternative: filter usages by the
   same ranking, which hides real files from a search.
6. **A function that only a lower-ranked file has.** Today `dup:g()` resolves if any file named `dup` has
   `g/0`. Proposed: only the file that `dup` resolves to counts, as for erlc. A project with two unrelated
   modules of one name in different applications then gets "unresolved function" for the functions of the
   second one. Alternative: keep searching the other files and accept that the two halves can differ.
