# Proposal

## Why

In a rebar3 project the build keeps a second view of the sources under `_build`, and the plugin sends "Go to
definition" there instead of to the file the developer edits
([#1040](https://github.com/ignatov/intellij-erlang/issues/1040)). The issue asks to prefer the files that are
not in the rebar build directory and, in its notes, to exclude the folders of `_build` that are built from the
project's own sources, the test `logs/` and `rel/`, but not the downloaded dependencies. The project's triage
of the issue, which is not part of the issue itself, sums it up as "Needs a ranking or exclusion policy for
_build, deps, logs and rel".

What goes wrong for the developer who lands in `_build`:

- The IDE treats the path under `_build` as another file. Find Usages of the module, started from the real
  source, finds nothing: the reference in the call belongs to the copy (table below).
- Where `_build` holds a real copy (a test suite under `_build/test/lib/<app>/test`, the sources in a release),
  an edit there is lost with the next build. Where it holds a link (`src` and `include` of an application), the
  edit reaches the real file, but under a path that the version control and the project view do not show.

Observed on master in a light fixture test, nothing excluded, with `apps/umb/src/dup.erl`, the same file under
`_build/default/lib/umb/src`, a caller in `apps/caller/src`, and `apps/umb/include/dup.hrl` with copies under
`_build/default/lib/umb` and `_build/test/lib/umb`:

| Reference | Resolves to today |
|---|---|
| `dup` in `dup:f()` and in `-import(dup, [f/0])` | `_build/default/lib/umb/src/dup.erl` |
| `f` in `dup:f()` and `f/0` in `-import(dup, [f/0])` | `apps/umb/src/dup.erl` |
| `dup` and `t` in the remote type `dup:t()` | both `_build/default/lib/umb/src/dup.erl` |
| `g` in `dup:g()`, when only the copy under `_build` has `g/0` | the copy |
| `-include_lib("umb/include/dup.hrl")`, and the macro and the record it defines | the application directory that was created last: `_build/test/lib/umb` when the files are created in the order `apps`, `_build/default`, `_build/test`; `apps/umb` in the reverse order |
| Find Usages of the module `dup`, started in `apps/umb/src/dup.erl` | no usage; the `dup` of `dup:f()` is listed for the copy |
| Find Usages of `f/0`, started in `apps/umb/src/dup.erl` | the call `dup:f()` is listed |

Two more, with a dependency `dep` under `_build/default/lib/dep` and `_build/test/lib/dep`, and with a
dependency that has the name of the OTP application `eunit`:

| Reference | Resolves to today |
|---|---|
| `dep_mod` in `dep_mod:f()` | `_build/default/lib/dep/src/dep_mod.erl` |
| `f` in `dep_mod:f()` | `_build/test/lib/dep/src/dep_mod.erl` |
| `eunit` in `eunit:test(x)` | `_build/default/lib/eunit/src/eunit.erl` |
| `test` in `eunit:test(x)` | `lib/eunit-2.2.4/src/eunit.erl` of the SDK |

The two halves of one call go to different files because two orders exist: module references sort the
candidates by path, so `_build` comes before `apps` and `src`; remote calls take the order of
`ErlangModuleIndex` (source roots, libraries, then shorter paths). `-include_lib` takes the last application
directory that the index reports. A review of this proposal observed that with `_build` excluded all of the
first table resolve to `apps/umb`.

What the build tool does, rebar3 3.28.0 on OTP 27, on Linux:

- It compiles `apps/umb/src/umb.erl`: the `source` in the compile info of the beam is that path, never a path
  under `_build`. The same holds for a test suite (`apps/umb/test/umb_SUITE.erl`, of which
  `_build/test/lib/umb/test` holds a real copy) and for a checkout (`_checkouts/<dep>/src`).
- `_build/<profile>/lib/<app>/src` and `include` are links to the application's own folders.
- A fetched dependency has its one real copy in `_build/default/lib/<dep>`. `_build/test/lib/<dep>` and
  `_build/prod/lib/<dep>` are links to it.
- A release (`_build/<profile>/rel/<name>/lib/<app>-<vsn>`) holds real copies of `src` and `include` when it
  includes sources. It is assembled after compilation; nothing is compiled from it.
- `_build/test/logs` holds HTML, CSS, JavaScript and log files after `rebar3 ct`: no `.erl`, `.hrl` or `.app`
  file, so nothing in it can be a target and this proposal has no rule for `logs`.
- The code path that `rebar3 path` prints has the `ebin` folders under `_build/default`. A dependency with the
  name of an OTP application other than kernel and stdlib (tried with `xmerl`, as a git dependency and as a
  checkout) wins over the OTP one: `code:lib_dir(xmerl)` and `code:which(xmerl_scan)` give the dependency, and
  `-include_lib("xmerl/include/...")` reads its header. A dependency named `stdlib` makes rebar3 crash.

Not run: rebar2 (its `rel/` folder is taken from its documentation), a hex dependency, Windows, and which of
the linked paths erlc opens for `-include_lib`.

## What Changes

- One order for every lookup that can find several files for a module name or several directories for an
  application name: kernel and stdlib of the SDK first, as today; then the project before the other SDK
  libraries; then files outside build output; then `_build/default/lib`; then the rest of `_build` outside a
  release; then releases. Ties are broken by path, so the answer does not depend on the order in which the
  index returns files.
- The module part and the function part of a remote call and of an `-import` entry use that one order, so they
  end in the same file. A remote type already follows its module reference and moves with it.
- `-include_lib("app/...")` picks the application directory by the same order.
- Nothing is excluded and nothing is removed from the indices. A dependency that exists only under
  `_build/default/lib` resolves as it does today.
- Not changed: kernel and stdlib first, the choice between versions of one application, and `-include` in
  IntelliJ IDEA, which looks next to the including file and in the include folders of the module and never
  asks the index. In a small IDE `-include` has one more step that does; see Impact.

Rejected, with the reasons in design.md:

- Excluding `_build` for every project: it hides the fetched dependencies, which the issue asks to keep.
- Ranking by `.gitignore`, which the issue suggests: ignored does not mean copied (`_checkouts` is ignored by
  the rebar3 template and is the source to edit).
- Treating two paths as one file when a link joins them: does not cover the real copies or Windows.
- Leaving `_build` out of `ErlangModuleIndex`: the same loss of dependencies, and an index version bump.

## Capabilities

### New Capabilities

None.

### Modified Capabilities
- `navigation`: new requirements for the choice between several files with the same module name and between
  several directories with the same application name. No existing requirement of
  `openspec/specs/navigation/spec.md` changes.

## Impact

The change is in two shared lookups, so every caller of them is listed.

`ErlangModuleIndex.getVirtualFilesByName`, `getFilesByName` and `getModulesByName` (the order of the files with
one module name):

| Caller | What changes |
|---|---|
| `ErlangModuleReferenceImpl.resolveInner` | The target of a module reference. Its own comparator goes away |
| `ErlangFunctionReferenceImpl.getExternalFunction` | The target of the function of a remote call and of an `-import` entry; only the first file counts (open decision 6) |
| `ErlangFunctionReferenceImpl.getErlangFunctionsFromModule` | The order of the candidates of a multi-resolve; the set is the same |
| `ErlangTypeReferenceImpl` | Follows the module reference, as today |
| `ErlangModulesUtil.getErlangModule`, used by `ErlangApplicationConfiguration.checkConfiguration` | The file in which a run configuration looks for its entry function. The scope is one IDE module |
| `ErlangModuleContributor.getItemsByName` | The order of the entries of one name in Go to Class. All copies are still listed |
| `ErlangDebugLocationResolver.resolveModuleFile` | The file in which the debugger shows a stack frame and resolves a breakpoint |
| `ErlangTestLocationProvider` | The order of the files that a test result links to; the set is the same |
| `ErlangPrepareDependenciesCompileTask.getPathsFromModuleName` | The order of the paths of a parse transform or behaviour module in the build order; the set is the same. This is build code: the implementation runs `testCompilation` |
| `ErlangFindIncludeQuickFix` | The order of the files offered; the set is the same |
| `ErlangPsiImplUtil.getExternalFunctionForCompletion` | Nothing: it collects the exported functions of every file of the name |

`ErlangApplicationIndex.getApplicationDirectoryByName` and `getAllApplicationDirectories` (the directory of an
application name; both use the same choice):

| Caller | What changes |
|---|---|
| `ErlangPsiImplUtil.getDirectlyIncludedFiles` | The target of `-include_lib`, and so of the macros, records and types that come from the header |
| `ErlangCompletionContributor.getApplicationDirectories` | The directory whose files the completion of an `-include_lib` path offers: today the one the index reports last, then the ranked one |
| `ErlangPsiImplUtil.getContainingOtpAppRoot`, used only in a small IDE by the last step of `-include` resolve (`getDirectlyIncludedFiles`) and of `-include` path completion | It finds the application root of a file only when that file's directory is the one chosen for the application name. Today that is the directory the index reports last; then it is `apps/<app>`, so the step works for the project's files and no longer for files inside a copy. Read from the code, not tested |
| The comparators of `ErlangModuleIndex` and `ErlangModuleReferenceImpl` | They ask for the kernel and stdlib directories; the SDK has one of each |

Further:

- Find Usages and Rename follow resolve. A module reference such as `dup` in `dup:f()` becomes a usage of the
  module in `apps/umb/src/dup.erl` and is renamed with it; today it belongs to the copy. Function usages of
  that call are found today and stay. Files under `_build` are still indexed, so usages inside the copies are
  still listed and renamed when the project does not exclude `_build`.
- Completion: the one change is the `-include_lib` path completion in the table. Module names are already
  listed once; functions offered after `dup:` are collected from every file named `dup` as today.
- Links inside quick documentation look files up by name on their own (`ErlangDocumentationProvider`) and are
  not changed. Go to Symbol still lists the copies.
- Indices: no key, value or filter changes, so no index version and no stub version bump.
- Rebar import: no change. It already excludes `_build` and `.rebar3` of each imported application.
- Differences from erlc: none. erlc does not choose between files; rebar3 hands it one source file per module
  and one code path, and the plugin picks the file that rebar3 compiles and the directory first on that path.

## Open decisions

1. **Ranking only, or exclusion too?** Proposed: ranking only in this change. It works however the project was
   opened and for projects that exist already. The issue also asks to exclude, at import, the folders of
   `_build` that are built from the project's own applications, test `logs/` and `rel/`. That changes what is
   indexed and what the import wizard offers (today it lists `_build/default/lib/<app>` as a second module with
   the same name), and deserves its own proposal under the `rebar` capability. Alternative: add it here.
2. **How `rel` is recognised.** `_build` and `_checkouts` are rebar3's own names and are recognised anywhere.
   `rel` is an ordinary name. Proposed: a `rel` directory next to a `rebar.config`, and `rel` inside
   `_build/<profile>`. Alternative: any directory with that name.
3. **`deps/` (rebar2).** Proposed: no rule. It holds the only copy of a rebar2 dependency and ranks as project
   source, as today. Alternative: rank it with `_build/default/lib`, below project applications.
4. **A custom `base_dir` in `rebar.config`.** Proposed: only the default name `_build` is recognised; a project
   with another `base_dir` keeps today's behaviour. Alternative: read `base_dir` from the `rebar.config` at the
   content root with `RebarConfigUtil` and treat that directory as `_build`. It costs reading and caching a
   file in resolve, and still misses a `base_dir` that comes from a profile or from `REBAR_BASE_DIR`.
5. **Usages inside the copies.** Proposed: keep listing them (see Impact). Alternative: filter usages by the
   same ranking, which hides real files from a search.
6. **A function that only a lower-ranked file has.** Today `dup:g()` resolves if any file named `dup` has
   `g/0`. Proposed: only the file that `dup` resolves to counts, because one module of that name is on the
   code path. A project with two unrelated modules of one name in different applications then gets "unresolved
   function" for the functions of the second one. Alternative: keep searching the other files and accept that
   the two halves can differ.
