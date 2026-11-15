# Proposal

## Why

In a rebar3 project the build keeps a second view of the sources under `_build`, and the plugin sends "Go to
definition" there instead of to the file the developer edits
([#1040](https://github.com/ignatov/intellij-erlang/issues/1040)). The issue names two lookups, "Go to symbol
definition" and "Include files for `-include`", and asks to prefer the results that are not in the rebar build
directory. In its notes it asks to exclude the folders of `_build` that are built from the project's own
sources, the test `logs/` and `rel/`, but not the downloaded dependencies. This project's own triage of the
issue, which is not public, sums it up as the need for a ranking or exclusion policy for `_build`, `deps`,
`logs` and `rel`.

What goes wrong for the developer who lands in `_build`:

- The IDE treats the path under `_build` as another file. Find Usages of the module, started from the real
  source, finds nothing: the reference in the call belongs to the copy (table below).
- Where `_build` holds a real copy (a test suite under `_build/test/lib/<app>/test`, the sources in a release
  that includes them), an edit there is lost with the next build. Where it holds a link (`src` and `include` of an application), the
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

With a release that holds copies of OTP applications under `_build/prod/rel/myrel/lib`, against the mock SDK of
the tests (eunit-2.2.4, stdlib-1.18.2):

| Reference | Resolves to today |
|---|---|
| `eunit` in `eunit:test(x)`, with `eunit-2.2.4/src/eunit.erl` in the release | the release copy |
| `test` in `eunit:test(x)` | `lib/eunit-2.2.4/src/eunit.erl` of the SDK |
| `lists` and `reverse` in `lists:reverse([])`, with `stdlib-6.2/ebin/stdlib.app` and `stdlib-6.2/src/lists.erl` in the release | both the release copy: its directory has the higher version and is taken for stdlib |
| The same with `stdlib-1.18.2` in the release, the version of the SDK | both the SDK |
| `-include_lib("eunit/include/eunit.hrl")`, with `eunit-2.9/ebin/eunit.app` and `eunit-2.9/include/eunit.hrl` in the release | the release copy, the higher version |
| The same with `eunit-2.2.4` in the release | the SDK |
| `eunit_new` and `f` in `eunit_new:f()`, with `eunit-2.9/src/eunit_new.erl` in the release and no module of that name in the SDK | both the release copy, the only file of that name |

And where neither file is build output:

| Reference | Resolves to today |
|---|---|
| `dup` and `f` in `dup:f()`, with `lib/aaaa/src/dup.erl` and `lib/zzz/src/dup.erl` | `dup` in `lib/aaaa`, `f` in `lib/zzz` |
| The same, with `lib/zzz/src` as the only source root | both in `lib/zzz` |
| `dup` and `f` in `dup:f()`, with `.eunit/dup.erl` and `src/dup.erl` | `dup` in `.eunit`, `f` in `src` |
| `dup` and `f` in `dup:f()`, with `other/dup.erl` outside every source root as the only file of that name | `dup` is unresolved, `f` in `other/dup.erl` |

And in a heavy test with two IDE modules, `zzz_a` and `aaa_b`, side by side in one directory, each with the
source root `src`, and no dependency between them. The caller is `zzz_a/src/caller.erl`:

| Reference | Resolves to today |
|---|---|
| `dup` and `f` in `dup:f()`, with `zzz_a/src/dup.erl` that exports `f/0` and `aaa_b/src/dup.erl` that does not | both in `zzz_a/src/dup.erl` |
| `b_mod` and `g` in `b_mod:g()`, with `aaa_b/src/b_mod.erl` as the only file of that name | `b_mod` is unresolved, `g` in `aaa_b/src/b_mod.erl` |

The two halves of one call go to different files because two orders exist: module references sort the
candidates by path, so `_build` comes before `apps` and `src`; remote calls take the order of
`ErlangModuleIndex` (source roots, not hidden, libraries, then shorter paths) and the first file in it that has
the function. The two also search different scopes: the module part the IDE module of the caller, its
dependencies and the libraries, the function part the whole project. `-include_lib` takes the last
application directory that the index reports, or the one with the higher version.

With `_build` excluded (the same probe, with `PsiTestUtil.addExcludedRoot`), every row of the first table
resolves to `apps/umb`, except `g` in `dup:g()`: it is unresolved, because only the copy has `g/0`. Find Usages
of the module, started in `apps/umb/src/dup.erl`, then lists the four module references of the caller.

What the build tool does, rebar3 3.28.0 on OTP 27, on Linux:

- It compiles `apps/umb/src/umb.erl`: the `source` in the compile info of the beam is that path, never a path
  under `_build`. The same holds for a test suite (`apps/umb/test/umb_SUITE.erl`, of which
  `_build/test/lib/umb/test` holds a real copy) and for a checkout (`_checkouts/<dep>/src`).
- `_build/<profile>/lib/<app>/src` and `include` are links to the application's own folders.
- A fetched dependency has its one real copy in `_build/default/lib/<dep>`. `_build/test/lib/<dep>` and
  `_build/prod/lib/<dep>` are links to it.
- A release (`_build/<profile>/rel/<name>/lib/<app>-<vsn>`) is assembled after compilation; nothing is compiled
  from it. What it holds depends on the relx options:
  - `{mode, dev}`, which the template sets: every entry is a link, to `_build/<profile>/lib/<app>` for the
    project's application and to the directory of the SDK for kernel, stdlib and sasl.
  - `{mode, prod}`: real directories with `ebin`, and a copy of `include` for every application that has
    that folder: kernel and stdlib, and the project's own application when it has one
    (`myrel-0.1.0/include/myrel.hrl` next to `ebin/myrel.app`); sasl has none. No `src`.
  - `{dev_mode, false}, {include_erts, true}, {include_src, true}`: real copies with `src`, of the project's
    application and of kernel, stdlib and sasl from the SDK (213 `.erl` files, `stdlib-6.2.2.3/src/lists.erl`
    among them).
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
  application name:
  1. kernel and stdlib of the SDK, as today;
  2. the project's files that are not in a release: outside build output, then `_build/default/lib`, then the
     rest of `_build`. They win over the other SDK libraries, as they do for module references today;
  3. the other SDK libraries;
  4. releases. A release copy of a module does not win over the SDK when the SDK has that module, kernel and
     stdlib included. A module that only the release copy has still resolves there. For `-include_lib` the
     choice is between application directories: the copy does not win when the SDK has the application.

  Ties: a file under a source root before one outside, a file that is not under a directory whose name starts
  with a dot before one that is, then the path. The answer does not depend on the order in which the index
  returns files.
- The function part of a remote call and of an `-import` entry is looked up in the file that the module part
  resolved to, and in no other file (open decision 6). So the two parts cannot end in different files. Only
  when the module part does not resolve within its scope does the function part search as it does today, the
  whole project and the first file that has the function, in the new order (open decision 7). A remote type
  already follows its module reference and moves with it.
- `-include_lib("app/...")` picks the application directory by the same order.
- Nothing is excluded and nothing is removed from the indices. A dependency that exists only under
  `_build/default/lib` resolves as it does today.
- Not changed: kernel and stdlib of the SDK first, and the choice between versions of one application outside
  releases.
- Not changed although the issue names it: `-include` in IntelliJ IDEA. It makes no choice between several
  files: it takes the file next to the including file, else the first hit in the include folders of the IDE
  module, and never asks the index (read from `ErlangPsiImplUtil.getDirectlyIncludedFiles`, not probed). From
  a source file it does not reach a copy unless an include folder of the module points into `_build`. The
  headers that the issue's case reaches through the index are those of `-include_lib`, which changes. In a
  small IDE `-include` has one more step that asks the index; see Impact.

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
| `ErlangFunctionReferenceImpl.getExternalFunction` | The target of the function of a remote call and of an `-import` entry: the function is looked up in the file that the module part resolved to (open decision 6). Only when the module part does not resolve does it search the whole project as today, in the new order (open decision 7). The lookup of an auto-imported function in `erlang`, which has no module part, searches as today in the new order |
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
- Completion: two changes, both in the table above: the `-include_lib` path completion, and in a small IDE
  the last step of the `-include` path completion. Module names are already listed once; functions offered after `dup:` are collected from every file named `dup` as today.
- Two IDE modules without a dependency between them (the last table of "Why"). The module reference searches
  the IDE module of the caller, its dependencies and the libraries
  (`ErlangModuleReferenceImpl.getSearchScope`), the function reference the whole project
  (`ErlangFunctionReferenceImpl.getExternalFunction`). Both rows of that table stay as they are:
  - `dup:f()`: `dup` resolves to the file of the caller's module, and `f` is looked up there. The file of the
    other module is not asked, although it is the first by path.
  - `b_mod:g()`: `b_mod` does not resolve, so `g` searches the whole project as today and resolves (open
    decision 7). The rebar import adds a module dependency only for the applications
    that an application lists, so such projects exist. The same holds for a file outside every source root,
    which the module reference does not find.

  A light fixture has one IDE module. The case of a file outside every source root is tested there; the
  two-module case needs a heavy test (`JavaModuleTestCase`, as `ErlangDebuggableRunConfigurationProducerTest`
  has) or stays untested (open decision 8).
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
6. **A function that the file of the module does not have.** Today `dup:g()` resolves if any file named `dup`
   in the project has `g/0`. Proposed: when `dup` resolves, `g` is looked up in the file that `dup` resolved
   to, with its includes, and in no other file, because one module of that name is on the code path. A project
   with two unrelated modules of one name then gets "Unresolved function" for the functions of the one that
   `dup` does not resolve to. This says nothing about a call whose module part does not resolve; that case is
   decision 7, and this decision reads the same whichever way decision 7 goes. Alternative: keep searching the
   other files and accept that the two parts can end in different files.
7. **A remote call whose module part does not resolve within its scope.** The module part searches the IDE
   module of the caller, its dependencies and the libraries. A call into an IDE module that the calling module
   does not depend on, or into a file outside every source root, resolves the function and not the module
   today, because the function part searches the whole project. Proposed: keep that. The function part then
   searches as today, the whole project and the first file that has the function, in the new order.
   Alternative: no search; the function is unresolved when the module is. Both parts then always agree, and
   what breaks is that such calls turn into "Unresolved function" warnings
   (`ErlangUnresolvedFunctionInspection`) in projects whose IDE modules lack dependencies, for example after a
   rebar import of applications that do not list each other. The alternative removes the last sentence of the
   requirement "Function of a remote call is looked up in the file of its module" and changes its scenario
   "Boundary: module in a file outside every source root".
8. **A test for two IDE modules.** A light fixture has one IDE module, so the two rows of the last table of
   "Why" can be asserted only by a heavy test. Proposed: one heavy test class, `ErlangTwoModulesResolveTest`
   (`JavaModuleTestCase`, with the modules created as in `ErlangDebuggableRunConfigurationProducerTest`), for
   the scenario "Boundary: module of that name in an IDE module that is not a dependency". It passes before
   and after the change, and fails for an implementation that looks for the function in the first file of the
   whole project: that is `aaa_b/src/dup.erl`, which has no `f/0`.
   Alternative: no heavy test. The scenario and task 3.14 are removed, the two-module case stays untested, and
   the review of the implementation reads the lookup instead.
