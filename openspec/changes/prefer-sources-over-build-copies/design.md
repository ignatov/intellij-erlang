# Design

## Context

Three lookups choose among several files today, each in its own way (see proposal.md, "Why", for what was
observed):

- `ErlangModuleReferenceImpl.resolveInner` asks `ErlangModuleIndex` within the module's scope with dependencies
  and libraries, which does not hold a file outside every source root, then sorts again with its own `ModuleResolutionComparator`: stdlib, kernel, project before
  library, then `FileUtil.comparePaths`. `_` sorts before lower-case letters, which is why `_build` wins.
- `ErlangFunctionReferenceImpl.getExternalFunction` asks `ErlangModuleIndex.getFilesByName` in the scope of the
  whole project and takes the first file that has the function. The order is that of
  `ErlangModuleIndex.MyProjectFilesComparator`: kernel or stdlib, source root, not hidden, library, shorter
  path. It does not ask the module reference of the same call, so the two can end in different files and the
  function can resolve where the module does not.
- `ErlangApplicationIndex.getApplicationDirectoryByName` and `getAllApplicationDirectories` share one
  processor. It keeps the last directory the index reports whose name equals the application name, else the
  directory with the highest name, so the result depends on index order and a release copy with a higher
  version than the SDK's wins. Both comparators take the kernel and stdlib directories from this lookup, so
  such a copy of stdlib also takes the first place of the SDK's stdlib. In a light fixture test the index reports the directories in the order in
  which the files were created.

A remote type asks the module reference for the file (`ErlangTypeReferenceImpl.getPsiFile`), so it has no order
of its own.

`-include` does not search: it looks next to the including file, then in the include folders of the module.
Only in a small IDE it then looks in the `include` folder of the application that contains the file, which
`getContainingOtpAppRoot` takes from `getAllApplicationDirectories`. Records, macros and types come from the
files reached through includes, so they follow `-include_lib`.

The rebar import excludes `<app>/_build` and `<app>/.rebar3` of every imported application and skips
`_build/test` and the top-level `rel` when it scans. A project opened as a plain folder has nothing excluded.

## Goals / Non-Goals

**Goals:**
- One order, implemented once, used by the three lookups and by the callers that proposal.md lists under
  "Impact".
- The answer depends on paths only, not on index order, on links or on how the project was opened.

**Non-Goals:**
- Excluding folders or changing what is indexed (proposal.md, open decision 1).
- Reading `deps_dir` or `checkouts_dir` from `rebar.config`. `base_dir` is proposal.md, open decision 4.
- A rule for `logs`: after `rebar3 ct` the folder holds no `.erl`, `.hrl` or `.app` file.
- Go to Class, Go to Symbol, completion and links in quick documentation.
- Making the existing rules for kernel, stdlib and application versions part of the specs.

## Decisions

1. **Rank by path, in one comparator.** A file gets a rank from its path below the content root:
   0 outside build output; 1 under `_build/default/lib`; 2 elsewhere under `_build`, except 3; 3 in a release:
   under `_build/<profile>/rel`, or under a `rel` directory whose parent has a `rebar.config`. The full order
   is:
   1. kernel and stdlib of the SDK, as today. Their directories come from decision 3, so a release copy of
      stdlib is not taken for the SDK's;
   2. project content of rank 0, 1 and 2, in that order;
   3. a library, that is the other applications of the SDK;
   4. project content of rank 3, a release;
   5. a file under a source root before a file outside every source root;
   6. a file that is not under a directory whose name starts with a dot before one that is;
   7. the path as text.

   Steps 5 to 7 are the tie-break, and the requirement "Tie-break between files of one name"
   states the same three steps in the same order. Steps 5 and 6 are what the function part does today; the
   module part has only step 7 today. Step 5 never decides for a module reference, because its scope has no
   file outside a source root, and so not for a function that follows it (decision 2); it decides in the
   search of decision 2 when the module does not resolve, and for the callers that ask the whole project
   (proposal.md, "Impact"). Step 6 replaces `File.isHidden`, which reads a file attribute on Windows: with
   the name rule the answer depends on the path only.

   Project content outside a release stands above the libraries because that is what rebar3 does: a
   dependency under `_build/default/lib` (rank 1) with the name of an OTP application other than kernel and
   stdlib wins over the OTP one (rank 0, library) on the code path. With the rank first, the SDK file would
   win. A dependency cannot replace kernel or stdlib: rebar3 crashes on one named `stdlib`.
   A release stands below the libraries because its copies of OTP applications come from the SDK: with
   `include_src` rebar3 copies `kernel`, `stdlib` and `sasl` with their `src` into the release, and in dev mode
   it links them to the SDK. A file there is never what the build compiles from.
   The order only ranks the files of one module name and hides none. So a release copy loses to the SDK
   where the SDK has a module of that name, and a module that only the release copy has still resolves to the
   copy: a release built with OTP 27 and `include_src` has `stdlib-6.x/src/json.erl`, and with an OTP 26 SDK
   `json:encode(X)` goes there. The requirement "Release copy does not win over the SDK" says the same.
   The comparator lives next to `ErlangModuleIndex`; `ModuleResolutionComparator` is removed.
   Alternative: keep both comparators and add the rank to each. Rejected: the two orders already disagree, and
   that disagreement is part of the defect.
2. **The function is looked up in the file that the module part resolved to.** For a reference with a module
   part (`Module:Function(...)`, an `-import` entry), `ErlangFunctionReferenceImpl` first resolves the module
   name as the module reference does: the lookup of `ErlangModuleReferenceImpl.resolveInner`, with its scope
   (the IDE module of the calling file, its dependencies and the libraries) and the shared order. When that
   gives a file, the function is looked up in that file with its includes and nowhere else (proposal.md, open
   decision 6). The two parts then cannot end in different files, whatever the whole project holds: a file of
   the same name in an IDE module that the caller does not depend on is not asked, even when it is first in
   the order.
   Only when the module name does not resolve within that scope does `getExternalFunction` run as today:
   the scope of the whole project and the first file that has the function, now in the shared order
   (proposal.md, open decision 7). That keeps the calls that resolve the function and not the module today.
   If the maintainer takes the alternative of open decision 7, this paragraph goes and the first one stays
   as it is.
   The lookup of an auto-imported function in `erlang` (a call without a module part) has no module
   reference. It runs as today, in the shared order.
   Alternative: take the first file of the whole project in the shared order, as an earlier draft had it.
   Rejected: the module part does not see the whole project, so the function part could look in a file that
   the module part cannot resolve to, and a call that resolves today would become unresolved.
3. **The processor of `ErlangApplicationIndex` collects all directories, then chooses.** First the groups of
   decision 1 (a project directory of rank 0, 1 and 2, then a library, then a release), then the existing
   rule (a directory without a version suffix, else the highest
   version), then the path. `getApplicationDirectoryByName` and `getAllApplicationDirectories` both use it.
4. **No exclusion.** Alternatives and why not:
   - A `DirectoryIndexExcludePolicy` that excludes every `_build`: fetched dependencies live in
     `_build/default/lib` and would stop resolving unless each is imported as a module or library.
   - Skipping `_build` in the input filter of `ErlangModuleIndex`: the same loss, plus an index version bump.
   - `.gitignore`: the rebar3 template ignores `_checkouts`, which is source the developer edits, and a project
     may commit or ignore anything. Ignored is not the same as copied.
   - Comparing canonical paths so that a link and its target count as one file: correct for rebar3's links on
     Linux and macOS, but not for the copied test suites, for releases, or for Windows, where rebar3 cannot
     always link. The rank covers the links as well, so this is not needed.

## Risks / Trade-offs

- [Two unrelated modules with one name in different applications lose resolution of the second one's
  functions] → open decision 6; the alternative keeps today's search.
- [A rebar3 project that still has an old `deps/` folder resolves to `deps/x` instead of
  `_build/default/lib/x`] → open decision 3.
- [A `rel` directory with sources but without a `rebar.config` beside it is not recognised] → it ranks as
  source, as today; open decision 2.
- [The tests of `-include_lib` fail before the change only for one creation order of the files] → today's
  result is the directory created last, so each test creates the expected directory first. This relies on the
  index reporting in creation order, which was observed and is not a documented contract; it cannot be made
  stricter from a test. After the change the result does not depend on the order: `testRanks` (task 1.1)
  checks the comparator with the files added in more than one order.
- [The build order of the external build gets its paths in another order] →
  `ErlangPrepareDependenciesCompileTask` lists every file of a module name, so the set is unchanged; the
  implementation runs `testCompilation`.
- [It is not verified whether the platform indexes files through rebar3's links at all] → the rank is by path
  and gives the same answer either way. The tests use plain copies.
- [A project that keeps its only copy of a module in a release loses to an SDK library that has a module of
  the same name] → a release is a build result. A module that exists nowhere else still resolves there
  (decision 1), which the scenario "Boundary: module that only a release copy has" pins.
- [A header that only the release copy of an OTP application has is no longer found by `-include_lib` when
  the copy has a higher version than the SDK's application] → `-include_lib` chooses one application directory
  and looks for the path there (`ErlangPsiImplUtil.getDirectlyIncludedFiles`, read from the code), and
  decision 3 chooses the SDK's. Today the copy's directory is chosen when its version is higher.
- [The scenario about a file outside every source root needs another source root than the light fixture has]
  → its test removes the fixture's source root and adds `lib/zzz/src` with `PsiTestUtil`, and restores both in
  `finally`. A probe did this on today's code; the tests that ran after it in the same class passed.
- [Two IDE modules without a dependency between them] → a light fixture has one IDE module, so this case
  needs a heavy test or stays untested. Proposed is one heavy test (`JavaModuleTestCase`, with two Erlang
  modules as `ErlangDebuggableRunConfigurationProducerTest` creates them) for the scenario "Boundary: module
  of that name in an IDE module that is not a dependency"; a probe of that kind ran on today's code. Without
  it (proposal.md, open decision 8) the case stays untested and the review of the implementation reads the
  lookup of decision 2. The search of decision 2 for an unresolved module is tested in a light fixture with a
  file outside every source root.
- [Cost] → the rank is computed only when a name has more than one candidate, by walking the parents of each
  file once. The comparator of `ErlangModuleIndex` queries `ErlangApplicationIndex` on every comparison today; the shared one
  resolves the kernel and stdlib directories once per sort.
- [The order of remote-call resolution changes from "library before project files outside source roots" to
  "project before library"] → this is what the module reference and `testUserModulesAppearFirst` already do,
  and what rebar3 does (decision 1). Today `eunit:test(x)` resolves `eunit` to the project's file and `test` to
  the SDK's. A release copy of a module that the SDK has is the exception in the other direction: today the
  module part goes to it, after the change both parts go to the SDK.

Areas of the "Stop and ask the maintainer" list that the implementation touches: none expected. No index
key, value, filter or version changes, no descriptor change, and no existing test expectation should change;
if one does, the implementation stops and reports it. `ErlangPrepareDependenciesCompileTask` is in
`src/.../compilation`, not in `jps-plugin/`; it is not changed, only the order of what it receives.
