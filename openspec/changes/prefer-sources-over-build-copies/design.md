# Design

## Context

Three lookups choose among several files today, each in its own way (see proposal.md, "Why", for what was
observed):

- `ErlangModuleReferenceImpl.resolveInner` asks `ErlangModuleIndex` within the module's scope with dependencies
  and libraries, then sorts again with its own `ModuleResolutionComparator`: stdlib, kernel, project before
  library, then `FileUtil.comparePaths`. `_` sorts before lower-case letters, which is why `_build` wins.
- `ErlangFunctionReferenceImpl.getExternalFunction` asks `ErlangModuleIndex.getFilesByName` in the scope of the
  whole project and takes the first file that has the function. The order is that of
  `ErlangModuleIndex.MyProjectFilesComparator`: kernel or stdlib, source root, not hidden, library, shorter
  path.
- `ErlangApplicationIndex.getApplicationDirectoryByName` and `getAllApplicationDirectories` share one
  processor. It keeps the last directory the index reports whose name equals the application name, so the
  result depends on index order. In a light fixture test the index reports the directories in the order in
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
   1. kernel and stdlib of the SDK, as today;
   2. project content before a library, as the module reference does today;
   3. the rank;
   4. source root before plain directory, not hidden before hidden;
   5. the path as text.

   Project before library stands above the rank because that is what rebar3 does: a dependency under
   `_build/default/lib` (rank 1) with the name of an OTP application other than kernel and stdlib wins over the
   OTP one (rank 0, library) on the code path. With the rank first, the SDK file would win. A dependency
   cannot replace kernel or stdlib: rebar3 crashes on one named `stdlib`.
   The comparator lives next to `ErlangModuleIndex`; `ModuleResolutionComparator` is removed.
   Alternative: keep both comparators and add the rank to each. Rejected: the two orders already disagree, and
   that disagreement is part of the defect.
2. **The function is looked up in the file the module resolves to.** `getExternalFunction` uses the same scope
   and order as the module reference and takes the first file only (with its includes), instead of the first
   file among all that happens to have the function. This is proposal.md, open decision 6.
3. **The processor of `ErlangApplicationIndex` collects all directories, then chooses.** Project before
   library and the rank first, then the existing rule (a directory without a version suffix, else the highest
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
- [Cost] → the rank is computed only when a name has more than one candidate, by walking the parents of each
  file once. The comparator of `ErlangModuleIndex` queries `ErlangApplicationIndex` on every comparison today; the shared one
  resolves the kernel and stdlib directories once per sort.
- [The order of remote-call resolution changes from "library before project files outside source roots" to
  "project before library"] → this is what the module reference and `testUserModulesAppearFirst` already do,
  and what rebar3 does (decision 1). Today `eunit:test(x)` resolves `eunit` to the project's file and `test` to
  the SDK's.

Areas of the "Stop and ask the maintainer" list that the implementation touches: none expected. No index
key, value, filter or version changes, no descriptor change, and no existing test expectation should change;
if one does, the implementation stops and reports it. `ErlangPrepareDependenciesCompileTask` is in
`src/.../compilation`, not in `jps-plugin/`; it is not changed, only the order of what it receives.
