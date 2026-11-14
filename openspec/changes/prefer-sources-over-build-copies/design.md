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
  path.
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
   module part has only step 7 today, and step 5 does not show there because its scope has no file outside a
   source root. Step 6 replaces `File.isHidden`, which reads a file attribute on Windows: with the name rule
   the answer depends on the path only.

   Project content outside a release stands above the libraries because that is what rebar3 does: a
   dependency under `_build/default/lib` (rank 1) with the name of an OTP application other than kernel and
   stdlib wins over the OTP one (rank 0, library) on the code path. With the rank first, the SDK file would
   win. A dependency cannot replace kernel or stdlib: rebar3 crashes on one named `stdlib`.
   A release stands below the libraries because its copies of OTP applications come from the SDK: with
   `include_src` rebar3 copies `kernel`, `stdlib` and `sasl` with their `src` into the release, and in dev mode
   it links them to the SDK. A file there is never what the build compiles from.
   The comparator lives next to `ErlangModuleIndex`; `ModuleResolutionComparator` is removed.
   Alternative: keep both comparators and add the rank to each. Rejected: the two orders already disagree, and
   that disagreement is part of the defect.
2. **The function is looked up in the first file of that order.** `getExternalFunction` keeps its scope, the
   whole project, sorts the files with the shared comparator and takes the first file only (with its
   includes), instead of the first file among all that happens to have the function (proposal.md, open
   decision 6). The scope is not changed to that of the module reference: it would turn calls that resolve
   today into unresolved ones (proposal.md, "Impact" and open decision 7). So both parts end in the same file
   where both see the same files, which is the case in one IDE module with every file under a source root.
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
- [A project that keeps its only copy of a module in a release loses to an SDK library of the same name] → a
  release is a build result; a module that exists nowhere else still resolves there.
- [The scenario about a file outside every source root needs another source root than the light fixture has]
  → its test removes the fixture's source root and adds `lib/zzz/src` with `PsiTestUtil`, and restores both in
  `finally`. A probe did this on today's code; the tests that ran after it in the same class passed.
- [No test covers two IDE modules] → light fixtures have one module. The scope argument of
  `getExternalFunction` stays as it is; the review of the implementation checks that line.
- [Cost] → the rank is computed only when a name has more than one candidate, by walking the parents of each
  file once. The comparator of `ErlangModuleIndex` queries `ErlangApplicationIndex` on every comparison today; the shared one
  resolves the kernel and stdlib directories once per sort.
- [The order of remote-call resolution changes from "library before project files outside source roots" to
  "project before library"] → this is what the module reference and `testUserModulesAppearFirst` already do,
  and what rebar3 does (decision 1). Today `eunit:test(x)` resolves `eunit` to the project's file and `test` to
  the SDK's. A release copy is the exception in the other direction: today the module part goes to it, after
  the change both parts go to the SDK.

Areas of the "Stop and ask the maintainer" list that the implementation touches: none expected. No index
key, value, filter or version changes, no descriptor change, and no existing test expectation should change;
if one does, the implementation stops and reports it. `ErlangPrepareDependenciesCompileTask` is in
`src/.../compilation`, not in `jps-plugin/`; it is not changed, only the order of what it receives.
