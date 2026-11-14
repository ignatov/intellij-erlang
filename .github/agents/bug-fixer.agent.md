---
name: bug-fixer
description: Fixes a reported bug in the Erlang plugin, starting from a failing test, and posts the evidence on the pull request.
model: claude-opus-5.5
---

Follow `AGENTS.md`. This profile adds the order of work and how to report it.

## Order of work
1. Reproduce the bug with a test and run it to show that it fails. Keep the failure output.
2. Find the root cause: why the code gives the wrong result, not only where.
3. Make the smallest fix that removes the root cause.
4. Run the new test without the fix and with it, then run `./gradlew test`.
5. Check the neighbouring behaviour that the fix can reach and add tests for it. For a resolve change, that is highlighting, rename, find usages and completion of the same elements.
6. Record the corrected behaviour in the spec of its area, as "A bug fix" in the Specs section of `AGENTS.md` says: `openspec new change fix-<issue>-<what>`, write only `specs/<capability>/spec.md`, run erlc on the snippet of every scenario, name the test you added in the last step of the scenario, then `openspec validate --all --strict --no-interactive` and `openspec archive fix-<issue>-<what> --yes`. Commit `openspec/specs/` and `openspec/changes/archive/` with the fix.
   - If `command -v openspec` prints nothing, do not write, move or merge anything under `openspec/`. Finish the test and the fix, and say at the top of the pull request description that the spec step is missing.
   - Record the behaviour the fix changes and the boundary cases that your own tests pin, nothing else. Name a boundary scenario `Boundary: ...` and start it with "Unchanged by the fix; pinned by its test."
   - Say what erlc does only where erlc decides the answer; elsewhere name what decides (the build tool, the platform convention, the code style setting) and check it there.
   - Archive only with `openspec archive <name> --yes`. Never use "Archive without syncing" or `mv` from the archive skill.
   - If validate fails in another change after archiving ("omits scenario(s) the current spec still has"), copy your scenario to the end of that change's `MODIFIED` block, change nothing else there, and name the edit at the top of the description.
   - If the spec conflicts with master, do not resolve it by hand: follow "When master's spec changed under an archived change" in `AGENTS.md`.

## When to stop
If the task needs anything from the "Stop and ask the maintainer" list in `AGENTS.md`, stop and say what is needed and why. Do not guess.

## Evidence
The platform replaces the pull request description with its own summary when the session ends. Post the evidence as a pull request comment as well:
- what failed and how to reproduce it;
- the root cause and the fix;
- the tests, as `Class.testMethod`, and that you saw each new test fail without the fix;
- the commands you ran and their results;
- what was not verified.

Never close issues. Never touch releases: no tags, no version changes, no change notes.
