# AGENTS.md

## Cursor Cloud specific instructions

This repository is a single product: the **IntelliJ IDEA Erlang plugin** (a JetBrains IDE
plugin, not a client/server app). It is built with Gradle + the IntelliJ Platform Gradle
Plugin. There are no databases, servers, or background services to run.

Standard build/test/run commands live in `docs/DEVELOPMENT.md` — use those. Notes below
are only the non-obvious things.

### Toolchain
- Requires **JDK 21** (already installed on the base image; `java`/`javac` resolve to 21).
  Gradle uses the current Java home, so `JAVA_HOME` does not need to be set.
- The Gradle wrapper (`./gradlew`, Gradle 9.0.0) manages Gradle itself.
- The first Gradle task that needs it downloads the **IntelliJ Platform SDK 2026.1** from
  JetBrains repositories (hundreds of MB); this requires network and is cached afterward.
  The environment update script (`./gradlew --no-daemon classes testClasses`) pre-warms
  this cache and compiles main + test sources.

### Test / lint / build / run
- **Test:** `./gradlew test` — ~839 tests, runs headless. Tests use the bundled mock Erlang
  SDKs in `testData/mockSdk-*`, so a real Erlang/OTP install is **not** required.
- **Lint / verify:** there is no separate linter. The verification gate is the JPS module's
  Java 11 bytecode compatibility check: `./gradlew :jps-plugin:verifyJpsJava11Compatibility`
  (also run automatically by `check`, `buildPlugin`, and `prepareSandbox`).
- **Build:** `./gradlew buildPlugin` produces `build/distributions/intellij-erlang-*.zip`.
  This step launches a short-lived headless IDE to build searchable options — expect several
  IDE warnings in the log; a `BUILD SUCCESSFUL` line is the signal it worked.
- **Run the app:** `./gradlew runIde` launches a sandbox IntelliJ IDEA on `DISPLAY=:1`.

### runIde gotchas (non-obvious)
- On a **fresh sandbox**, IntelliJ blocks on a **"JetBrains User Agreement"** consent dialog
  that sits *behind* the splash screen — so the splash looks stuck for minutes. You must
  accept it (check the box, click **Continue**), then choose **Don't Send** on the data-sharing
  prompt, then **Trust Project**. After that the main window appears. This only needs to be
  done once per sandbox; state persists under `.intellijPlatform/sandbox/`.
- To open a project directly and skip the Welcome screen, pass the path as an argument:
  `./gradlew runIde --args="/absolute/path/to/erlang-project"`.
- The `testCompilation` Gradle task (real-compiler tests) needs a real Erlang SDK via
  `-Derlang.sdk.path=/path/to/erlang`; it is optional and separate from `./gradlew test`.
