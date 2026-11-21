package org.intellij.erlang.macrolayer.fuzz;

import org.jetbrains.annotations.Nullable;

import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * The knobs of the macro fuzz suite, all from the environment (Gradle forwards it to the test JVM; add {@code --rerun},
 * because Gradle does not treat the environment as a test input).
 * <ul>
 *   <li>{@code MACRO_FUZZ_SEED}: the base seed (default: a fixed one, so that the smoke run is deterministic);</li>
 *   <li>{@code MACRO_FUZZ_CASE}: run only this case index (replay);</li>
 *   <li>{@code MACRO_FUZZ_SECONDS}: a long run of that many seconds per test method; without it, a short smoke run;</li>
 *   <li>{@code MACRO_FUZZ_INPUTS}: a path list (separated by {@code :}) of files or directories of {@code .erl}/{@code .hrl}
 *       inputs to mutate, in addition to the generated ones;</li>
 *   <li>{@code MACRO_FUZZ_OUT}: where failing cases are written (default {@code build/macro-fuzz});</li>
 *   <li>{@code MACRO_FUZZ_KNOWN}: a file of known signatures (one per line, {@code signature<TAB>id}) that do not fail the
 *       test (default {@code testData/macroFuzz/known-findings.txt});</li>
 *   <li>{@code MACRO_FUZZ_ACTION_MS}: the time limit of one action (default 20000);</li>
 *   <li>{@code MACRO_FUZZ_ONLY}: a substring of the action label: run only such actions (replay of one action).</li>
 * </ul>
 */
public final class FuzzConfig {
  public static final long DEFAULT_SEED = 20261121L;

  private FuzzConfig() {
  }

  public static @Nullable String env(String name) {
    String v = System.getenv(name);
    if (v == null || v.isBlank()) v = System.getProperty(name);
    return v == null || v.isBlank() ? null : v.trim();
  }

  public static long seed() {
    String v = env("MACRO_FUZZ_SEED");
    return v == null ? DEFAULT_SEED : Long.parseLong(v);
  }

  public static boolean seedGiven() {
    return env("MACRO_FUZZ_SEED") != null;
  }

  public static int onlyCase() {
    String v = env("MACRO_FUZZ_CASE");
    return v == null ? -1 : Integer.parseInt(v);
  }

  public static long seconds() {
    String v = env("MACRO_FUZZ_SECONDS");
    return v == null ? 0 : Long.parseLong(v);
  }

  public static boolean isLongRun() {
    return seconds() > 0;
  }

  public static @Nullable String inputs() {
    return env("MACRO_FUZZ_INPUTS");
  }

  public static @Nullable String only() {
    return env("MACRO_FUZZ_ONLY");
  }

  public static long actionMillis() {
    String v = env("MACRO_FUZZ_ACTION_MS");
    return v == null ? 20000 : Long.parseLong(v);
  }

  public static Path out() {
    String v = env("MACRO_FUZZ_OUT");
    return v == null ? Paths.get("build", "macro-fuzz").toAbsolutePath() : Paths.get(v).toAbsolutePath();
  }

  public static Path known() {
    String v = env("MACRO_FUZZ_KNOWN");
    return v == null ? Paths.get("testData", "macroFuzz", "known-findings.txt").toAbsolutePath() : Paths.get(v).toAbsolutePath();
  }

  /** The seed of case {@code index} of {@code family}: independent of the other cases, so one case replays alone. */
  public static long caseSeed(long base, String family, int index) {
    long h = base * 0x9E3779B97F4A7C15L + family.hashCode() * 0xC2B2AE3D27D4EB4FL + index * 0x165667B19E3779F9L;
    h ^= h >>> 33;
    h *= 0xFF51AFD7ED558CCDL;
    h ^= h >>> 33;
    return h;
  }
}
