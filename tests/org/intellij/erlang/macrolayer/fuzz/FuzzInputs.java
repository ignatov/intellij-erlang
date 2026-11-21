package org.intellij.erlang.macrolayer.fuzz;

import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Stream;

/** Inputs from disk: the sample project and the files of {@code MACRO_FUZZ_INPUTS}. */
public final class FuzzInputs {
  private FuzzInputs() {
  }

  /**
   * The sample project: {@code MACRO_FUZZ_SAMPLES} (for example {@code /Users/ignatov/src/erl-test}), else the snapshot in
   * {@code testData/macroFuzz/erl-test}. Its {@code src}, {@code include} and {@code libs} folders, relative path -> text.
   */
  public static @NotNull Map<String, String> samples() {
    String dir = FuzzConfig.env("MACRO_FUZZ_SAMPLES");
    Path root = dir != null ? Paths.get(dir) : Paths.get("testData", "macroFuzz", "erl-test").toAbsolutePath();
    Map<String, String> r = new TreeMap<>();
    for (String top : List.of("src", "include", "libs")) {
      Path p = root.resolve(top);
      if (!Files.isDirectory(p)) continue;
      try (Stream<Path> s = Files.walk(p)) {
        for (Path f : s.filter(Files::isRegularFile).sorted().toList()) {
          String name = f.getFileName().toString();
          if (!(name.endsWith(".erl") || name.endsWith(".hrl") || name.endsWith(".app.src"))) continue;
          r.put(root.relativize(f).toString().replace('\\', '/'), read(f));
        }
      }
      catch (IOException ignored) {
      }
    }
    return r;
  }

  /** The {@code .erl}/{@code .hrl} files of {@code MACRO_FUZZ_INPUTS} (a {@code :}-separated list of files and folders). */
  public static @NotNull List<Path> extraFiles() {
    String v = FuzzConfig.inputs();
    List<Path> r = new ArrayList<>();
    if (v == null) return r;
    for (String part : v.split(":")) {
      Path p = Paths.get(part);
      if (Files.isRegularFile(p)) r.add(p);
      else if (Files.isDirectory(p)) {
        try (Stream<Path> s = Files.walk(p)) {
          s.filter(Files::isRegularFile).filter(f -> f.toString().endsWith(".erl") || f.toString().endsWith(".hrl")).sorted().forEach(r::add);
        }
        catch (IOException ignored) {
        }
      }
    }
    return r;
  }

  public static @NotNull String read(@NotNull Path f) {
    try {
      byte[] bytes = Files.readAllBytes(f);
      String text = new String(bytes, StandardCharsets.UTF_8);
      if (text.contains("�")) text = new String(bytes, StandardCharsets.ISO_8859_1);
      return text;
    }
    catch (IOException e) {
      return "";
    }
  }
}
