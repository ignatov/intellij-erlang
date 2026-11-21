package org.intellij.erlang.parser.macro;

import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.Expansion;
import org.intellij.erlang.parser.macro.ErlangMacroExpander.HeaderSummary;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Measurement, not run in CI: with {@code EXPANDER_DIFF_DIR} set, writes the expanded text of every .erl file there
 * (includes resolved next to the file) to {@code EXPANDER_DIFF_OUT}/name.erl and its outcomes to name.outcomes, for a
 * comparison of {@code erlc -P} of both files.
 */
public class ErlangMacroExpanderDiffProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testWriteExpansions() throws Exception {
    String in = System.getenv("EXPANDER_DIFF_DIR");
    String outDir = System.getenv("EXPANDER_DIFF_OUT");
    if (in == null || outDir == null) return;
    Path dir = Paths.get(in), out = Paths.get(outDir);
    Files.createDirectories(out);
    Map<Path, HeaderSummary> headers = new HashMap<>();
    try (Stream<Path> s = Files.list(dir)) {
      for (Path p : s.filter(x -> x.toString().endsWith(".erl")).sorted().toList()) {
        String text = Files.readString(p, StandardCharsets.UTF_8);
        Expansion x = ErlangMacroExpander.expand(text, new ErlangLexer(getProject()), 4000, (owner, attribute, string) -> {
          if (string == null) return null;
          Path base = owner == null ? dir : ((Path)owner.header).getParent();
          Path h = base.resolve(string).normalize();
          if (!Files.isRegularFile(h)) return null;
          return headers.computeIfAbsent(h, k -> {
            try {
              return ErlangMacroExpander.summarize(Files.readString(k, StandardCharsets.UTF_8), new ErlangLexer(getProject()), k);
            }
            catch (Exception e) {
              throw new RuntimeException(e);
            }
          });
        });
        String name = p.getFileName().toString();
        Files.writeString(out.resolve(name), x.text, StandardCharsets.UTF_8);
        StringBuilder o = new StringBuilder(x.outcomes.toString());
        for (ErlangMacroExpander.Form f : x.forms) {
          if (f.circularMacro != null) o.append("\ncircular ").append(f.circularMacro);
        }
        Files.writeString(out.resolve(name.replace(".erl", ".outcomes")), o.append('\n').toString(), StandardCharsets.UTF_8);
      }
    }
  }
}
