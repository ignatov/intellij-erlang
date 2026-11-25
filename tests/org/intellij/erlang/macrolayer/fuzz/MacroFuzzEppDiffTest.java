package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.lexer.Lexer;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

/**
 * The expander differential: the plugin's macro expansion ({@code ErlangMacroExpander}, same-file macros) against the real
 * preprocessor (epp, through {@code testData/macroFuzz/epp_compare.escript}) for generated modules and the sample files,
 * for each form the expander says it expanded. A form whose tokens differ is a failure. Needs {@code escript}
 * ({@code MACRO_FUZZ_ESCRIPT}, default {@code /opt/homebrew/bin/escript}); without it, or on master (no expander), the
 * test does nothing. It runs only with {@code MACRO_FUZZ_EPP=1} or in a long run.
 */
public class MacroFuzzEppDiffTest extends MacroFuzzTestBase {
  public void testExpanderAgreesWithEpp() throws Throwable {
    String escript = Objects.requireNonNullElse(FuzzConfig.env("MACRO_FUZZ_ESCRIPT"), "/opt/homebrew/bin/escript");
    if (!Files.isExecutable(Paths.get(escript))) return;
    // a tool outside the repository: only on demand (MACRO_FUZZ_EPP=1) or in long runs, never in the normal test task
    if (FuzzConfig.env("MACRO_FUZZ_EPP") == null && !FuzzConfig.isLongRun()) return;
    Class<?> expander;
    try {
      expander = Class.forName("org.intellij.erlang.parser.macro.ErlangMacroExpander");
    }
    catch (ClassNotFoundException e) {
      return;
    }
    Method expand = expander.getMethod("expand", CharSequence.class, Lexer.class, int.class);
    Map<String, String> samples = FuzzInputs.samples();
    List<String> sampleFiles = new ArrayList<>();
    for (String p : samples.keySet()) if (p.endsWith(".erl")) sampleFiles.add(p);
    Path script = Paths.get("testData", "macroFuzz", "epp_compare.escript").toAbsolutePath();
    runCases("epp", 6, (index, rnd, caseId) -> {
      String text;
      String name;
      if (index % 3 == 2 && !sampleFiles.isEmpty()) {
        name = sampleFiles.get(index / 3 % sampleFiles.size());
        text = samples.get(name);
      }
      else {
        name = "m.erl";
        text = new MacroCaseGenerator(rnd).project().get("m.erl");
      }
      // same-file macros only: the includes go
      text = text.replace("\r\n", "\n").replaceAll("(?m)^-include(_lib)?\\(.*$", "");
      String module = name.substring(name.lastIndexOf('/') + 1, name.length() - 4);
      rec.setCaseFiles(Map.of(name, text));
      String src = text;
      Path dir = FuzzConfig.out().resolve("epp-work").resolve(caseId.replace(':', '_'));
      Files.createDirectories(dir);
      Path erl = dir.resolve(module + ".erl");
      Files.writeString(erl, src, StandardCharsets.UTF_8);
      StringBuilder plugin = new StringBuilder();
      Map<Integer, String> expandedByLine = new HashMap<>();
      rec.guard("expand", () -> {
        Lexer lexer = (Lexer)Class.forName("org.intellij.erlang.parser.ErlangLexer").getConstructor(com.intellij.openapi.project.Project.class)
          .newInstance(getProject());
        Object expansion = expand.invoke(null, src, lexer, 4000);
        String out = (String)expansion.getClass().getField("text").get(expansion);
        List<?> forms = (List<?>)expansion.getClass().getField("forms").get(expansion);
        for (Object f : forms) {
          Field status = f.getClass().getField("status");
          if (!"EXPANDED".equals(String.valueOf(status.get(f)))) continue;
          int srcStart = f.getClass().getField("srcStart").getInt(f);
          int expStart = f.getClass().getField("expStart").getInt(f);
          int expEnd = f.getClass().getField("expEnd").getInt(f);
          int line = lineOf(src, firstSignificant(src, srcStart));
          String formText = out.substring(Math.max(0, expStart), Math.min(out.length(), expEnd));
          expandedByLine.put(line, formText);
          plugin.append(line).append('\t').append(Base64.getEncoder().encodeToString(formText.getBytes(StandardCharsets.UTF_8))).append('\n');
        }
      });
      if (plugin.length() == 0) return;
      Path pf = dir.resolve(module + ".plugin");
      Files.writeString(pf, plugin.toString(), StandardCharsets.UTF_8);
      rec.guard("epp compare", () -> {
        Process p = new ProcessBuilder(escript, script.toString(), erl.toString(), pf.toString()).redirectErrorStream(true).start();
        String result = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        p.waitFor();
        for (String l : result.split("\n")) {
          if (!l.startsWith("DIFF ")) continue;
          String[] parts = l.split("\t");
          int line = Integer.parseInt(parts[0].substring(5).trim());
          String detail = parts.length > 2 ? "plugin " + abbreviate(parts[1]) + " epp " + abbreviate(parts[2]) : l;
          rec.fail("expander-diff", "form differs from epp", name + " line " + line + ": " + detail + "\n  plugin text: " +
                                                                expandedByLine.get(line));
        }
      });
    });
  }

  private static String abbreviate(String s) {
    return s.length() > 600 ? s.substring(0, 600) + "..." : s;
  }

  /** Skips blanks and comments, so that the form's line is the line of its first token, as epp numbers it. */
  private static int firstSignificant(String text, int offset) {
    int i = offset;
    while (i < text.length()) {
      char c = text.charAt(i);
      if (Character.isWhitespace(c)) i++;
      else if (c == '%') {
        while (i < text.length() && text.charAt(i) != '\n') i++;
      }
      else break;
    }
    return i;
  }

  private static int lineOf(String text, int offset) {
    int line = 1;
    for (int i = 0; i < Math.min(offset, text.length()); i++) if (text.charAt(i) == '\n') line++;
    return line;
  }
}
