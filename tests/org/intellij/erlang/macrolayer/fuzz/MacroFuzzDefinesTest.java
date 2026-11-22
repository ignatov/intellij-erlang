package org.intellij.erlang.macrolayer.fuzz;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The bodies of fragment macros: every editor action at the offsets inside {@code -define} forms of fragment macros and
 * just after them, in modules made of fragments and their uses; a module erlc compiles gets no error highlighting; and
 * a broken {@code -define} (unterminated, a bad parameter list, ...) that erlc refuses must stay red on its line. The
 * erlc parts need {@code MACRO_FUZZ_ERLC_ORACLE=1} or a long run.
 */
public class MacroFuzzDefinesTest extends MacroFuzzTestBase {
  private static final Pattern DEFINE = Pattern.compile("(?m)^-define\\(.*$");

  private static boolean smoke() {
    return !FuzzConfig.isLongRun() && FuzzConfig.onlyCase() < 0;
  }

  private static boolean erlcOn() {
    String erlc = Objects.requireNonNullElse(FuzzConfig.env("MACRO_FUZZ_ERLC"), "/opt/homebrew/bin/erlc");
    return Files.isExecutable(Paths.get(erlc)) && (FuzzConfig.env("MACRO_FUZZ_ERLC_ORACLE") != null || FuzzConfig.isLongRun());
  }

  public void testActionsInAndAfterFragmentDefines() throws Throwable {
    EditorActionProbe.enableInspections(myFixture);
    runCases("defines", 2, (index, rnd, caseId) -> {
      String text = new MacroCaseGenerator(rnd).fragmentModule();
      Map<String, String> files = Map.of("m.erl", text);
      boolean compiles = erlcOn() && MacroFuzzOracleTest.erlc(files, "m.erl") == null;
      FuzzWorld w = world(files, false);
      try {
        PsiFile f = w.psi("m.erl");
        open(f.getVirtualFile());
        warm(myFixture.getFile());
        List<HighlightInfo>[] infos = new List[1];
        rec.guard("highlight", () -> infos[0] = myFixture.doHighlighting(HighlightSeverity.ERROR));
        if (compiles && infos[0] != null) {
          for (HighlightInfo h : infos[0]) {
            if (h.getSeverity() != HighlightSeverity.ERROR) continue;
            rec.fail("false-error", "error in a fragment module erlc compiles: " + String.valueOf(h.getDescription()).replaceAll("'[^']*'", "'X'"),
                     "at " + h.getStartOffset() + " «" + text.substring(h.getStartOffset(), Math.min(text.length(), h.getEndOffset())) + "»: " +
                     h.getDescription());
          }
        }
        // offsets inside every -define and the three after it
        List<Integer> offsets = new ArrayList<>();
        Matcher m = DEFINE.matcher(text);
        while (m.find()) for (int o = m.start(); o <= Math.min(text.length(), m.end() + 3); o++) offsets.add(o);
        if (smoke()) {
          Collections.shuffle(offsets, rnd);
          offsets = new ArrayList<>(offsets.subList(0, Math.min(8, offsets.size())));
        }
        EditorActionProbe probe = new EditorActionProbe(rec, myFixture);
        TargetProbe targets = new TargetProbe(rec, myFixture.getEditor(), myFixture.getFile());
        targets.searchedTargets = smoke() ? 1 : 4;
        Document doc = myFixture.getEditor().getDocument();
        for (int offset : offsets) {
          for (EditorActionProbe.Act a : probe.acts()) {
            if (!a.readOnly() || a.name().equals("highlight") || a.name().equals("goto-symbol")) continue;
            if (smoke() && rnd.nextInt(4) != 0) continue;
            myFixture.getEditor().getCaretModel().moveToOffset(Math.min(offset, doc.getTextLength()));
            myFixture.getEditor().getSelectionModel().removeSelection();
            probe.run(a, rnd, "@" + offset + " ");
          }
          targets.probeOffset(Math.min(offset, doc.getTextLength()), false);
          targets.probeOffset(Math.min(offset, doc.getTextLength()), true);
          EditorActionProbe.Act edit = probe.random(rnd, false);
          if (!edit.readOnly()) {
            myFixture.getEditor().getCaretModel().moveToOffset(Math.min(offset, doc.getTextLength()));
            probe.run(edit, rnd, "@" + offset + " ");
            probe.checkConsistency("@" + offset + " " + edit.name() + " ");
            if (!doc.getText().equals(text)) {
              WriteCommandAction.runWriteCommandAction(getProject(), () -> doc.setText(text));
              PsiDocumentManager.getInstance(getProject()).commitDocument(doc);
            }
          }
        }
      }
      finally {
        w.dispose();
      }
    });
  }

  /** Ways to break a -define line. */
  private static String breakDefine(String line, Random rnd) {
    return switch (rnd.nextInt(7)) {
      case 0 -> line.endsWith(").") ? line.substring(0, line.length() - 2) : line + " (";   // unterminated
      case 1 -> line.replaceFirst("\\(([A-Z][A-Za-z]*)\\)", "($1,)");                       // trailing comma in the parameters
      case 2 -> line.replaceFirst("\\(([A-Z][A-Za-z]*)", "(1");                              // a number as a parameter
      case 3 -> line.replaceFirst("-define\\(([A-Z_]+)", "-define($1 $1");                   // two names
      case 4 -> line.replaceFirst("-define\\(", "-define(\"str\", ");                        // a string as the name
      case 5 -> line.replaceFirst("\\(([A-Z][A-Za-z]*)\\)", "($1 $1)");                      // a missing comma in the parameters
      default -> line.replaceFirst("-define\\([A-Z_]+", "-define(");                         // no name
    };
  }

  public void testBrokenDefinesStayRed() throws Throwable {
    if (!erlcOn()) return;
    EditorActionProbe.enableInspections(myFixture);
    runCases("bad-defines", 6, (index, rnd, caseId) -> {
      String text = new MacroCaseGenerator(rnd).fragmentModule().replace("-include(\"missing.hrl\").\n", "");
      List<int[]> lines = new ArrayList<>();
      Matcher m = DEFINE.matcher(text);
      while (m.find()) lines.add(new int[]{m.start(), m.end()});
      if (lines.isEmpty()) return;
      int[] l = lines.get(rnd.nextInt(lines.size()));
      String line = text.substring(l[0], l[1]);
      String broken = breakDefine(line, rnd);
      if (broken.equals(line)) return;
      String bad = text.substring(0, l[0]) + broken + text.substring(l[1]);
      Map<String, String> files = Map.of("m.erl", bad);
      String err = MacroFuzzOracleTest.erlc(files, "m.erl");
      int lineNo = bad.substring(0, l[0]).split("\n", -1).length;
      // only when erlc refuses this very line
      if (err == null || !err.contains("m.erl:" + lineNo + ":")) return;
      FuzzWorld w = world(files, false);
      try {
        PsiFile f = w.psi("m.erl");
        open(f.getVirtualFile());
        warm(myFixture.getFile());
        List<HighlightInfo>[] infos = new List[1];
        rec.guard("highlight", () -> infos[0] = myFixture.doHighlighting(HighlightSeverity.ERROR));
        int start = l[0];
        int end = l[0] + broken.length();
        boolean red = false;
        if (infos[0] != null) {
          for (HighlightInfo h : infos[0]) {
            if (h.getSeverity() == HighlightSeverity.ERROR && h.getStartOffset() <= end && h.getEndOffset() >= start) red = true;
          }
        }
        if (!red) {
          String firstErr = err.lines().filter(x -> x.contains("m.erl:" + lineNo + ":")).findFirst().orElse(err);
          rec.fail("missing-red", "a broken -define that erlc refuses is not red", "line " + lineNo + " «" + broken + "» erlc: " + firstErr);
        }
      }
      finally {
        w.dispose();
      }
    });
  }
}
