/*
 * Copyright 2012-2026 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.elp.lsp;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.ExternalLanguageAnnotators;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.platform.lsp.api.customization.LspDiagnosticsSupport;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.Range;
import org.intellij.erlang.ErlangLanguage;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * What the user sees of ELP's syntax error P1711 in the modules {@code shop_chars}, {@code shop_macros} and {@code shop_syntax}: the
 * diagnostics that the real ELP published go through the descriptor's diagnostics support with the platform's annotation holder of
 * the file, as the platform's LSP highlighting passes them. {@code erlc} compiles the first two modules and rejects the third.
 */
public class ElpRecordedSyntaxErrorTest extends ElpRecordedProjectTestBase {
  private static final String SYNTAX_ERROR = "P1711";

  /**
   * ELP's lexer does not know the {@code \x{...}} escape of a character literal; erlc compiles it. The plugin's lexer does not know
   * it either, and its parser reports the first three literals: there ELP's errors stay.
   */
  public void testCharLiteralWithHexadecimalEscapeIsAWarning() {
    Map<Diagnostic, HighlightSeverity> shown = shownSyntaxErrors("src/shop_chars.erl");
    assertFalse(shown.isEmpty());
    List<String> wrong = new ArrayList<>();
    shown.forEach((diagnostic, severity) -> {
      if (line(diagnostic) == lineOf("src/shop_chars.erl", "$\\x{20B9}") && severity != HighlightSeverity.WARNING) {
        wrong.add(describe(diagnostic, severity));
      }
    });
    assertEmpty(wrong);
    assertOnlyPluginParseErrorsKeepElpErrors("src/shop_chars.erl", shown);
  }

  /**
   * A macro that expands to the head of a catch clause, as epgsql and rebar3 define {@code ?WITH_STACKTRACE}; erlc compiles it. The
   * plugin's parser reports the use of the macro, and there ELP's error stays.
   */
  public void testMacroThatExpandsToAClauseHeadIsAWarning() {
    Map<Diagnostic, HighlightSeverity> shown = shownSyntaxErrors("src/shop_macros.erl");
    int define = lineOf("src/shop_macros.erl", "-define(WITH_STACKTRACE");
    List<String> atDefine = new ArrayList<>();
    shown.forEach((diagnostic, severity) -> {
      if (line(diagnostic) == define) atDefine.add(describe(diagnostic, severity));
    });
    assertEquals(1, atDefine.size());
    assertTrue(atDefine.getFirst(), atDefine.getFirst().endsWith(" " + HighlightSeverity.WARNING));
    assertOnlyPluginParseErrorsKeepElpErrors("src/shop_macros.erl", shown);
  }

  /**
   * erlc rejects both functions. ELP's own parser reports the stray variable, where the plugin's parser reports an error too, and ELP
   * reports erlc's own "syntax error before: 'end'" next to the plugin's error. Both stay errors.
   */
  public void testRealSyntaxErrorsStayErrors() {
    Map<Diagnostic, HighlightSeverity> shown = shownSyntaxErrors("src/shop_syntax.erl");
    assertEquals(2, shown.size());
    shown.forEach((diagnostic, severity) -> assertEquals(describe(diagnostic, severity), HighlightSeverity.ERROR, severity));
    assertFalse(parseErrors("src/shop_syntax.erl").isEmpty());
  }

  /** Every ELP syntax error that is still an error is one where the plugin's parser reports an error too. */
  private void assertOnlyPluginParseErrorsKeepElpErrors(@NotNull String path, @NotNull Map<Diagnostic, HighlightSeverity> shown) {
    List<TextRange> parseErrors = parseErrors(path);
    List<String> wrong = new ArrayList<>();
    shown.forEach((diagnostic, severity) -> {
      boolean pluginError = false;
      for (TextRange error : parseErrors) pluginError |= error.intersects(range(path, diagnostic.getRange()));
      if ((severity == HighlightSeverity.ERROR) != pluginError) wrong.add(describe(diagnostic, severity) + ", plugin error: " + pluginError);
    });
    assertEmpty(wrong);
  }

  /** ELP's recorded P1711 diagnostics of the file, with the severity that the editor shows for each. */
  @NotNull
  private Map<Diagnostic, HighlightSeverity> shownSyntaxErrors(@NotNull String path) {
    List<Diagnostic> recorded = new ArrayList<>();
    for (Diagnostic diagnostic : myAnswers.diagnostics().get(path)) {
      if (SYNTAX_ERROR.equals(ElpDiagnosticsSupport.code(diagnostic))) recorded.add(diagnostic);
    }
    LspDiagnosticsSupport support = assertInstanceOf(myDescriptor.getLspCustomization().getDiagnosticsCustomizer(), LspDiagnosticsSupport.class);
    ExternalLanguageAnnotators.INSTANCE.addExplicitExtension(ErlangLanguage.INSTANCE, new ExternalAnnotator<PsiFile, PsiFile>() {
      /** Also in a file with parse errors, which the default skips. */
      @Override
      public PsiFile collectInformation(@NotNull PsiFile file, @NotNull Editor editor, boolean hasErrors) {
        return file;
      }

      @Override
      public PsiFile doAnnotate(PsiFile file) {
        return file;
      }

      @Override
      public void apply(@NotNull PsiFile file, PsiFile annotationResult, @NotNull AnnotationHolder holder) {
        for (Diagnostic diagnostic : recorded) support.createAnnotation(holder, diagnostic, range(path, diagnostic.getRange()), List.of());
      }
    }, getTestRootDisposable());
    myFixture.configureFromTempProjectFile(path);
    List<HighlightInfo> infos = myFixture.doHighlighting();
    Map<Diagnostic, HighlightSeverity> shown = new LinkedHashMap<>();
    for (Diagnostic diagnostic : recorded) {
      TextRange range = range(path, diagnostic.getRange());
      HighlightSeverity severity = null;
      for (HighlightInfo info : infos) {
        if (info.getStartOffset() == range.getStartOffset() && info.getEndOffset() == range.getEndOffset() &&
            support.getMessage(diagnostic).equals(info.getDescription())) {
          severity = info.getSeverity();
        }
      }
      assertNotNull("not shown: " + describe(file(path), range), severity);
      shown.put(diagnostic, severity);
    }
    return shown;
  }

  @NotNull
  private List<TextRange> parseErrors(@NotNull String path) {
    List<TextRange> ranges = new ArrayList<>();
    for (PsiErrorElement error : PsiTreeUtil.collectElementsOfType(psiFile(path), PsiErrorElement.class)) ranges.add(error.getTextRange());
    return ranges;
  }

  @NotNull
  private TextRange range(@NotNull String path, @NotNull Range range) {
    Document document = document(file(path));
    return new TextRange(document.getLineStartOffset(range.getStart().getLine()) + range.getStart().getCharacter(),
                         document.getLineStartOffset(range.getEnd().getLine()) + range.getEnd().getCharacter());
  }

  private int lineOf(@NotNull String path, @NotNull String needle) {
    Document document = document(file(path));
    int offset = document.getText().indexOf(needle);
    assertTrue(path + ": " + needle, offset >= 0);
    return document.getLineNumber(offset);
  }

  private static int line(@NotNull Diagnostic diagnostic) {
    return diagnostic.getRange().getStart().getLine();
  }

  @NotNull
  private String describe(@NotNull Diagnostic diagnostic, @NotNull HighlightSeverity severity) {
    return ElpDiagnosticsSupport.code(diagnostic) + " at " + (line(diagnostic) + 1) + ":" + (diagnostic.getRange().getStart().getCharacter() + 1) +
           " " + severity;
  }
}
