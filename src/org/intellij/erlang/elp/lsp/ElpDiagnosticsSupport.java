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

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.util.TextRange;
import com.intellij.platform.lsp.api.customization.LspDiagnosticsSupport;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

/**
 * Hides the diagnostics of eqWAlizer unless they are switched on. ELP has no LSP setting for eqWAlizer: it type-checks a module
 * when the project's {@code .elp.toml} or the module's {@code -typing([eqwalizer])} asks for it, and {@code enable_all} is on by
 * default. So ELP still runs it, and the client drops what it publishes; the diagnostics carry {@code source: "eqWAlizer"}.
 */
final class ElpDiagnosticsSupport extends LspDiagnosticsSupport {
  static final String EQWALIZER_SOURCE = "eqWAlizer";

  private final boolean myShowEqwalizer;

  ElpDiagnosticsSupport(boolean showEqwalizer) {
    myShowEqwalizer = showEqwalizer;
  }

  boolean isShowEqwalizer() {
    return myShowEqwalizer;
  }

  /**
   * @return {@code null}, which means no annotation, for a hidden eqWAlizer diagnostic, and at most a warning for a shown one:
   * eqWAlizer reports its type errors as errors, but the code compiles, so they must not paint the file red
   */
  @Nullable
  @Override
  public HighlightSeverity getHighlightSeverity(@NotNull Diagnostic diagnostic) {
    if (!isEqwalizer(diagnostic)) {
      HighlightSeverity severity = super.getHighlightSeverity(diagnostic);
      return severity == HighlightSeverity.ERROR && LOWERED_TO_WARNING.contains(code(diagnostic)) ? HighlightSeverity.WARNING : severity;
    }
    return myShowEqwalizer ? HighlightSeverity.WARNING : null;
  }

  /**
   * ELP's lints that it reports as errors on code that {@code erlc} compiles (measured on ten real projects): W0008 on the helper
   * functions of Common Test suites with {@code export_all}, W0053 and W0014. They are shown as warnings. ELP's syntax error P1711
   * needs the file, see {@link #createAnnotation}.
   */
  static final Set<String> LOWERED_TO_WARNING = Set.of("W0008", "W0053", "W0014");

  static final String SYNTAX_ERROR = "P1711";
  /**
   * The message of a P1711 of ELP's own parser. ELP also reports the syntax errors of erlc's parser as P1711, with erlc's message
   * ("syntax error before: 'end'"), and those stay errors.
   */
  static final String ELP_PARSER_MESSAGE = "Syntax Error";

  /**
   * Shows a syntax error of ELP's own parser as a warning where the plugin's parser finds no error. ELP 1.1.0's parser reports
   * code that erlc compiles: a char literal such as {@code $\x{4141}}, a macro that expands to part of a clause such as
   * {@code -define(WITH_STACKTRACE(T, R, S), T:R:S ->)}. A real syntax error stays red: the plugin shows its own parse errors, and
   * an ELP syntax error that overlaps one of them stays an error.
   */
  @Override
  public void createAnnotation(@NotNull AnnotationHolder holder, @NotNull Diagnostic diagnostic, @NotNull TextRange textRange,
                               @NotNull List<? extends IntentionAction> quickFixes) {
    boolean lower = isElpParserSyntaxError(diagnostic) && !overlapsParseError(holder.getCurrentAnnotationSession().getFile(), textRange);
    super.createAnnotation(holder, lower ? asWarning(diagnostic) : diagnostic, textRange, quickFixes);
  }

  private boolean isElpParserSyntaxError(@NotNull Diagnostic diagnostic) {
    return SYNTAX_ERROR.equals(code(diagnostic)) && ELP_PARSER_MESSAGE.equals(getMessage(diagnostic)) &&
           getHighlightSeverity(diagnostic) == HighlightSeverity.ERROR;
  }

  private static boolean overlapsParseError(@NotNull PsiFile file, @NotNull TextRange range) {
    List<TextRange> errors = CachedValuesManager.getCachedValue(file, () -> CachedValueProvider.Result.create(
      ContainerUtil.map(PsiTreeUtil.collectElementsOfType(file, PsiErrorElement.class), PsiElement::getTextRange), file));
    return ContainerUtil.exists(errors, error -> error.intersects(range));
  }

  /** The same diagnostic at warning severity. The message is a plain string: {@link #isElpParserSyntaxError} compared it. */
  @NotNull
  private Diagnostic asWarning(@NotNull Diagnostic diagnostic) {
    Diagnostic warning = new Diagnostic();
    warning.setRange(diagnostic.getRange());
    warning.setSeverity(DiagnosticSeverity.Warning);
    warning.setCode(diagnostic.getCode());
    warning.setCodeDescription(diagnostic.getCodeDescription());
    warning.setSource(diagnostic.getSource());
    warning.setMessage(getMessage(diagnostic));
    warning.setTags(diagnostic.getTags());
    warning.setRelatedInformation(diagnostic.getRelatedInformation());
    warning.setData(diagnostic.getData());
    return warning;
  }

  @Nullable
  static String code(@NotNull Diagnostic diagnostic) {
    Either<String, Integer> code = diagnostic.getCode();
    return code == null ? null : String.valueOf(code.get());
  }

  static boolean isEqwalizer(@NotNull Diagnostic diagnostic) {
    return EQWALIZER_SOURCE.equalsIgnoreCase(diagnostic.getSource());
  }
}
