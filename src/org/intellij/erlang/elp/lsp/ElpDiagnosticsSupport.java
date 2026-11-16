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

import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.platform.lsp.api.customization.LspDiagnosticsSupport;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
   * functions of Common Test suites with {@code export_all}, W0053 and W0014. They are shown as warnings. P1711, ELP's parse error,
   * is not lowered: it is a real error in most files, and ELP's known false ones (a macro that expands to part of a clause, a char
   * literal such as {@code $\x{4141}}) can be told apart only with the file, which this severity hook does not get.
   */
  static final Set<String> LOWERED_TO_WARNING = Set.of("W0008", "W0053", "W0014");

  @Nullable
  static String code(@NotNull Diagnostic diagnostic) {
    Either<String, Integer> code = diagnostic.getCode();
    return code == null ? null : String.valueOf(code.get());
  }

  static boolean isEqwalizer(@NotNull Diagnostic diagnostic) {
    return EQWALIZER_SOURCE.equalsIgnoreCase(diagnostic.getSource());
  }
}
