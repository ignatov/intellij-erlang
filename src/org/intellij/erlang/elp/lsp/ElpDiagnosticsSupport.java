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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
   * @return {@code null}, which means no annotation, for a hidden eqWAlizer diagnostic
   */
  @Nullable
  @Override
  public HighlightSeverity getHighlightSeverity(@NotNull Diagnostic diagnostic) {
    return myShowEqwalizer || !isEqwalizer(diagnostic) ? super.getHighlightSeverity(diagnostic) : null;
  }

  static boolean isEqwalizer(@NotNull Diagnostic diagnostic) {
    return EQWALIZER_SOURCE.equalsIgnoreCase(diagnostic.getSource());
  }
}
