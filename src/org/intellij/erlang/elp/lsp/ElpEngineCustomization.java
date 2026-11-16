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

import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.platform.lsp.api.customization.*;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.elp.ElpEngine;
import org.jetbrains.annotations.NotNull;

/**
 * What the platform LSP client asks ELP for in the {@code ELP} position, where the plugin's counterparts stay silent under a root
 * that ELP serves ({@link ElpEngine}): diagnostics, completion, hover documentation, parameter name inlay hints, folding,
 * selection ranges, go to definition and type definition, signature help, highlighting of usages in the file, code actions as
 * intentions, and workspace symbols for Go to Symbol and Go to Class.
 * <p>
 * Switched off here: find references, which the plugin's own ELP entry point asks for instead
 * ({@code ElpFindUsagesHandlerFactory}), so that there is one target; call hierarchy, which the client does not deliver for an
 * Erlang element; semantic tokens, document symbols and code lenses, because the plugin's lexer, structure view and breadcrumbs
 * work on the open file without its engine; and the features ELP does not offer.
 * <p>
 * Built against IDEA 2026.1, which has no switch for them, this customization leaves on what later clients added: the client's
 * own rename (2026.2 and later) next to {@code ElpRenameHandler}, go to implementation (2026.3) and the inline and extract
 * refactorings of the code actions (2026.3).
 */
final class ElpEngineCustomization extends ElpDiagnosticsOnlyCustomization {
  private final LspCodeActionsCustomizer myCodeActions = new LspCodeActionsSupport();

  /**
   * The client also gets the files outside the project's content, such as the OTP sources, where the plugin completes: ELP completes
   * only where the plugin is silent, so that the list is filled by exactly one of them.
   */
  private final LspCompletionCustomizer myCompletion = new LspCompletionSupport() {
    @Override
    public boolean shouldRunCodeCompletion(@NotNull CompletionParameters parameters) {
      return ElpEngine.ownsFeature(parameters.getOriginalFile(), ElpEngine.Feature.COMPLETION);
    }
  };
  private final LspWorkspaceSymbolCustomizer myWorkspaceSymbols = new LspWorkspaceSymbolSupport();
  private final LspHoverCustomizer myHover = new LspHoverSupport();
  private final LspInlayHintCustomizer myInlayHints = new LspInlayHintSupport();
  private final LspFoldingRangeCustomizer myFolding = new LspFoldingRangeSupport();
  private final LspSelectionRangeCustomizer mySelection = new LspSelectionRangeSupport();
  private final LspGoToDefinitionCustomizer myGoToDefinition = new LspGoToDefinitionSupport();

  private final LspGoToTypeDefinitionCustomizer myGoToTypeDefinition = new LspGoToTypeDefinitionSupport();
  private final LspSignatureHelpCustomizer mySignatureHelp = new LspSignatureHelpSupport();
  /** The client asks only for files of the plain text and TextMate languages by default. */
  private final LspDocumentHighlightsCustomizer myDocumentHighlights = new LspDocumentHighlightsSupport() {
    @Override
    public boolean shouldAskServerForDocumentHighlights(@NotNull PsiFile psiFile) {
      return true;
    }
  };

  ElpEngineCustomization(@NotNull LspDiagnosticsCustomizer diagnostics) {
    super(diagnostics);
  }

  @NotNull
  @Override
  public LspCodeActionsCustomizer getCodeActionsCustomizer() {
    return myCodeActions;
  }

  @NotNull
  @Override
  public LspCompletionCustomizer getCompletionCustomizer() {
    return myCompletion;
  }

  @NotNull
  @Override
  public LspWorkspaceSymbolCustomizer getWorkspaceSymbolCustomizer() {
    return myWorkspaceSymbols;
  }

  @NotNull
  @Override
  public LspHoverCustomizer getHoverCustomizer() {
    return myHover;
  }

  @NotNull
  @Override
  public LspInlayHintCustomizer getInlayHintCustomizer() {
    return myInlayHints;
  }

  @NotNull
  @Override
  public LspFoldingRangeCustomizer getFoldingRangeCustomizer() {
    return myFolding;
  }

  @NotNull
  @Override
  public LspSelectionRangeCustomizer getSelectionRangeCustomizer() {
    return mySelection;
  }

  @NotNull
  @Override
  public LspGoToDefinitionCustomizer getGoToDefinitionCustomizer() {
    return myGoToDefinition;
  }

  @NotNull
  @Override
  public LspGoToTypeDefinitionCustomizer getGoToTypeDefinitionCustomizer() {
    return myGoToTypeDefinition;
  }

  @NotNull
  @Override
  public LspSignatureHelpCustomizer getSignatureHelpCustomizer() {
    return mySignatureHelp;
  }

  @NotNull
  @Override
  public LspDocumentHighlightsCustomizer getDocumentHighlightsCustomizer() {
    return myDocumentHighlights;
  }
}
