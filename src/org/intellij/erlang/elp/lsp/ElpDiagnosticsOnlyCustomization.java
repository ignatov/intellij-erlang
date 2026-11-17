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

import com.intellij.platform.lsp.api.customization.*;
import org.jetbrains.annotations.NotNull;

/**
 * The baseline of {@link ElpEngineCustomization}: diagnostics, the quick fixes attached to them and the commands those quick
 * fixes may run; every other feature that the IDEA 2026.1 client has a switch for is switched off explicitly, and
 * {@link ElpEngineCustomization} switches on what ELP owns. The switches that later clients added keep their defaults.
 */
class ElpDiagnosticsOnlyCustomization extends LspCustomization {
  private final LspDiagnosticsCustomizer myDiagnostics;

  private final LspCodeActionsCustomizer myCodeActions = new LspCodeActionsSupport() {
    @Override
    public boolean getIntentionActionsSupport() {
      return false;
    }
  };

  ElpDiagnosticsOnlyCustomization(@NotNull LspDiagnosticsCustomizer diagnostics) {
    myDiagnostics = diagnostics;
  }

  @NotNull
  @Override
  public LspDiagnosticsCustomizer getDiagnosticsCustomizer() {
    return myDiagnostics;
  }

  @NotNull
  @Override
  public LspCodeActionsCustomizer getCodeActionsCustomizer() {
    return myCodeActions;
  }

  @NotNull
  @Override
  public LspGoToDefinitionCustomizer getGoToDefinitionCustomizer() {
    return LspGoToDefinitionDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspGoToTypeDefinitionCustomizer getGoToTypeDefinitionCustomizer() {
    return LspGoToTypeDefinitionDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspHoverCustomizer getHoverCustomizer() {
    return LspHoverDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspCompletionCustomizer getCompletionCustomizer() {
    return LspCompletionDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspSemanticTokensCustomizer getSemanticTokensCustomizer() {
    return LspSemanticTokensDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspFormattingCustomizer getFormattingCustomizer() {
    return LspFormattingDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspFindReferencesCustomizer getFindReferencesCustomizer() {
    return LspFindReferencesDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspOptimizeImportsCustomizer getOptimizeImportsCustomizer() {
    return LspOptimizeImportsDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspDocumentColorCustomizer getDocumentColorCustomizer() {
    return LspDocumentColorDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspDocumentLinkCustomizer getDocumentLinkCustomizer() {
    return LspDocumentLinkDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspFoldingRangeCustomizer getFoldingRangeCustomizer() {
    return LspFoldingRangeDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspInlayHintCustomizer getInlayHintCustomizer() {
    return LspInlayHintDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspDocumentHighlightsCustomizer getDocumentHighlightsCustomizer() {
    return LspDocumentHighlightsDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspSignatureHelpCustomizer getSignatureHelpCustomizer() {
    return LspSignatureHelpDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspDocumentSymbolCustomizer getDocumentSymbolCustomizer() {
    return LspDocumentSymbolDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspWorkspaceSymbolCustomizer getWorkspaceSymbolCustomizer() {
    return LspWorkspaceSymbolDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspCallHierarchyCustomizer getCallHierarchyCustomizer() {
    return LspCallHierarchyDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspTypeHierarchyCustomizer getTypeHierarchyCustomizer() {
    return LspTypeHierarchyDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspSelectionRangeCustomizer getSelectionRangeCustomizer() {
    return LspSelectionRangeDisabled.INSTANCE;
  }

  @NotNull
  @Override
  public LspCodeLensCustomizer getCodeLensCustomizer() {
    return LspCodeLensDisabled.INSTANCE;
  }
}
