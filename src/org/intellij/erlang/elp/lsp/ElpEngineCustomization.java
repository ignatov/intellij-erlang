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
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

/**
 * What the platform LSP client asks ELP for in the {@code ELP} position, where the plugin's counterparts stay silent while ELP
 * serves the file ({@link org.intellij.erlang.elp.ElpEngine}): diagnostics, completion, hover documentation, parameter name inlay
 * hints, folding, selection ranges, go to definition and type definition, signature help, highlighting of usages in the file,
 * and code actions as intentions.
 * <p>
 * Switched off here: find references and rename, which the plugin's own ELP entry points ask for instead
 * ({@code ElpFindUsagesHandlerFactory}, {@code ElpRenameHandler}), so that there is one target and one rename; workspace symbols,
 * because ELP answers them with module names only; call hierarchy and implementations, which the client does not deliver for an
 * Erlang element that the plugin resolves; semantic tokens, document symbols and code lenses, which stay with the plugin; and
 * the features ELP does not offer. The inline and extract refactorings of the code actions stay off too, so ELP's
 * refactoring actions appear as intentions and the plugin's refactorings keep their shortcuts.
 */
final class ElpEngineCustomization extends ElpDiagnosticsOnlyCustomization {
  private final LspCodeActionsCustomizer myCodeActions = new LspCodeActionsSupport() {
    @Override
    public boolean getInlineRefactoringSupport() {
      return false;
    }

    @Override
    public boolean getExtractRefactoringSupport() {
      return false;
    }
  };

  private final LspCompletionCustomizer myCompletion = new LspCompletionSupport();
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
