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
 * The {@code ELP} position: on top of the diagnostics, ELP serves the features it served through this client in practice, and
 * the plugin switches its own counterparts off ({@link org.intellij.erlang.elp.ElpEngine}): completion, hover documentation,
 * parameter name inlay hints, folding, selection ranges, go to definition and code actions as intentions. Every other feature
 * stays switched off here, so that the plugin's PSI owns it alone: find usages, rename, signature help, call hierarchy, workspace
 * symbols, implementations, code lenses, semantic tokens and the rest. The inline and extract refactorings of the code actions
 * stay off too, so ELP's refactoring actions appear as intentions and the plugin's refactorings keep their shortcuts.
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
}
