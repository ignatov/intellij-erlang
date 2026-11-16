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

import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandler;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.util.Lsp4jUtilKt;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.util.PsiTreeUtil;
import org.eclipse.lsp4j.*;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.intellij.erlang.elp.ElpEngine;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Go to declaration under a root that ELP serves: asks ELP's {@code textDocument/definition} before the platform looks at the
 * plugin's references. The platform asks the declaration handlers first and uses the first non-empty answer, so the plugin's
 * references never answer there: when ELP has no target (nothing there, a timeout, the server loads or crashed) the handler answers
 * with the element at the caret, which goes nowhere. Only on a declaration itself, which ELP answers with the declaration, the
 * handler answers nothing, so that the platform shows its usages, which ELP finds ({@code ElpFindUsagesHandlerFactory}).
 * Registered in {@code erlang-elp-lsp.xml}.
 */
public final class ElpGotoDeclarationHandler implements GotoDeclarationHandler {
  private static final int TIMEOUT_MS = 2000;

  @Override
  public PsiElement @Nullable [] getGotoDeclarationTargets(@Nullable PsiElement sourceElement, int offset, Editor editor) {
    if (!ElpEngine.ownsFeature(sourceElement, ElpEngine.Feature.NAVIGATION)) return null;
    PsiElement[] nowhere = {sourceElement};
    // The request blocks; the platform computes the targets in the background.
    if (ApplicationManager.getApplication().isDispatchThread()) return nowhere;
    PsiFile psiFile = sourceElement.getContainingFile();
    VirtualFile file = psiFile.getOriginalFile().getVirtualFile();
    Project project = psiFile.getProject();
    LspServer client = ElpLspServerStatus.findClient(project, file, ElpEngine.Feature.NAVIGATION);
    if (client == null) return nowhere;

    Document document = editor.getDocument();
    DefinitionParams params = new DefinitionParams(client.getDocumentIdentifier(file), Lsp4jUtilKt.getLsp4jPosition(document, offset));
    Either<List<? extends Location>, List<? extends LocationLink>> result =
      client.sendRequestSync(TIMEOUT_MS, server -> server.getTextDocumentService().definition(params));
    if (result == null) return nowhere;

    List<Target> targets = new ArrayList<>();
    if (result.isLeft()) {
      for (Location location : result.getLeft()) targets.add(new Target(location.getUri(), location.getRange()));
    }
    else {
      for (LocationLink link : result.getRight()) targets.add(new Target(link.getTargetUri(), link.getTargetSelectionRange()));
    }
    if (targets.size() == 1 && isAtCaret(targets.getFirst(), client, file, document, offset)) return null;

    List<PsiElement> elements = new ArrayList<>();
    for (Target target : targets) {
      PsiElement element = findElement(project, client, target);
      if (element != null) elements.add(element);
    }
    return elements.isEmpty() ? nowhere : elements.toArray(PsiElement.EMPTY_ARRAY);
  }

  /**
   * On a declaration ELP answers with the declaration itself; the platform then shows its usages, which ELP finds.
   */
  private static boolean isAtCaret(@NotNull Target target, @NotNull LspServer client, @NotNull VirtualFile file,
                                   @NotNull Document document, int offset) {
    if (!file.equals(client.getDescriptor().findFileByUri(target.uri()))) return false;
    TextRange range = Lsp4jUtilKt.getRangeInDocument(document, target.range());
    return range != null && range.containsOffset(offset);
  }

  /**
   * @return the named element whose name is at the target, or the leaf there; the platform navigates to its text offset
   */
  @Nullable
  private static PsiElement findElement(@NotNull Project project, @NotNull LspServer client, @NotNull Target target) {
    VirtualFile file = client.getDescriptor().findFileByUri(target.uri());
    if (file == null) return null;
    PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
    Document document = FileDocumentManager.getInstance().getDocument(file);
    if (psiFile == null || document == null) return null;
    Integer offset = Lsp4jUtilKt.getOffsetInDocument(document, target.range().getStart());
    if (offset == null) return null;
    PsiElement leaf = psiFile.findElementAt(offset);
    PsiNameIdentifierOwner owner = PsiTreeUtil.getParentOfType(leaf, PsiNameIdentifierOwner.class, false);
    PsiElement name = owner != null ? owner.getNameIdentifier() : null;
    return name != null && name.getTextRange().containsOffset(offset) ? owner : leaf;
  }

  private record Target(@NotNull String uri, @NotNull Range range) {
  }
}
