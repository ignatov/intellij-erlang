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

import com.intellij.find.findUsages.AbstractFindUsagesDialog;
import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.find.findUsages.FindUsagesOptions;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Document;
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
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.SearchScope;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.Processor;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.ReferenceContext;
import org.eclipse.lsp4j.ReferenceParams;
import org.intellij.erlang.elp.ElpEngine;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Find usages of an element under a root that ELP serves: the target is still the plugin's element, so there is one target and no
 * choice popup, but its usages are ELP's {@code textDocument/references} at the element's name. Find Usages, Show Usages and the
 * usages shown on a declaration all go through this handler. When ELP has no answer (an error, a timeout, an empty list, the server
 * loads or crashed) there are no usages: the plugin does not search in parallel. Highlighting usages in the file is ELP's
 * {@code textDocument/documentHighlight}, which the platform's LSP client asks for; this handler highlights nothing of its own.
 * Registered in {@code erlang-elp-lsp.xml} before the plugin's factory.
 */
public final class ElpFindUsagesHandlerFactory extends FindUsagesHandlerFactory {
  private static final int TIMEOUT_MS = 10_000;

  @Override
  public boolean canFindUsages(@NotNull PsiElement element) {
    return ElpEngine.ownsFeature(element, ElpEngine.Feature.FIND_USAGES) && pluginHandler(element, false) != null;
  }

  @Nullable
  @Override
  public FindUsagesHandler createFindUsagesHandler(@NotNull PsiElement element, boolean forHighlightUsages) {
    FindUsagesHandler plugin = pluginHandler(element, forHighlightUsages);
    if (plugin == null) return null;
    return new ElpFindUsagesHandler(element, plugin);
  }

  /** The handler the platform would use without this factory: the plugin's factory, or the default one. */
  @Nullable
  private static FindUsagesHandler pluginHandler(@NotNull PsiElement element, boolean forHighlightUsages) {
    for (FindUsagesHandlerFactory factory : FindUsagesHandlerFactory.EP_NAME.getExtensionList(element.getProject())) {
      if (factory instanceof ElpFindUsagesHandlerFactory || !factory.canFindUsages(element)) continue;
      FindUsagesHandler handler = factory.createFindUsagesHandler(element, forHighlightUsages);
      if (handler != null && handler != FindUsagesHandler.NULL_HANDLER) return handler;
    }
    return null;
  }

  private static final class ElpFindUsagesHandler extends FindUsagesHandler {
    private final FindUsagesHandler myPlugin;

    ElpFindUsagesHandler(@NotNull PsiElement element, @NotNull FindUsagesHandler plugin) {
      super(element);
      myPlugin = plugin;
    }

    @NotNull
    @Override
    public AbstractFindUsagesDialog getFindUsagesDialog(boolean isSingleFile, boolean toShowInNewTab, boolean mustOpenInNewTab) {
      return myPlugin.getFindUsagesDialog(isSingleFile, toShowInNewTab, mustOpenInNewTab);
    }

    @Override
    public boolean processElementUsages(@NotNull PsiElement element,
                                        @NotNull Processor<? super UsageInfo> processor,
                                        @NotNull FindUsagesOptions options) {
      List<UsageInfo> usages = elpUsages(element);
      if (usages == null) return true;
      for (UsageInfo usage : usages) {
        if (!processor.process(usage)) return false;
      }
      return true;
    }

    @NotNull
    @Override
    public Collection<PsiReference> findReferencesToHighlight(@NotNull PsiElement target, @NotNull SearchScope searchScope) {
      return List.of();
    }
  }

  /**
   * @return ELP's references to {@code element} as usages, or {@code null} when ELP does not answer
   */
  @Nullable
  static List<UsageInfo> elpUsages(@NotNull PsiElement element) {
    Request request = ReadAction.nonBlocking(() -> request(element)).executeSynchronously();
    if (request == null) return null;
    List<? extends Location> locations = request.client().sendRequestSync(
      TIMEOUT_MS, server -> server.getTextDocumentService().references(request.params()));
    if (locations == null) return null;
    return ReadAction.nonBlocking(() -> {
      List<UsageInfo> usages = new ArrayList<>();
      for (Location location : locations) {
        UsageInfo usage = usage(element.getProject(), request.client(), location);
        if (usage != null) usages.add(usage);
      }
      return usages;
    }).executeSynchronously();
  }

  @Nullable
  private static Request request(@NotNull PsiElement element) {
    if (!element.isValid()) return null;
    PsiFile psiFile = element.getContainingFile();
    VirtualFile file = psiFile != null ? psiFile.getOriginalFile().getVirtualFile() : null;
    Document document = file != null ? FileDocumentManager.getInstance().getDocument(file) : null;
    if (document == null) return null;
    LspServer client = ElpLspServerStatus.findClient(element.getProject(), file, ElpEngine.Feature.FIND_USAGES);
    if (client == null) return null;
    PsiElement name = element instanceof PsiNameIdentifierOwner owner ? owner.getNameIdentifier() : null;
    int offset = name != null ? name.getTextRange().getStartOffset() : element.getTextOffset();
    ReferenceParams params = new ReferenceParams(client.getDocumentIdentifier(file), Lsp4jUtilKt.getLsp4jPosition(document, offset),
                                                 new ReferenceContext(false));
    return new Request(client, params);
  }

  @Nullable
  private static UsageInfo usage(@NotNull Project project, @NotNull LspServer client, @NotNull Location location) {
    VirtualFile file = client.getDescriptor().findFileByUri(location.getUri());
    PsiFile psiFile = file != null ? PsiManager.getInstance(project).findFile(file) : null;
    Document document = file != null ? FileDocumentManager.getInstance().getDocument(file) : null;
    if (psiFile == null || document == null) return null;
    TextRange range = Lsp4jUtilKt.getRangeInDocument(document, location.getRange());
    return range != null ? new UsageInfo(psiFile, range.getStartOffset(), range.getEndOffset()) : null;
  }

  private record Request(@NotNull LspServer client, @NotNull ReferenceParams params) {
  }
}
