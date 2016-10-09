/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.marker;

import com.intellij.codeHighlighting.Pass;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.codeInsight.daemon.LineMarkerProvider;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.markup.GutterIconRenderer;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.util.FunctionUtil;
import com.intellij.util.containers.HashSet;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangFunctionWithArity;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.Set;

public class ErlangRecursiveCallLineMarkerProvider implements LineMarkerProvider, DumbAware {
  @Override
  public LineMarkerInfo getLineMarkerInfo(@NotNull PsiElement element) {
    return null; //do nothing
  }

  @Override
  public void collectSlowLineMarkers(@NotNull List<PsiElement> elements, @NotNull Collection<LineMarkerInfo> result) {
    Set<Integer> lines = new HashSet<>();
    for (PsiElement element : elements) {
      if (element instanceof ErlangFunctionCallExpression || element instanceof ErlangFunctionWithArity) {
        PsiReference reference = element.getReference();
        PsiElement resolve = reference != null ? reference.resolve() : null;
        if (resolve instanceof ErlangFunction) {
          if (ErlangPsiImplUtil.isRecursiveCall(element, (ErlangFunction) resolve)) {
            PsiDocumentManager instance = PsiDocumentManager.getInstance(element.getProject());
            Document document = instance.getDocument(element.getContainingFile());
            int textOffset = element.getTextOffset();
            if (document == null) continue;
            int lineNumber = document.getLineNumber(textOffset);
            if (!lines.contains(lineNumber)) {
              result.add(new RecursiveMethodCallMarkerInfo(element));
            }
            lines.add(lineNumber);
          }
        }
      }
    }
  }

  private static class RecursiveMethodCallMarkerInfo extends LineMarkerInfo<PsiElement> {
    private RecursiveMethodCallMarkerInfo(@NotNull PsiElement methodCall) {
      super(methodCall,
            methodCall.getTextRange(),
            ErlangIcons.RECURSIVE_CALL,
            Pass.UPDATE_OVERRIDDEN_MARKERS,
            FunctionUtil.<PsiElement, String>constant("Recursive call"),
            null,
            GutterIconRenderer.Alignment.RIGHT
      );
    }
  }
}


