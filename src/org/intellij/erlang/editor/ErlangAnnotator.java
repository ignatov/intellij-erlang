/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.editor;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.colors.EditorColorsManager;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
public class ErlangAnnotator implements Annotator, DumbAware {
  @Override
  public void annotate(@NotNull PsiElement psiElement, @NotNull final AnnotationHolder annotationHolder) {
    psiElement.accept(new ErlangVisitor() {
      @Override
      public void visitAtomAttribute(@NotNull ErlangAtomAttribute o) {
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
      }

      @Override
      public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitSpecification(@NotNull ErlangSpecification o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitAttribute(@NotNull ErlangAttribute o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitExport(@NotNull ErlangExport o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitInclude(@NotNull ErlangInclude o) {
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "include");
        markAttributeNameAsKeyword(o, annotationHolder, "include_lib");
      }

      @Override
      public void visitModule(@NotNull ErlangModule o) {
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "record");
      }

      @Override
      public void visitMacrosDefinition(@NotNull ErlangMacrosDefinition o) {
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "define");
      }

      @Override
      public void visitMacrosName(@NotNull ErlangMacrosName o) {
        final PsiElement firstChild = o.getFirstChild();
        if (firstChild != null) {
          setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.VARIABLES);
        }
      }

      // todo: add export, import and other bundled attributes
    });
  }

  private static void markAttributeNameAsKeyword(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder, @NotNull String name) {
    PsiElement rec = o.getFirstChild();
    while (rec != null) {
      if (rec instanceof LeafPsiElement && name.equals(rec.getText())) break;
      rec = rec.getNextSibling();
    }
    if (rec != null) {
      setHighlighting(rec, annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
    }
  }

  private static void markFirstChildAsKeyword(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder) {
    final PsiElement firstChild = o.getFirstChild();
    if (firstChild != null) {
      setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
    }
  }

  private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder, final TextAttributesKey key) {
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(TextAttributes.ERASE_MARKER);
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
  }
}
