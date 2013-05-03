/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.documentation.ErlangDocUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

/**
 * @author ignatov
 */
public class ErlangAnnotator implements Annotator, DumbAware {
  @Override
  public void annotate(@NotNull PsiElement psiElement, @NotNull final AnnotationHolder annotationHolder) {
    if (psiElement instanceof PsiComment) {
      highlightEdocTags((PsiComment) psiElement, annotationHolder);
    }
    else if (!(psiElement instanceof ErlangFile)) return;
    psiElement.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitAtomAttribute(@NotNull ErlangAtomAttribute o) {
        super.visitAtomAttribute(o);
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
      }

      @Override
      public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
        super.visitCallbackSpec(o);
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitSpecification(@NotNull ErlangSpecification o) {
        super.visitSpecification(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.SPECIFICATION);
      }

      @Override
      public void visitAttribute(@NotNull ErlangAttribute o) {
        super.visitAttribute(o);
        if (o.getSpecification() != null) {
          markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.SPECIFICATION);
        }
        else {
          markFirstChildAsKeyword(o, annotationHolder);
        }
      }

      @Override
      public void visitExport(@NotNull ErlangExport o) {
        super.visitExport(o);
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitExportTypeAttribute(@NotNull ErlangExportTypeAttribute o) {
        super.visitExportTypeAttribute(o);
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitImportDirective(@NotNull ErlangImportDirective o) {
        super.visitImportDirective(o);
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitInclude(@NotNull ErlangInclude o) {
        super.visitInclude(o);
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "include");
        markAttributeNameAsKeyword(o, annotationHolder, "include_lib");
      }

      @Override
      public void visitModule(@NotNull ErlangModule o) {
        super.visitModule(o);
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
        super.visitRecordDefinition(o);
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "record");
      }

      @Override
      public void visitMacrosDefinition(@NotNull ErlangMacrosDefinition o) {
        super.visitMacrosDefinition(o);
        markFirstChildAsKeyword(o, annotationHolder);
        markAttributeNameAsKeyword(o, annotationHolder, "define");
      }

      @Override
      public void visitTypeDefinition(@NotNull ErlangTypeDefinition o) {
        super.visitTypeDefinition(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.TYPE_DEFINITION);
        markAttributeName(o, annotationHolder, "type", ErlangSyntaxHighlighter.TYPE_DEFINITION);
        markAttributeName(o, annotationHolder, "opaque", ErlangSyntaxHighlighter.TYPE_DEFINITION);
      }

      @Override
      public void visitMacrosName(@NotNull ErlangMacrosName o) {
        super.visitMacrosName(o);
        final PsiElement firstChild = o.getFirstChild();
        if (firstChild != null) {
          setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.MACRO);
        }
      }

      @Override
      public void visitBehaviour(@NotNull ErlangBehaviour o) {
        super.visitBehaviour(o);
        markFirstChildAsKeyword(o, annotationHolder);
      }

      @Override
      public void visitRecordRef(@NotNull ErlangRecordRef o) {
        super.visitRecordRef(o);
        final PsiElement firstChild = o.getFirstChild();
        if (firstChild != null) {
          setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.RECORDS);
        }
      }

      @Override
      public void visitQAtom(@NotNull ErlangQAtom o) {
        super.visitQAtom(o);
        PsiElement atom = o.getAtom();
        PsiElement parent = o.getParent();
        PsiElement parentNextSibling = parent.getNextSibling();
        boolean needHighlighting =
          parent instanceof ErlangMaxExpression
            || parent instanceof ErlangTypeRef && (parentNextSibling == null || parentNextSibling.getNode().getElementType() != ErlangTypes.ERL_PAR_LEFT);
        if (atom != null && needHighlighting) {
          setHighlighting(atom, annotationHolder, ErlangSyntaxHighlighter.ATOM);
        }
        else if (parent instanceof ErlangTypeRef || parent instanceof ErlangTypeDefinition) {
          setHighlighting(atom, annotationHolder, ErlangSyntaxHighlighter.TYPE);
        }
      }

      @Override
      public void visitFunction(@NotNull ErlangFunction o) {
        super.visitFunction(o);

        for (ErlangFunctionClause erlangFunClause :  o.getFunctionClauseList()) {
          setHighlighting(erlangFunClause.getFirstChild(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);
        }
      }
    });
  }

  private static void highlightEdocTags(@NotNull PsiComment comment, @NotNull AnnotationHolder annotationHolder) {
    IElementType tokenType = comment.getTokenType();
    final Set<String> edocTags;
    if (tokenType == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT) {
      edocTags = ErlangDocUtil.EDOC_FUNCTION_TAGS;
    }
    else if (tokenType == ErlangParserDefinition.ERL_MODULE_DOC_COMMENT) {
      edocTags = ErlangDocUtil.EDOC_MODULE_TAGS;
    }
    else {
      return;
    }

    String commentText = comment.getText();
    List<Pair<String, Integer>> wordsWithOffset = StringUtil.getWordsWithOffset(commentText);
    for (Pair<String, Integer> pair : wordsWithOffset) {
      Integer offset = pair.second;
      String tag = pair.first;
      if (edocTags.contains(tag)) {
        TextRange range = TextRange.from(comment.getTextOffset() + offset, tag.length());
        setHighlighting(range, annotationHolder, ErlangSyntaxHighlighter.DOC_COMMENT_TAG);
      }
    }
  }

  private static void markAttributeNameAsKeyword(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder, @NotNull String name) {
    markAttributeName(o, annotationHolder, name, ErlangSyntaxHighlighter.KEYWORD);
  }

  private static void markAttributeName(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder,
                                        @NotNull String name, @NotNull TextAttributesKey key) {
    PsiElement rec = o.getFirstChild();
    while (rec != null) {
      if (rec instanceof LeafPsiElement && name.equals(rec.getText())) break;
      rec = rec.getNextSibling();
    }
    if (rec != null) {
      setHighlighting(rec, annotationHolder, key);
    }
  }

  private static void markFirstChildAsKeyword(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder) {
    markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.KEYWORD);
  }

  private static void markFirstChild(@NotNull ErlangCompositeElement o, @NotNull AnnotationHolder annotationHolder,
                                     @NotNull TextAttributesKey key) {
    final PsiElement firstChild = o.getFirstChild();
    if (firstChild != null) {
      setHighlighting(firstChild, annotationHolder, key);
    }
  }

  private static void setHighlighting(@NotNull TextRange range, @NotNull AnnotationHolder holder, @NotNull TextAttributesKey key) {
    holder.createInfoAnnotation(range, null).setEnforcedTextAttributes(TextAttributes.ERASE_MARKER);
    holder.createInfoAnnotation(range, null).setEnforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
  }

  private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder, @NotNull TextAttributesKey key) {
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(TextAttributes.ERASE_MARKER);
    holder.createInfoAnnotation(element, null).setEnforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key));
  }
}
