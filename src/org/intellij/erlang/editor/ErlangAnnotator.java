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
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.documentation.ErlangDocUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

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
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitCallbackSpec(@NotNull ErlangCallbackSpec o) {
        super.visitCallbackSpec(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitSpecification(@NotNull ErlangSpecification o) {
        super.visitSpecification(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitAttribute(@NotNull ErlangAttribute o) {
        super.visitAttribute(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitExport(@NotNull ErlangExport o) {
        super.visitExport(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitExportTypeAttribute(@NotNull ErlangExportTypeAttribute o) {
        super.visitExportTypeAttribute(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitImportDirective(@NotNull ErlangImportDirective o) {
        super.visitImportDirective(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitInclude(@NotNull ErlangInclude o) {
        super.visitInclude(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
        markNameAsAttribute(o, annotationHolder, "include");
      }

      @Override
      public void visitFunctionWithArity(@NotNull ErlangFunctionWithArity o) {
        super.visitFunctionWithArity(o);
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);        
      }

      @Override
      public void visitExportFunction(@NotNull ErlangExportFunction o) {
        super.visitExportFunction(o);
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);
      }

      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        super.visitFunctionCallExpression(o);
        TextAttributesKey key = o.getParent() instanceof ErlangGuard ? ErlangSyntaxHighlighter.GUARD : ErlangSyntaxHighlighter.FUNCTION_CALL;
        markCall(o.getQAtom(), annotationHolder, key);
      }

      @Override
      public void visitGenericFunctionCallExpression(@NotNull ErlangGenericFunctionCallExpression o) {
        super.visitGenericFunctionCallExpression(o);
        markCall(PsiTreeUtil.getPrevSiblingOfType(o.getArgumentList(), ErlangQAtom.class), annotationHolder, ErlangSyntaxHighlighter.FUNCTION_CALL);        
      }

      @Override
      public void visitModuleRef(@NotNull ErlangModuleRef o) {
        super.visitModuleRef(o);
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.MODULE_REF);
      }

      @Override
      public void visitIncludeLib(@NotNull ErlangIncludeLib o) {
        super.visitIncludeLib(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
        markNameAsAttribute(o, annotationHolder, "include_lib");
      }

      @Override
      public void visitModule(@NotNull ErlangModule o) {
        super.visitModule(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
        super.visitRecordDefinition(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
        markNameAsAttribute(o, annotationHolder, "record");
        ErlangQAtom nameAtom = o.getQAtom();
        if (nameAtom != null) {
          setHighlighting(nameAtom, annotationHolder, ErlangSyntaxHighlighter.RECORDS);
        }
      }

      @Override
      public void visitMacrosDefinition(@NotNull ErlangMacrosDefinition o) {
        super.visitMacrosDefinition(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
        markNameAsAttribute(o, annotationHolder, "define");
      }

      @Override
      public void visitTypeDefinition(@NotNull ErlangTypeDefinition o) {
        super.visitTypeDefinition(o);
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
        markAttributeName(o, annotationHolder, "type", ErlangSyntaxHighlighter.ATTRIBUTE);
        markAttributeName(o, annotationHolder, "opaque", ErlangSyntaxHighlighter.ATTRIBUTE);
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
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
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
          if (atom != null) {
            boolean builtIn = ErlangPsiImplUtil.BUILT_IN_TYPES.contains(atom.getText());
            setHighlighting(atom, annotationHolder, builtIn ? ErlangSyntaxHighlighter.BUILT_IN_TYPE : ErlangSyntaxHighlighter.TYPE);
          }
        }
      }

      @Override
      public void visitFunction(@NotNull ErlangFunction o) {
        super.visitFunction(o);
        for (ErlangFunctionClause erlangFunClause : o.getFunctionClauseList()) {
          setHighlighting(erlangFunClause.getFirstChild(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);
        }
      }

      @Override
      public void visitSpecFun(@NotNull ErlangSpecFun o) {
        super.visitSpecFun(o);
        //noinspection unchecked
        ErlangCompositeElement parent = PsiTreeUtil.getParentOfType(o, ErlangSpecification.class, ErlangCallbackSpec.class);
        ErlangQAtom qAtom = o.getQAtom();
        if (parent instanceof ErlangSpecification) {
          setHighlighting(qAtom, annotationHolder, ErlangSyntaxHighlighter.SPEC);
        }
        else if (parent instanceof ErlangCallbackSpec) {
          setHighlighting(qAtom, annotationHolder, ErlangSyntaxHighlighter.CALLBACK);
        }
      }
    });
  }

  private static void markCall(@Nullable ErlangQAtom atom, @NotNull AnnotationHolder annotationHolder, TextAttributesKey key) {
    if (atom != null && atom.getMacros() == null) {
      setHighlighting(atom, annotationHolder, key);
    }
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
        setHighlighting(range, annotationHolder, ErlangSyntaxHighlighter.DOC_TAG);
      }
    }
  }

  private static void markNameAsAttribute(@NotNull ErlangCompositeElement o,
                                          @NotNull AnnotationHolder annotationHolder,
                                          @NotNull String name) {
    markAttributeName(o, annotationHolder, name, ErlangSyntaxHighlighter.ATTRIBUTE);
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
