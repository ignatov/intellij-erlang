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

package org.intellij.erlang.editor;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.annotation.HighlightSeverity;
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

import java.math.BigInteger;
import java.util.List;
import java.util.Set;

public class ErlangAnnotator implements Annotator, DumbAware {
  @Override
  public void annotate(@NotNull PsiElement o, @NotNull final AnnotationHolder annotationHolder) {
    if (o instanceof PsiComment) {
      highlightEdocTags((PsiComment) o, annotationHolder);
      return;
    }

    if (o instanceof LeafPsiElement leaf) {
      IElementType type = leaf.getElementType();
      if (type == ErlangTypes.ERL_INTEGER) {
        checkBasedInteger(leaf, annotationHolder);
        return;
      }
      if (type == ErlangTypes.ERL_CHAR || type == ErlangTypes.ERL_STRING || type == ErlangTypes.ERL_SIGIL_STRING) {
        checkEscapes(leaf, annotationHolder);
        return;
      }
      if (type == ErlangTypes.ERL_ATOM_NAME) {
        checkEscapes(leaf, annotationHolder);
      }
    }

    if (o instanceof LeafPsiElement && ((LeafPsiElement) o).getElementType() == ErlangTypes.ERL_ATOM_NAME) {
      PsiElement parent = o.getParent();
      if (parent instanceof ErlangMetaAttribute ||
          parent instanceof ErlangRecordDefinition ||
          parent instanceof ErlangMacrosDefinition ||
          parent instanceof ErlangTypeDefinition ||
          parent instanceof ErlangInclude ||
          parent instanceof ErlangIncludeLib
      ) {
        setHighlighting(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
        return;
      }
    }

    o.accept(new ErlangVisitor() {
      @Override
      public void visitFunctionWithArity(@NotNull ErlangFunctionWithArity o) {
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);
      }

      @Override
      public void visitExportFunction(@NotNull ErlangExportFunction o) {
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);
      }

      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression o) {
        TextAttributesKey key = o.getParent() instanceof ErlangGuard ? ErlangSyntaxHighlighter.GUARD : ErlangSyntaxHighlighter.FUNCTION_CALL;
        markCall(o.getQAtom(), annotationHolder, key);
      }

      @Override
      public void visitGenericFunctionCallExpression(@NotNull ErlangGenericFunctionCallExpression o) {
        markCall(PsiTreeUtil.getPrevSiblingOfType(o.getArgumentList(), ErlangQAtom.class), annotationHolder, ErlangSyntaxHighlighter.FUNCTION_CALL);
      }

      @Override
      public void visitModuleRef(@NotNull ErlangModuleRef o) {
        setHighlighting(o.getQAtom(), annotationHolder, ErlangSyntaxHighlighter.MODULE_REF);
      }

      @Override
      public void visitRecordDefinition(@NotNull ErlangRecordDefinition o) {
        ErlangQAtom nameAtom = o.getQAtom();
        if (nameAtom != null) {
          setHighlighting(nameAtom, annotationHolder, ErlangSyntaxHighlighter.RECORDS);
        }
      }

      @Override
      public void visitTypeDefinition(@NotNull ErlangTypeDefinition o) {
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitElseAtomAttribute(@NotNull ErlangElseAtomAttribute o) {
        markFirstChild(o, annotationHolder, ErlangSyntaxHighlighter.ATTRIBUTE);
      }

      @Override
      public void visitMacrosName(@NotNull ErlangMacrosName o) {
        PsiElement firstChild = o.getFirstChild();
        if (firstChild != null) {
          setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.MACRO);
        }
      }

      @Override
      public void visitRecordRef(@NotNull ErlangRecordRef o) {
        PsiElement firstChild = o.getFirstChild();
        if (firstChild != null) {
          setHighlighting(firstChild, annotationHolder, ErlangSyntaxHighlighter.RECORDS);
        }
      }

      @Override
      public void visitQAtom(@NotNull ErlangQAtom o) {
        PsiElement atom = o.getAtom();
        PsiElement parent = o.getParent();
        boolean needHighlighting = ErlangPsiImplUtil.standaloneAtom(o);
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
        for (ErlangFunctionClause erlangFunClause : o.getFunctionClauseList()) {
          setHighlighting(erlangFunClause.getFirstChild(), annotationHolder, ErlangSyntaxHighlighter.FUNCTION);
        }
      }

      @Override
      public void visitSpecFun(@NotNull ErlangSpecFun o) {
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

  private static void checkBasedInteger(@NotNull PsiElement integer, @NotNull AnnotationHolder holder) {
    String text = integer.getText();
    int hash = text.indexOf('#');
    if (hash < 0) return;
    int start = integer.getTextRange().getStartOffset();
    BigInteger base = new BigInteger(text.substring(0, hash).replace("_", ""));
    if (base.compareTo(BigInteger.TWO) < 0 || base.compareTo(BigInteger.valueOf(36)) > 0) {
      holder.newAnnotation(HighlightSeverity.ERROR, "illegal base '" + base + "'").range(TextRange.from(start, hash)).create();
      return;
    }
    // erl_scan ends the integer before the first character that is not a digit of the base
    int radix = base.intValue();
    int end = hash + 1;
    while (end < text.length()) {
      if (Character.digit(text.charAt(end), radix) >= 0) {
        end++;
      }
      else if (text.charAt(end) == '_' && end > hash + 1 && end + 1 < text.length() && Character.digit(text.charAt(end + 1), radix) >= 0) {
        end += 2;
      }
      else {
        break;
      }
    }
    if (end == hash + 1) {
      holder.newAnnotation(HighlightSeverity.ERROR, "illegal integer").range(integer).create();
    }
    else if (end < text.length()) {
      holder.newAnnotation(HighlightSeverity.ERROR, "syntax error before: " + text.substring(end))
        .range(TextRange.create(start + end, start + text.length())).create();
    }
  }

  private static void checkEscapes(@NotNull PsiElement literal, @NotNull AnnotationHolder holder) {
    String text = literal.getText();
    if (isVerbatim(text)) return;
    int start = literal.getTextRange().getStartOffset();
    int i = text.indexOf('\\');
    while (i >= 0 && i + 1 < text.length()) {
      char next = text.charAt(i + 1);
      int escapeEnd = i + (next == '^' ? 3 : 2);
      if (next == '^' && (i + 2 >= text.length() || !isControlName(text.charAt(i + 2)))) {
        // erl_scan rejects the control escape and goes on at the character after the caret
        holder.newAnnotation(HighlightSeverity.ERROR, "illegal character")
          .range(TextRange.create(start + i, start + i + 2)).create();
        escapeEnd = i + 2;
      }
      if (next == 'x' && i + 2 < text.length() && text.charAt(i + 2) == '{') {
        int close = text.indexOf('}', i + 3);
        if (close > i + 3 && isHexDigits(text, i + 3, close)) {
          if (!isUnicodeCodePoint(new BigInteger(text.substring(i + 3, close), 16))) {
            holder.newAnnotation(HighlightSeverity.ERROR, "illegal character")
              .range(TextRange.create(start + i, start + close + 1)).create();
          }
          escapeEnd = close + 1;
        }
      }
      i = text.indexOf('\\', escapeEnd);
    }
  }

  private static boolean isVerbatim(@NotNull String text) {
    if (text.startsWith("\"\"\"")) return true;
    if (!text.startsWith("~") || text.length() < 2) return false;
    char modifier = text.charAt(1);
    if (modifier == 'B' || modifier == 'S') return true;
    return text.startsWith("\"\"\"", 1);
  }

  // The characters that erl_scan accepts after \^
  private static boolean isControlName(char c) {
    return c >= '@' && c <= '_' || c >= 'a' && c <= 'z' || c == '?';
  }

  private static boolean isHexDigits(@NotNull String text, int from, int to) {
    for (int i = from; i < to; i++) {
      if (Character.digit(text.charAt(i), 16) < 0) return false;
    }
    return true;
  }

  // The ?UNICODE check of erl_scan
  private static boolean isUnicodeCodePoint(@NotNull BigInteger value) {
    if (value.bitLength() > 31) return false;
    int c = value.intValue();
    return c < 0xD800 || c > 0xDFFF && c < 0xFFFE || c > 0xFFFF && c <= 0x10FFFF;
  }

  private static void markCall(@Nullable ErlangQAtom atom, @NotNull AnnotationHolder annotationHolder, TextAttributesKey key) {
    if (atom != null && atom.getMacros() == null) {
      setHighlighting(atom, annotationHolder, key);
    }
  }

  private static void highlightEdocTags(@NotNull PsiComment comment, @NotNull AnnotationHolder annotationHolder) {
    IElementType tokenType = comment.getTokenType();
    Set<String> edocTags;
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

  private static void markFirstChild(@NotNull ErlangCompositeElement o,
                                     @NotNull AnnotationHolder annotationHolder,
                                     @NotNull TextAttributesKey key) {
    PsiElement firstChild = o.getFirstChild();
    if (firstChild != null) {
      setHighlighting(firstChild, annotationHolder, key);
    }
  }

  private static void setHighlighting(@NotNull TextRange range, @NotNull AnnotationHolder holder, @NotNull TextAttributesKey key) {
    holder.newSilentAnnotation(HighlightSeverity.INFORMATION).range(range).enforcedTextAttributes(TextAttributes.ERASE_MARKER).create();
    holder.newSilentAnnotation(HighlightSeverity.INFORMATION).range(range).enforcedTextAttributes(EditorColorsManager.getInstance().getGlobalScheme().getAttributes(key)).create();
  }

  private static void setHighlighting(@NotNull PsiElement element, @NotNull AnnotationHolder holder, @NotNull TextAttributesKey key) {
    setHighlighting(element.getTextRange(), holder, key);
  }
}
