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

package org.intellij.erlang.formatter;

import com.intellij.formatting.Indent;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.TokenType;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.psi.ErlangArgumentDefinition;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangListOpExpression;
import org.intellij.erlang.psi.ErlangParenthesizedExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Set;

import static org.intellij.erlang.ErlangTypes.*;

public class ErlangIndentProcessor {
  private static final Set<IElementType> BIN_OPERATORS = ContainerUtil.newHashSet(
    ERL_OP_PLUS, ERL_OP_MINUS, ERL_OP_AR_MUL, ERL_OP_AR_DIV, ERL_REM,
    ERL_OR, ERL_XOR, ERL_BOR, ERL_BXOR, ERL_BSL, ERL_BSR, ERL_AND,
    ERL_BAND, ERL_OP_EQ_EQ, ERL_OP_DIV_EQ, ERL_OP_EQ_COL_EQ, ERL_OP_EQ_DIV_EQ,
    ERL_OP_LT, ERL_OP_EQ_LT, ERL_OP_GT, ERL_OP_GT_EQ, ERL_OP_LT_EQ,
    ERL_OP_EQ, ERL_OP_EXL, ERL_OP_LT_MINUS, ERL_ANDALSO, ERL_ORELSE
  );
  private final ErlangCodeStyleSettings myErlangSettings;

  public ErlangIndentProcessor(@NotNull ErlangCodeStyleSettings erlangSettings) {
    myErlangSettings = erlangSettings;
  }

  public Indent getChildIndent(ASTNode node, int binaryExpressionIndex) {
    if (binaryExpressionIndex > 0) return Indent.getNormalIndent();

    IElementType elementType = node.getElementType();
    ASTNode parent = node.getTreeParent();
    IElementType parentType = parent != null ? parent.getElementType() : null;
    ASTNode grandfather = parent != null ? parent.getTreeParent() : null;
    IElementType grandfatherType = grandfather != null ? grandfather.getElementType() : null;
    ASTNode prevSibling = FormatterUtil.getPreviousNonWhitespaceSibling(node);
    IElementType prevSiblingElementType = prevSibling != null ? prevSibling.getElementType() : null;

    if (parent == null || parent.getTreeParent() == null) {
      return Indent.getNoneIndent();
    }

    boolean containerNormal = ErlangFormattingBlock.isContainerNormal(parentType);
    boolean containerContinuation = ErlangFormattingBlock.isContainerContinuation(parentType, containerNormal);
    if (containerNormal || containerContinuation) {
      boolean initial = elementType == ERL_RADIX;

      boolean left = elementType == ERL_PAR_LEFT || elementType == ERL_CURLY_LEFT ||
        elementType == ERL_BRACKET_LEFT || elementType == ERL_BIN_START || elementType == ERL_RADIX;

      boolean right = elementType == ERL_PAR_RIGHT || elementType == ERL_CURLY_RIGHT ||
        elementType == ERL_BRACKET_RIGHT || elementType == ERL_BIN_END;

      if (initial || left || right &&
        !FormatterUtil.isPrecededBy(node, ERL_COMMA, TokenType.ERROR_ELEMENT) &&
        !FormatterUtil.isPrecededBy(node, ERL_OR_OR, TokenType.ERROR_ELEMENT)) {
        return Indent.getNoneIndent();
      }
      return containerContinuation ? Indent.getContinuationIndent() : Indent.getNormalIndent();
    }

    if ((parentType == ERL_GUARD || parentType == ERL_CLAUSE_GUARD && elementType == ERL_WHEN) && grandfatherType != ERL_IF_CLAUSE) {
      ASTNode clauseGuard = parentType == ERL_GUARD ? grandfather : parent;
      boolean inArgumentList = clauseGuard != null && clauseGuard.getTreeParent() != null &&
                               clauseGuard.getTreeParent().getElementType() == ERL_ARGUMENT_LIST;
      int guardIndentSize = myErlangSettings.GUARD_INDENT_SIZE;
      return guardIndentSize >= 0 && !inArgumentList ? Indent.getSpaceIndent(guardIndentSize) : Indent.getNormalIndent();
    }
    if (parentType == ERL_RECORD_TUPLE) {
      // todo: not a smart solution
      boolean insideCall = PsiTreeUtil.getParentOfType(node.getPsi(), ErlangArgumentDefinition.class, ErlangParenthesizedExpression.class) != null;
      return insideCall ? Indent.getNormalIndent() : Indent.getNoneIndent();
    }
    if (parentType == ERL_BEGIN_END_BODY || parentType == ERL_TRY_EXPRESSIONS_CLAUSE || parentType == ERL_MAYBE_MATCH_EXPRS ) {
      return Indent.getNoneIndent();
    }
    if (parentType == ERL_TRY_CLAUSES || parentType == ERL_FUN_CLAUSES) {
      return Indent.getNormalIndent();
    }
    if (parentType == ERL_CASE_EXPRESSION || parentType == ERL_RECEIVE_EXPRESSION || parentType == ERL_TRY_EXPRESSION ||
        parentType == ERL_BEGIN_END_EXPRESSION || parentType == ERL_IF_EXPRESSION || parentType == ERL_FUN_EXPRESSION || 
        parentType == ERL_CATCH_EXPRESSION || parentType == ERL_MAYBE_EXPRESSION ) {
      if (elementType == ERL_CR_CLAUSE || elementType == ERL_IF_CLAUSE || elementType == ERL_BEGIN_END_BODY ||
        elementType == ERL_TRY_EXPRESSIONS_CLAUSE || elementType == ERL_MAYBE_MATCH_EXPRS || elementType == ERL_AFTER_CLAUSE_BODY) {
        return Indent.getNormalIndent(myErlangSettings.INDENT_RELATIVE);
      }
      if (elementType == ERL_OF || elementType == ERL_CATCH || elementType == ERL_ELSE || elementType == ERL_AFTER || elementType == ERL_END ||
        elementType == ERL_TRY_CLAUSES || elementType == ERL_FUN_CLAUSES) {
        return myErlangSettings.INDENT_RELATIVE ? Indent.getSpaceIndent(0, true) : Indent.getNoneIndent();
      }
      if (parentType == ERL_CASE_EXPRESSION && elementType != ERL_CASE) {
        return Indent.getNormalIndent(myErlangSettings.INDENT_RELATIVE);
      }
    }
    if (ErlangParserDefinition.COMMENTS.contains(elementType) && parentType == ERL_TRY_EXPRESSION) {
      return Indent.getNormalIndent();
    }
    if (needIndent(parentType)) {
      return Indent.getNormalIndent();
    }
    if (node.getPsi() instanceof ErlangListOpExpression) {
      return Indent.getNoneIndent();
    }
    if (parent.getPsi() instanceof ErlangListOpExpression && (grandfather == null || grandfather.getPsi() instanceof ErlangExpression)) {
      return Indent.getNormalIndent();
    }
    if (parentType == ERL_TOP_TYPE && prevSibling != null) {
      return getTopTypeContinuationIndent(parent);
    }
    if (elementType == ERL_TOP_TYPE && parentType == ERL_TYPE_DEFINITION) {
      return Indent.getNormalIndent();
    }
    if (parentType == ERL_PREFIX_EXPRESSION) {
      return Indent.getNoneIndent();
    }
    if (parent.getPsi() instanceof ErlangExpression && (BIN_OPERATORS.contains(elementType) || BIN_OPERATORS.contains(prevSiblingElementType))) {
      return Indent.getNormalIndent();
    }
    return Indent.getNoneIndent();
  }

  /**
   * Indent of the lines of a multi-line type union after its first member. The indent is relative to the line
   * where the union starts: if the union, or the list item that holds it, starts a line, the union lines are
   * aligned with it; otherwise they get the indent the item itself would get on a new line.
   */
  @NotNull
  private Indent getTopTypeContinuationIndent(@NotNull ASTNode topType) {
    ASTNode item = topType;
    ASTNode container = item.getTreeParent();
    while (true) {
      if (startsLine(item) || isForcedToNewLine(item)) return Indent.getNoneIndent();
      if (container == null || isTopTypeContainer(container.getElementType())) break;
      item = container;
      container = item.getTreeParent();
    }
    IElementType containerType = container != null ? container.getElementType() : null;
    boolean containerNormal = ErlangFormattingBlock.isContainerNormal(containerType);
    boolean containerContinuation = ErlangFormattingBlock.isContainerContinuation(containerType, containerNormal);
    return containerContinuation ? Indent.getContinuationIndent() : Indent.getNormalIndent();
  }

  /**
   * Whether the formatter itself moves the node to a new line, which {@link #startsLine} does not see yet.
   */
  private boolean isForcedToNewLine(@NotNull ASTNode node) {
    if (myErlangSettings.NEW_LINE_AFTER_ARROW == ErlangCodeStyleSettings.NewLineAfterArrow.DO_NOT_FORCE) return false;
    ASTNode prevLeaf = FormatterUtil.getPreviousNonWhitespaceLeaf(node);
    return prevLeaf != null && prevLeaf.getElementType() == ERL_ARROW;
  }

  static boolean isTopTypeContainer(@NotNull IElementType type) {
    return type == ERL_TYPE_DEFINITION || needIndent(type) ||
      ErlangFormattingBlock.isContainerNormal(type) || ErlangFormattingBlock.isContainerContinuation(type, false);
  }

  static boolean startsLine(@NotNull ASTNode node) {
    PsiElement prevLeaf = PsiTreeUtil.prevLeaf(node.getPsi());
    return prevLeaf == null || prevLeaf instanceof PsiWhiteSpace && prevLeaf.textContains('\n');
  }

  private static boolean needIndent(@Nullable IElementType type) {
    return ErlangFormattingBlock.BLOCKS_TOKEN_SET.contains(type);
  }
}
