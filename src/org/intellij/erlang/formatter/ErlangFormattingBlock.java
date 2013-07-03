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

package org.intellij.erlang.formatter;

import com.intellij.formatting.*;
import com.intellij.formatting.alignment.AlignmentStrategy;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.formatter.common.AbstractBlock;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.ErlangParserDefinition.COMMENTS;
import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangFormattingBlock extends AbstractBlock {
  public static final TokenSet BLOCKS_TOKEN_SET = TokenSet.create(
    ERL_CLAUSE_BODY,
    ERL_MACROS_BODY,
    ERL_TUPLE_EXPRESSION,
    ERL_LIST_EXPRESSION,
    ERL_IF_CLAUSES,
    ERL_TRY_CLAUSES,
    ERL_CATCH_EXPRESSION,
    ERL_BEGIN_END_BODY,
    ERL_TOP_TYPE_CLAUSE,
    ERL_FUN_CLAUSES,
    ERL_TRY_EXPRESSIONS_CLAUSE,
    ERL_TYPE_SIG_GUARD,
    ERL_AFTER_CLAUSE
  );

  private final Indent myIndent;
  private final AlignmentStrategy myAlignmentStrategy;
  private final CommonCodeStyleSettings mySettings;
  private final ErlangCodeStyleSettings myErlangSettings;
  private final SpacingBuilder mySpacingBuilder;
  private List<Block> mySubBlocks;

  public ErlangFormattingBlock(@NotNull ASTNode node,
                               @Nullable Alignment alignment,
                               @Nullable AlignmentStrategy alignmentStrategy,
                               @Nullable Wrap wrap,
                               @NotNull CommonCodeStyleSettings settings,
                               @NotNull ErlangCodeStyleSettings erlangSettings,
                               @NotNull SpacingBuilder spacingBuilder) {
    super(node, wrap, alignment);
    myAlignmentStrategy = alignmentStrategy;
    mySettings = settings;
    myErlangSettings = erlangSettings;
    mySpacingBuilder = spacingBuilder;
    myIndent = new ErlangIndentProcessor(myErlangSettings).getChildIndent(node);
  }

  @Override
  public Indent getIndent() {
    return myIndent;
  }

  @NotNull
  @Override
  protected List<Block> buildChildren() {
    if (mySubBlocks == null) {
      mySubBlocks = buildSubBlocks();
    }
    return new ArrayList<Block>(mySubBlocks);
  }

  private List<Block> buildSubBlocks() {
    List<Block> blocks = new ArrayList<Block>();
    Alignment baseAlignment = Alignment.createAlignment();
    AlignmentStrategy alignmentStrategy = createOrGetAlignmentStrategy();

    for (ASTNode child = myNode.getFirstChildNode(); child != null; child = child.getTreeNext()) {
      IElementType childType = child.getElementType();
      if (child.getTextRange().getLength() == 0 || childType == TokenType.WHITE_SPACE) continue;

      Alignment alignment = getAlignment(getNode(), child, baseAlignment);
      blocks.add(new ErlangFormattingBlock(child, alignment, alignmentStrategy, null, mySettings, myErlangSettings, mySpacingBuilder));
    }
    return Collections.unmodifiableList(blocks);
  }

  @Nullable
  private Alignment getAlignment(@NotNull ASTNode parent, @NotNull ASTNode child, @Nullable Alignment baseAlignment) {
    IElementType childType = child.getElementType();
    IElementType parentType = parent.getElementType();
    Alignment fromStrategy = calculateAlignmentFromStrategy(parent, child);
    if (fromStrategy != null) return fromStrategy;

    if (myErlangSettings.ALIGN_MULTILINE_BLOCK) {
      if (parentType == ERL_PARENTHESIZED_EXPRESSION || parentType == ERL_ARGUMENT_LIST
        || parentType == ERL_ARGUMENT_DEFINITION_LIST || parentType == ERL_FUN_TYPE) {
        if (childType != ERL_PAR_LEFT && childType != ERL_PAR_RIGHT) {
          return baseAlignment;
        }
      }
      if (parentType == ERL_TUPLE_EXPRESSION || parentType == ERL_RECORD_TUPLE || parentType == ERL_TYPED_RECORD_FIELDS || parentType == ERL_RECORD_FIELDS) {
        if (childType != ERL_CURLY_LEFT && childType != ERL_CURLY_RIGHT) {
          return baseAlignment;
        }
      }
      if (parentType == ERL_LIST_EXPRESSION || parentType == ERL_LIST_COMPREHENSION || parentType == ERL_EXPORT_FUNCTIONS || parentType == ERL_EXPORT_TYPES) {
        if (childType != ERL_BRACKET_LEFT && childType != ERL_BRACKET_RIGHT && childType != ERL_BIN_START && childType != ERL_BIN_END && childType != ERL_LC_EXPRS) {
          return baseAlignment;
        }
      }
      if (parentType == ERL_FUN_TYPE_SIGS) {
        if (childType == ERL_TYPE_SIG) {
          return baseAlignment;
        }
      }
      PsiElement psi = parent.getPsi();
      if (psi instanceof ErlangFakeBinaryExpression) {
        return baseAlignment;
      }
    }
    return null;
  }

  @Nullable
  private Alignment calculateAlignmentFromStrategy(@NotNull ASTNode parent, ASTNode child) {
    @NotNull IElementType childType = child.getElementType();
    @NotNull IElementType parentType = parent.getElementType();
    if (myAlignmentStrategy != null) {
      Alignment alignment = myAlignmentStrategy.getAlignment(parentType, childType);
      if (alignment != null && childType == ERL_CLAUSE_BODY && myErlangSettings.ALIGN_FUNCTION_CLAUSES && StringUtil.countNewLines(child.getText()) > 0) return null; // redesign this hack

      return alignment;
    }
    return null;
  }

  @Nullable
  private AlignmentStrategy createOrGetAlignmentStrategy() {
    PsiElement psi = getNode().getPsi();
    if (myErlangSettings.ALIGN_FUNCTION_CLAUSES && psi instanceof ErlangFunction) {
      return AlignmentStrategy.createAlignmentPerTypeStrategy(ContainerUtil.<IElementType>list(ERL_CLAUSE_BODY), ERL_FUNCTION_CLAUSE, true);
    }
    return myAlignmentStrategy;
  }

  @Override
  @Nullable
  public Spacing getSpacing(@Nullable Block child1, @NotNull Block child2) {
    if (child2 instanceof ErlangFormattingBlock) {
      if (COMMENTS.contains(((ErlangFormattingBlock) child2).getNode().getElementType()) && mySettings.KEEP_FIRST_COLUMN_COMMENT) {
        return Spacing.createKeepingFirstColumnSpacing(0, Integer.MAX_VALUE, true, mySettings.KEEP_BLANK_LINES_IN_CODE);
      }
    }
    return mySpacingBuilder.getSpacing(this, child1, child2);
  }

  @NotNull
  @Override
  public ChildAttributes getChildAttributes(int newChildIndex) {
    Indent childIndent = getChildIndent(myNode.getElementType(), newChildIndex);

    if (childIndent != null) return new ChildAttributes(childIndent, null);

    IElementType type = newChildIndex > 0 ? getIElementType(newChildIndex) : null;
    if (type != null) childIndent = getChildIndent(type, newChildIndex);
    
    return new ChildAttributes(childIndent == null ? Indent.getNoneIndent() : childIndent, null);
  }

  @Nullable
  private IElementType getIElementType(int newChildIndex) {
    Block block = getSubBlocks().get(newChildIndex - 1);
    while (block instanceof ErlangFormattingBlock && !block.getSubBlocks().isEmpty()) {
      List<Block> subBlocks = block.getSubBlocks();
      Block childBlock = subBlocks.get(subBlocks.size() - 1);
      if (!(childBlock instanceof ErlangFormattingBlock)) break;
      else {
        ASTNode node = ((ErlangFormattingBlock) childBlock).getNode();
        PsiElement psi = node.getPsi();
        IElementType elementType = node.getElementType();
        if (elementType instanceof ErlangTokenType) break;
        if (psi instanceof LeafPsiElement || psi instanceof ErlangQAtom || psi instanceof ErlangQVar) break;
      }
      block = childBlock;
    }
    return block instanceof ErlangFormattingBlock ? ((ErlangFormattingBlock) block).getNode().getElementType() : null;
  }

  @Nullable
  private static Indent getChildIndent(@Nullable IElementType type, int newChildIndex) {
      if (
      type == ERL_IF_EXPRESSION ||
      type == ERL_CASE_EXPRESSION ||
      type == ERL_BEGIN_END_EXPRESSION ||
      type == ERL_AFTER_CLAUSE ||
      type == ERL_FUN_EXPRESSION && newChildIndex == 1 ||
      type == ERL_RECEIVE_EXPRESSION && newChildIndex == 1 ||
      type == ERL_TRY_CATCH && newChildIndex == 1 ||
      type == ERL_TRY_EXPRESSION && newChildIndex == 1 ||
      type == ERL_TRY_EXPRESSION && newChildIndex == 3 ||
      type == ERL_SEMI) {
      return Indent.getNormalIndent(true);
    }

    if (type == ERL_BEGIN_END_BODY) return Indent.getNoneIndent();

    if (type == ERL_TRY_EXPRESSIONS_CLAUSE && newChildIndex == 1) return Indent.getNoneIndent();

    if (BLOCKS_TOKEN_SET.contains(type) ||
      type == ERL_TYPED_RECORD_FIELDS
      ) return Indent.getNormalIndent(false);

    return null;
  }

  @Override
  public boolean isLeaf() {
    return myNode.getFirstChildNode() == null;
  }
}
