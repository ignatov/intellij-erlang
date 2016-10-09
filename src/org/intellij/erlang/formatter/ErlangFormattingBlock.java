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

import com.intellij.formatting.*;
import com.intellij.formatting.alignment.AlignmentStrategy;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.formatter.WrappingUtil;
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

public class ErlangFormattingBlock extends AbstractBlock {
  public static final TokenSet BLOCKS_TOKEN_SET = TokenSet.create(
    ERL_CLAUSE_BODY,
    ERL_MACROS_BODY,
    ERL_TUPLE_EXPRESSION,
    ERL_LIST_EXPRESSION,
    ERL_TRY_CLAUSES,
    ERL_CATCH_EXPRESSION,
    ERL_BEGIN_END_BODY,
    ERL_TOP_TYPE_CLAUSE,
    ERL_FUN_CLAUSES,
    ERL_TRY_EXPRESSIONS_CLAUSE,
    ERL_TYPE_SIG_GUARD,
    ERL_TOP_TYPE_CLAUSE
  );
  private static final TokenSet CURLY_CONTAINERS = TokenSet.create(
    ERL_TUPLE_EXPRESSION, ERL_RECORD_TUPLE, ERL_TYPED_RECORD_FIELDS, ERL_RECORD_LIKE_TYPE, ERL_MAP_TUPLE
  );
  private static final TokenSet PARENTHESIS_CONTAINERS = TokenSet.create(
    ERL_PARENTHESIZED_EXPRESSION, ERL_ARGUMENT_LIST, ERL_ARGUMENT_DEFINITION_LIST, ERL_FUN_TYPE, ERL_FUN_TYPE_ARGUMENTS
  );
  private static final TokenSet BRACKETS_CONTAINERS = TokenSet.create(
    ERL_LIST_EXPRESSION, ERL_EXPORT_FUNCTIONS, ERL_EXPORT_TYPES, ERL_BINARY_EXPRESSION
  );
  private static final TokenSet BINARY_EXPRESSIONS = TokenSet.create(
    ERL_LIST_OP_EXPRESSION, ERL_ASSIGNMENT_EXPRESSION, ERL_SEND_EXPRESSION, //right-assoc
    ERL_ADDITIVE_EXPRESSION, ERL_MULTIPLICATIVE_EXPRESSION
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
                               @NotNull SpacingBuilder spacingBuilder,
                               int binaryExpressionIndex) {
    super(node, wrap, alignment);
    myAlignmentStrategy = alignmentStrategy;
    mySettings = settings;
    myErlangSettings = erlangSettings;
    mySpacingBuilder = spacingBuilder;
    myIndent = new ErlangIndentProcessor(erlangSettings).getChildIndent(node, binaryExpressionIndex);
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
    return new ArrayList<>(mySubBlocks);
  }

  private List<Block> buildSubBlocks() {
    final List<Block> blocks = new ArrayList<>();
    final Alignment baseAlignment = Alignment.createAlignment(true);
    final Alignment baseAlignment2 = Alignment.createAlignment(true);
    final AlignmentStrategy alignmentStrategy = createOrGetAlignmentStrategy();
    final Ref<Wrap> chopDownIfLongWrap = new Ref<>();

    // if uniform binary expressions option is enabled, blocks for binary expression sequences are built flat, that is
    // for an expression like 1 + 1 + 1 a single parent block with 5 children in it is constructed.
    if (myErlangSettings.UNIFORM_BINARY_EXPRESSIONS && BINARY_EXPRESSIONS.contains(myNode.getElementType())) {
      class BinaryExpressionSequenceBlocksBuilder {
        private int myBinaryExpressionIndex = 0;
        private void build(ASTNode node) {
          for (ASTNode child = node.getFirstChildNode(); child != null; child = child.getTreeNext()) {
            if (!shouldCreateBlockFor(child)) continue;
            IElementType childType = child.getElementType();
            if (BINARY_EXPRESSIONS.contains(childType) ||
              myBinaryExpressionIndex != 0 &&
                childType == ERL_MAX_EXPRESSION &&
                child.getFirstChildNode() != null &&
                child.getFirstChildNode().getElementType() == ERL_STRING_LITERAL) {
              build(child);
            }
            else {
              blocks.add(createChildBlock(node, child, chopDownIfLongWrap, baseAlignment, baseAlignment2, alignmentStrategy, myBinaryExpressionIndex));
              myBinaryExpressionIndex++;
            }
          }
        }
      }
      new BinaryExpressionSequenceBlocksBuilder().build(myNode);
    }
    else {
      for (ASTNode child = myNode.getFirstChildNode(); child != null; child = child.getTreeNext()) {
        if (!shouldCreateBlockFor(child)) continue;
        blocks.add(createChildBlock(myNode, child, chopDownIfLongWrap, baseAlignment, baseAlignment2, alignmentStrategy, -1));
      }
    }
    return Collections.unmodifiableList(blocks);
  }

  private static boolean shouldCreateBlockFor(ASTNode node) {
    return node.getTextRange().getLength() != 0 && node.getElementType() != TokenType.WHITE_SPACE;
  }

  private ErlangFormattingBlock createChildBlock(ASTNode parent,
                                                 ASTNode child,
                                                 Ref<Wrap> chopDownIfLongWrap,
                                                 Alignment baseAlignment,
                                                 Alignment baseAlignment2,
                                                 @Nullable AlignmentStrategy alignmentStrategy,
                                                 int binaryExpressionIndex) {
    Alignment alignment = getAlignment(parent, child, baseAlignment, baseAlignment2, binaryExpressionIndex);
    WrapType wrapType = calculateWrapType(parent, child);
    Wrap wrap;
    if (wrapType == WrapType.CHOP_DOWN_IF_LONG) {
      if (chopDownIfLongWrap.isNull()) {
        chopDownIfLongWrap.set(Wrap.createWrap(wrapType, true));
      }
      wrap = chopDownIfLongWrap.get();
    }
    else if (wrapType == null) {
      wrap = null;
    }
    else {
      wrap = Wrap.createWrap(wrapType, true);
    }
    return new ErlangFormattingBlock(child, alignment, alignmentStrategy, wrap, mySettings, myErlangSettings, mySpacingBuilder, binaryExpressionIndex);
  }

  @Nullable
  private WrapType calculateWrapType(@NotNull ASTNode parent, @NotNull ASTNode node) {
    IElementType parentType = parent.getElementType();
    PsiElement nodePsi = node.getPsi();
    PsiElement parentPsi = parent.getPsi();
    if (parentType == ERL_CLAUSE_BODY && nodePsi instanceof ErlangExpression) {
      return WrappingUtil.getWrapType(myErlangSettings.EXPRESSION_IN_CLAUSE_WRAP);
    }
    if (parentType == ERL_ARGUMENT_LIST && nodePsi instanceof ErlangExpression) {
      return WrappingUtil.getWrapType(mySettings.CALL_PARAMETERS_WRAP);
    }
    if (parentPsi instanceof ErlangFakeBinaryExpression && nodePsi instanceof ErlangExpression) {
      return WrappingUtil.getWrapType(mySettings.BINARY_OPERATION_WRAP);
    }
    return null;
  }

  @Nullable
  private Alignment getAlignment(@NotNull ASTNode parent, @NotNull ASTNode child, @Nullable Alignment baseAlignment, @Nullable Alignment baseAlignment2, int binaryExpressionIndex) {
    IElementType childType = child.getElementType();
    IElementType parentType = parent.getElementType();
    Alignment fromStrategy = calculateAlignmentFromStrategy(parent, child);
    if (fromStrategy != null) return fromStrategy;

    if (PARENTHESIS_CONTAINERS.contains(parentType)) {
      if (childType != ERL_PAR_LEFT && childType != ERL_PAR_RIGHT && childType != ERL_COMMA) {if (myErlangSettings.ALIGN_MULTILINE_BLOCK) return baseAlignment;}
      else if (myErlangSettings.NEW_LINE_BEFORE_COMMA) return baseAlignment2;
    }
    if (CURLY_CONTAINERS.contains(parentType)) {
      if (childType != ERL_CURLY_LEFT && childType != ERL_CURLY_RIGHT && childType != ERL_COMMA && childType != ERL_RADIX) {
        if (myErlangSettings.ALIGN_MULTILINE_BLOCK) return baseAlignment;
      }
      else if (myErlangSettings.NEW_LINE_BEFORE_COMMA) return baseAlignment2;
    }
    if (BRACKETS_CONTAINERS.contains(parentType)) {
      if (childType != ERL_BRACKET_LEFT && childType != ERL_BRACKET_RIGHT && childType != ERL_BIN_START && childType != ERL_BIN_END &&
        childType != ERL_COMMA && childType != ERL_OP_OR) {if (myErlangSettings.ALIGN_MULTILINE_BLOCK) return baseAlignment;}
      else if (myErlangSettings.NEW_LINE_BEFORE_COMMA) return baseAlignment2;
    }
    if (myErlangSettings.ALIGN_MULTILINE_BLOCK) {
      if (parentType == ERL_LIST_COMPREHENSION) {
        boolean bracketsCurliesOrComma = childType == ERL_BRACKET_LEFT || childType == ERL_BRACKET_RIGHT || childType == ERL_COMMA ||
          childType == ERL_CURLY_LEFT || childType == ERL_CURLY_RIGHT || childType == ERL_RADIX;
        if (!bracketsCurliesOrComma && childType != ERL_BIN_START && childType != ERL_BIN_END && childType != ERL_LC_EXPRESSION) return baseAlignment;
      }
      if (parentType == ERL_FUN_TYPE_SIGS && childType == ERL_TYPE_SIG) {
        return baseAlignment;
      }
      PsiElement psi = parent.getPsi();
      if ((psi instanceof ErlangFakeBinaryExpression || parentType == ERL_MAX_EXPRESSION && childType == ERL_STRING_LITERAL) &&
        (binaryExpressionIndex > 1 || binaryExpressionIndex < 0)) {
        return baseAlignment;
      }
    }
    if (myErlangSettings.ALIGN_GUARDS && parentType == ERL_GUARD && childType != ERL_COMMA) return baseAlignment;
    return null;
  }

  @Nullable
  private Alignment calculateAlignmentFromStrategy(@NotNull ASTNode parent, ASTNode child) {
    @NotNull IElementType childType = child.getElementType();
    @NotNull IElementType parentType = parent.getElementType();
    if (myAlignmentStrategy != null) {
      Alignment alignment = myAlignmentStrategy.getAlignment(parentType, childType);
      if (alignment != null &&
        childType == ERL_CLAUSE_BODY && myErlangSettings.ALIGN_FUNCTION_CLAUSES &&
        (myErlangSettings.NEW_LINE_AFTER_ARROW == ErlangCodeStyleSettings.NewLineAfterArrow.FORCE || StringUtil.countNewLines(child.getText()) > 0)) {
        return null; // redesign this hack
      }
      return alignment;
    }
    return null;
  }

  @Nullable
  private AlignmentStrategy createOrGetAlignmentStrategy() {
    PsiElement psi = getNode().getPsi();
    if (myErlangSettings.ALIGN_FUNCTION_CLAUSES && psi instanceof ErlangFunction) {
      return AlignmentStrategy.createAlignmentPerTypeStrategy(ContainerUtil.list(ERL_CLAUSE_BODY), ERL_FUNCTION_CLAUSE, true);
    }
    if (myErlangSettings.ALIGN_FUN_CLAUSES && psi instanceof ErlangFunExpression) {
      return AlignmentStrategy.createAlignmentPerTypeStrategy(ContainerUtil.list(ERL_FUN_CLAUSE), ERL_FUN_CLAUSES, true);
    }
    if (myErlangSettings.ALIGN_RECORD_FIELD_ASSIGNMENTS && psi instanceof ErlangRecordTuple) {
      return AlignmentStrategy.createAlignmentPerTypeStrategy(ContainerUtil.list(ERL_OP_EQ), ERL_RECORD_FIELD, true);
    }

    return myAlignmentStrategy;
  }

  @Override
  @Nullable
  public Spacing getSpacing(@Nullable Block child1, @NotNull Block child2) {
    if (child2 instanceof ErlangFormattingBlock) {
      ASTNode node = ((ErlangFormattingBlock) child2).getNode();
      if (COMMENTS.contains(node.getElementType()) && mySettings.KEEP_FIRST_COLUMN_COMMENT) {
        return Spacing.createKeepingFirstColumnSpacing(0, Integer.MAX_VALUE, true, mySettings.KEEP_BLANK_LINES_IN_CODE);
      }
    }
    //adds a space between fun and and fun name var in named fun expressions
    if (child1 instanceof ErlangFormattingBlock && child2 instanceof ErlangFormattingBlock &&
      ((ErlangFormattingBlock) child1).getNode().getElementType() == ERL_FUN &&
      ((ErlangFormattingBlock) child2).getNode().getElementType() == ERL_FUN_CLAUSES) {
      ErlangFunClauses funClauses = (ErlangFunClauses) ((ErlangFormattingBlock) child2).getNode().getPsi();
      List<ErlangFunClause> funClauseList = funClauses.getFunClauseList();
      if (!funClauseList.isEmpty() && funClauseList.get(0).getArgumentDefinition() != null) {
        return Spacing.createSpacing(1, 1, 0, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
      }
    }
    //force newline after arrow
    if (myErlangSettings.NEW_LINE_AFTER_ARROW != ErlangCodeStyleSettings.NewLineAfterArrow.DO_NOT_FORCE &&
      child1 instanceof ErlangFormattingBlock && ((ErlangFormattingBlock) child1).getNode().getElementType() == ERL_ARROW) {
      int spaceAroundArrow = myErlangSettings.SPACE_AROUND_ARROW ? 1 : 0;
      if (myErlangSettings.NEW_LINE_AFTER_ARROW == ErlangCodeStyleSettings.NewLineAfterArrow.FORCE_EXCEPT_ONE_LINE_CLAUSES) {
        TextRange dependency = TextRange.create(child2.getTextRange().getStartOffset(), this.getTextRange().getEndOffset());
        return Spacing.createDependentLFSpacing(spaceAroundArrow, spaceAroundArrow, dependency, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
      }
      return Spacing.createSpacing(spaceAroundArrow, spaceAroundArrow, 1, mySettings.KEEP_LINE_BREAKS, mySettings.KEEP_BLANK_LINES_IN_CODE);
    }
    return mySpacingBuilder.getSpacing(this, child1, child2);
  }

  @NotNull
  @Override
  public ChildAttributes getChildAttributes(int newChildIndex) {
    Indent childIndent = getChildIndent(myNode.getElementType(), newChildIndex);
    IElementType type = getPreviousElementType(newChildIndex);
    Alignment alignment = getChildAlignment(type);
    if (childIndent != null) return new ChildAttributes(childIndent, alignment);
    if (type != null) childIndent = getChildIndentByPreviousChild(type, newChildIndex);
    return new ChildAttributes(childIndent == null ? Indent.getNoneIndent() : childIndent, alignment);
  }

  @Nullable
  private Alignment getChildAlignment(@Nullable IElementType type) {
    IElementType parentType = getNode().getElementType();
    List<Block> subBlocks = getSubBlocks();
    if (type != ERL_COMMA && myErlangSettings.NEW_LINE_BEFORE_COMMA) {
      if (BRACKETS_CONTAINERS.contains(parentType) || parentType == ERL_RECORD_TUPLE) {
        return subBlocks.get(0).getAlignment();
      }
    }
    if (type == ERL_COMMA && myErlangSettings.ALIGN_MULTILINE_BLOCK) {
      if (BRACKETS_CONTAINERS.contains(parentType) || parentType == ERL_RECORD_TUPLE) {
        return subBlocks.size() > 1 ? subBlocks.get(1).getAlignment() : null;
      }
    }
    return null;
  }

  @Nullable
  private IElementType getPreviousElementType(int newChildIndex) {
    if (newChildIndex <= 0) return null;
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
  private Indent getChildIndent(@Nullable IElementType type, int newChildIndex) {
    if (
      type == ERL_IF_EXPRESSION && newChildIndex == 1 ||
      type == ERL_CASE_EXPRESSION && newChildIndex == 1 ||
      type == ERL_BEGIN_END_EXPRESSION && newChildIndex == 1 ||
      type == ERL_FUN_EXPRESSION && newChildIndex == 1 ||
      type == ERL_RECEIVE_EXPRESSION && (newChildIndex == 1 || newChildIndex == 3 || newChildIndex == 5) ||
      type == ERL_TRY_EXPRESSION && (newChildIndex == 1 || newChildIndex == 3 || newChildIndex == 5)) {
      return Indent.getNormalIndent(myErlangSettings.INDENT_RELATIVE);
    }

    if (type == ERL_IF_EXPRESSION || type == ERL_CASE_EXPRESSION ||
      type == ERL_RECEIVE_EXPRESSION || type == ERL_BEGIN_END_EXPRESSION || type == ERL_FUN_EXPRESSION) {
      IElementType previousElement = getPreviousElementType(newChildIndex);
      if (previousElement != null &&
        previousElement != ERL_OF && previousElement != ERL_AFTER && previousElement != ERL_SEMI) {
        return Indent.getSpaceIndent(0, myErlangSettings.INDENT_RELATIVE);
      }
    }

    if (type == ERL_BEGIN_END_BODY) return Indent.getNoneIndent();

    if (type == ERL_TRY_EXPRESSIONS_CLAUSE && newChildIndex == 1) return Indent.getNoneIndent();

    if (BLOCKS_TOKEN_SET.contains(type) || type == ERL_TYPED_RECORD_FIELDS) {
      return Indent.getNormalIndent(false);
    }

    boolean containerNormal = isContainerNormal(type);
    boolean containerContinuation = isContainerContinuation(type, containerNormal);

    if (containerNormal || containerContinuation) {
      IElementType previousElement = newChildIndex != 1 ? getPreviousElementType(newChildIndex) : null;
      if (newChildIndex == 1 ||
        newChildIndex == 2 && type == ERL_MAP_TUPLE ||
        previousElement != null && previousElement == ERL_COMMA) {
        return containerContinuation ? Indent.getContinuationIndent() : Indent.getNormalIndent();
      }
    }

    return null;
  }

  @Nullable
  private Indent getChildIndentByPreviousChild(@Nullable IElementType type, int newChildIndex) {
    if (getNode().getPsi() instanceof ErlangFunction && type == ERL_SEMI) return Indent.getNoneIndent();

    if (type == ERL_SEMI || type == ERL_OF && newChildIndex == 3)  {
      return Indent.getNormalIndent(myErlangSettings.INDENT_RELATIVE);
    }

    return null;
  }

  @Override
  public boolean isLeaf() {
    return myNode.getFirstChildNode() == null;
  }

  public static boolean isContainerNormal(@Nullable IElementType type) {
    return type == ERL_ARGUMENT_LIST || type == ERL_LIST_COMPREHENSION ||
      CURLY_CONTAINERS.contains(type) || BRACKETS_CONTAINERS.contains(type);
  }

  public static boolean isContainerContinuation(@Nullable IElementType type, boolean containerNormal) {
    return !containerNormal && PARENTHESIS_CONTAINERS.contains(type);
  }
}
