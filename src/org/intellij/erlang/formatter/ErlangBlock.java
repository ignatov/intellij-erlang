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

package org.intellij.erlang.formatter;

import com.intellij.formatting.*;
import com.intellij.formatting.templateLanguages.BlockWithParent;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.psi.ErlangFakeBinaryExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangBlock implements ASTBlock, BlockWithParent {
  public static final TokenSet BLOCKS_TOKEN_SET = TokenSet.create(
    ERL_CLAUSE_BODY,
//    ERL_TYPED_RECORD_FIELDS,
    ERL_ARGUMENT_LIST,
    ERL_TUPLE_EXPRESSION,
    ERL_LIST_EXPRESSION,
    ERL_CR_CLAUSES,
    ERL_IF_CLAUSES,
    ERL_TRY_CLAUSES,
    ERL_CATCH_EXPRESSION,
    ERL_BEGIN_END_BODY,
    ERL_TOP_TYPE_CLAUSE,
    ERL_FUN_CLAUSES,
    ERL_TRY_EXPRESSIONS_CLAUSE,
    ERL_TYPE_SIG_GUARD
  );

  private ASTNode myNode;
  private Alignment myAlignment;
  private Indent myIndent;
  private Wrap myWrap;
  private CommonCodeStyleSettings mySettings;
  private ErlangCodeStyleSettings myErlangSettings;
  private final SpacingBuilder mySpacingBuilder;
  private List<Block> mySubBlocks;
  private BlockWithParent myParent;

  public ErlangBlock(@Nullable BlockWithParent parent, // todo[ignatov]: remove parent, use AlignmentStrategy instead of
                     @NotNull ASTNode node,
                     @Nullable Alignment alignment,
                     @Nullable Wrap wrap,
                     @NotNull CommonCodeStyleSettings settings,
                     @NotNull ErlangCodeStyleSettings erlangSettings,
                     @NotNull SpacingBuilder spacingBuilder) {
    myParent = parent;
    myNode = node;
    myAlignment = alignment;
    myWrap = wrap;
    mySettings = settings;
    myErlangSettings = erlangSettings;
    mySpacingBuilder = spacingBuilder;
    myIndent = new ErlangIndentProcessor(mySettings).getChildIndent(node);
  }

  @Override
  public ASTNode getNode() {
    return myNode;
  }

  @NotNull
  @Override
  public TextRange getTextRange() {
    return myNode.getTextRange();
  }

  @Override
  public Wrap getWrap() {
    return myWrap;
  }

  @Override
  public Indent getIndent() {
    return myIndent;
  }

  @Override
  public Alignment getAlignment() {
    return myAlignment;
  }

  @NotNull
  @Override
  public List<Block> getSubBlocks() {
    if (mySubBlocks == null) {
      mySubBlocks = buildSubBlocks();
    }
    return new ArrayList<Block>(mySubBlocks);
  }

  private List<Block> buildSubBlocks() {
    List<Block> blocks = new ArrayList<Block>();
    Alignment alignment = null;
    Alignment baseAlignment = Alignment.createAlignment();
    IElementType parentType = getNode().getElementType();
    PsiElement psi = getNode().getPsi();

    for (ASTNode child = myNode.getFirstChildNode(); child != null; child = child.getTreeNext()) {
      IElementType childType = child.getElementType();

      if (child.getTextRange().getLength() == 0 || childType == TokenType.WHITE_SPACE) continue;

      if (myErlangSettings.ALIGN_MULTILINE_BLOCK) {
        if (parentType == ERL_PARENTHESIZED_EXPRESSION || parentType == ERL_ARGUMENT_LIST
          || parentType == ERL_ARGUMENT_DEFINITION_LIST || parentType == ERL_FUN_TYPE) {
          if (childType != ERL_PAR_LEFT && childType != ERL_PAR_RIGHT) {
            alignment = baseAlignment;
          }
        }
        if (parentType == ERL_TUPLE_EXPRESSION || parentType == ERL_RECORD_TUPLE || parentType == ERL_TYPED_RECORD_FIELDS) {
          if (childType != ERL_CURLY_LEFT && childType != ERL_CURLY_RIGHT) {
            alignment = baseAlignment;
          }
        }
        if (parentType == ERL_LIST_EXPRESSION || parentType == ERL_LIST_COMPREHENSION || parentType == ERL_EXPORT_FUNCTIONS) {
          if (childType != ERL_BRACKET_LEFT && childType != ERL_BRACKET_RIGHT && childType != ERL_BIN_START && childType != ERL_BIN_END && childType != ERL_LC_EXPRS) {
            alignment = baseAlignment;
          }
        }
        if (psi instanceof ErlangFakeBinaryExpression) {
          alignment = baseAlignment;
        }
      }
      boolean isEmacsStyleFunctionAlignment = false;
      //noinspection ConstantConditions
      if (isEmacsStyleFunctionAlignment && parentType == ERL_FUNCTION_CLAUSE && childType == ERL_CLAUSE_BODY) { // Emacs style alignment for function clauses
        @Nullable BlockWithParent clause = getParent();
        List<Block> subBlocks = clause instanceof  ASTBlock ? ((ASTBlock) clause).getSubBlocks() : Collections.<Block>emptyList();

        List<Block> functionsBlock = ContainerUtil.filter(subBlocks, new Condition<Block>() {
          @Override
          public boolean value(Block block) {
            return ((ASTBlock) block).getNode().getElementType() == ERL_FUNCTION_CLAUSE;
          }
        });

        Block first = ContainerUtil.getFirstItem(functionsBlock);

        if (this.equals(first)) {
          alignment = Alignment.createAlignment(true);
        }
        else if (first != null) {
          List<Block> list = first.getSubBlocks();
          Block filter = ContainerUtil.getFirstItem(ContainerUtil.filter(list, new Condition<Block>() {
            @Override
            public boolean value(Block block) {
              return ((ASTBlock) block).getNode().getElementType() == ERL_CLAUSE_BODY;
            }
          }));
          alignment = filter == null ? null : filter.getAlignment();
        }
      }

      blocks.add(buildSubBlock(child, alignment));
      alignment = null;
    }
    return Collections.unmodifiableList(blocks);
  }

  private Block buildSubBlock(@NotNull ASTNode child, @Nullable Alignment alignment) {
    return new ErlangBlock(this, child, alignment, null, mySettings, myErlangSettings, mySpacingBuilder);
  }

  @Override
  public Spacing getSpacing(Block child1, Block child2) {
    return mySpacingBuilder.getSpacing(this, child1, child2);
  }

  @NotNull
  @Override
  public ChildAttributes getChildAttributes(int newChildIndex) {
    Indent childIndent = Indent.getNoneIndent();
    IElementType type = myNode.getElementType();
    ASTNode sibling = FormatterUtil.getNextNonWhitespaceSibling(myNode);
    if (BLOCKS_TOKEN_SET.contains(type) || type == ERL_IF_EXPRESSION) {
      childIndent = Indent.getNormalIndent(true);
    }
    return new ChildAttributes(childIndent, null);
  }

  @Override
  public boolean isIncomplete() {
    return false;
  }

  @Override
  public boolean isLeaf() {
    return myNode.getFirstChildNode() == null;
  }

  @Override
  public BlockWithParent getParent() {
    return myParent;
  }

  @Override
  public void setParent(BlockWithParent blockWithParent) {
    myParent = blockWithParent;
  }
}
