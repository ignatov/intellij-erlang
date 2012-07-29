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
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.TokenType;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.intellij.erlang.ErlangTypes;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangBlock implements ASTBlock {
  public static final TokenSet BLOCKS_TOKEN_SET = TokenSet.create(
    ErlangTypes.ERL_CLAUSE_BODY,
    ErlangTypes.ERL_TYPED_RECORD_FIELDS,
    ErlangTypes.ERL_ARGUMENT_LIST,
    ErlangTypes.ERL_TUPLE_EXPRESSION,
    ErlangTypes.ERL_LIST_EXPRESSION,
    ErlangTypes.ERL_CR_CLAUSES,
    ErlangTypes.ERL_IF_CLAUSES,
    ErlangTypes.ERL_TRY_CLAUSES,
    ErlangTypes.ERL_CATCH_EXPRESSION,
    ErlangTypes.ERL_CLAUSE_GUARD,
    ErlangTypes.ERL_BEGIN_END_BODY
  );

  public static final TokenSet BRACES_TOKEN_SET = TokenSet.create(
    ErlangTypes.ERL_CURLY_LEFT,
    ErlangTypes.ERL_CURLY_RIGHT,
    ErlangTypes.ERL_BRACKET_LEFT,
    ErlangTypes.ERL_BRACKET_RIGHT,
    ErlangTypes.ERL_PAR_LEFT,
    ErlangTypes.ERL_PAR_RIGHT
  );

  private ASTNode myNode;
  private Alignment myAlignment;
  private Indent myIndent;
  private Wrap myWrap;
  private CodeStyleSettings mySettings;
  private final SpacingBuilder mySpacingBuilder;
  private List<Block> mySubBlocks;

  public ErlangBlock(ASTNode node, Alignment alignment, Indent indent, Wrap wrap, CodeStyleSettings settings,
                     SpacingBuilder spacingBuilder) {
    myNode = node;
    myAlignment = alignment;
    myIndent = indent;
    myWrap = wrap;
    mySettings = settings;
    mySpacingBuilder = spacingBuilder;
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
    for (ASTNode child = myNode.getFirstChildNode(); child != null; child = child.getTreeNext()) {

      IElementType childType = child.getElementType();

      if (child.getTextRange().getLength() == 0) continue;

      if (childType == TokenType.WHITE_SPACE) {
        continue;
      }

      blocks.add(buildSubBlock(child));
    }
    return Collections.unmodifiableList(blocks);
  }

  private Block buildSubBlock(ASTNode child) {
    Wrap wrap = null;
    Indent childIndent = Indent.getNoneIndent();
    Alignment childAlignment = null;

    if (BLOCKS_TOKEN_SET.contains(myNode.getElementType())) {
      childIndent = indentIfNotBrace(child);
    }

    return new ErlangBlock(child, childAlignment, childIndent, wrap, mySettings, mySpacingBuilder);
  }

  private static Indent indentIfNotBrace(ASTNode child) {
    return BRACES_TOKEN_SET.contains(child.getElementType()) ? Indent.getNoneIndent() : Indent.getNormalIndent();
  }

  @Override
  public Spacing getSpacing(Block child1, Block child2) {
    return mySpacingBuilder.getSpacing(this, child1, child2);
  }

  @NotNull
  @Override
  public ChildAttributes getChildAttributes(int newChildIndex) {
    Indent childIndent = Indent.getNoneIndent();
    if (BLOCKS_TOKEN_SET.contains(myNode.getElementType())) {
      childIndent = Indent.getNormalIndent();
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
}
