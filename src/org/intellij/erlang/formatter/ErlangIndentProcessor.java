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

import com.intellij.formatting.Indent;
import com.intellij.lang.ASTNode;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangPrefixExpression;
import org.jetbrains.annotations.Nullable;

import java.util.Set;

import static org.intellij.erlang.ErlangParserDefinition.COMMENTS;
import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangIndentProcessor {
  public static final Set<IElementType> BIN_OPERATORS = ContainerUtil.set(
    ERL_OP_PLUS, ERL_OP_MINUS, ERL_OP_AR_MUL, ERL_OP_AR_DIV, ERL_REM,
    ERL_OR, ERL_XOR, ERL_BOR, ERL_BXOR, ERL_BSL, ERL_BSR, ERL_AND,
    ERL_BAND, ERL_OP_EQ_EQ, ERL_OP_DIV_EQ, ERL_OP_EQ_COL_EQ, ERL_OP_EQ_DIV_EQ,
    ERL_OP_LT, ERL_OP_EQ_LT, ERL_OP_GT, ERL_OP_GT_EQ, ERL_OP_LT_EQ, ERL_OP_PLUS_PLUS,
    ERL_OP_MINUS_MINUS, ERL_OP_EQ, ERL_OP_EXL, ERL_OP_LT_MINUS, ERL_ANDALSO, ERL_ORELSE
  );
  private final CommonCodeStyleSettings settings;

  public ErlangIndentProcessor(CommonCodeStyleSettings settings) {
    this.settings = settings;
  }

  public Indent getChildIndent(ASTNode node) {
    IElementType elementType = node.getElementType();
    ASTNode parent = node.getTreeParent();
    IElementType parentType = parent != null ? parent.getElementType() : null;
    ASTNode prevSibling = FormatterUtil.getPreviousNonWhitespaceSibling(node);
    IElementType prevSiblingElementType = prevSibling != null ? prevSibling.getElementType() : null;

    if (parent == null || parent.getTreeParent() == null) {
      return Indent.getNoneIndent();
    }
    if (COMMENTS.contains(elementType) && settings.KEEP_FIRST_COLUMN_COMMENT) {
      return Indent.getAbsoluteNoneIndent();
    }
//    if (node instanceof PsiErrorElement) {
//      return Indent.getContinuationIndent();
//    }
    if (parentType == ERL_PARENTHESIZED_EXPRESSION || parentType == ERL_ARGUMENT_LIST
      || parentType == ERL_ARGUMENT_DEFINITION_LIST || parentType == ERL_FUN_TYPE || parentType == ERL_FUN_TYPE_ARGUMENTS) {
      if (elementType == ERL_PAR_LEFT || elementType == ERL_PAR_RIGHT) {
        return Indent.getNoneIndent();
      }
      return Indent.getContinuationIndent();
    }
    if (parentType == ERL_TUPLE_EXPRESSION || parentType == ERL_RECORD_TUPLE || parentType == ERL_TYPED_RECORD_FIELDS) {
      if (elementType == ERL_CURLY_LEFT || elementType == ERL_CURLY_RIGHT) {
        return Indent.getNoneIndent();
      }
      return Indent.getNormalIndent();
    }
    if (parentType == ERL_LIST_EXPRESSION || parentType == ERL_LIST_COMPREHENSION || parentType == ERL_EXPORT_FUNCTIONS || parentType == ERL_EXPORT_TYPES) {
      if (elementType == ERL_BRACKET_LEFT || elementType == ERL_BRACKET_RIGHT || elementType == ERL_BIN_START || elementType == ERL_BIN_END || elementType == ERL_LC_EXPRS) {
        return Indent.getNoneIndent();
      }
      return Indent.getNormalIndent();
    }
    if (parentType == ERL_GUARD || (parentType == ERL_CLAUSE_GUARD && elementType == ERL_WHEN)) {
      return Indent.getNormalIndent();
    }
    if (parentType == ERL_LC_EXPRS) {
      return Indent.getNormalIndent();
    }
    if (needIndent(parentType)) {
      return Indent.getNormalIndent();
    }
    if (parent.getPsi() instanceof ErlangExpression && (BIN_OPERATORS.contains(elementType) || BIN_OPERATORS.contains(prevSiblingElementType))) {
      return Indent.getNormalIndent();
    }
    return Indent.getNoneIndent();
  }

  private static boolean needIndent(@Nullable IElementType type) {
    if (type == null) {
      return false;
    }
    return ErlangBlock.BLOCKS_TOKEN_SET.contains(type);
  }
}
