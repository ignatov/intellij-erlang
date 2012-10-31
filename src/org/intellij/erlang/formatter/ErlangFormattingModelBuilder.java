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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.tree.TokenSet;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangFormattingModelBuilder implements FormattingModelBuilder {
  @NotNull
  @Override
  public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
    CommonCodeStyleSettings commonSettings = settings.getCommonSettings(ErlangLanguage.INSTANCE);
    ErlangCodeStyleSettings erlangSettings = settings.getCustomSettings(ErlangCodeStyleSettings.class);
    final ErlangBlock block = new ErlangBlock(null, element.getNode(), null, null, commonSettings, erlangSettings,
      createSpacingBuilder(commonSettings));
    return FormattingModelProvider.createFormattingModelForPsiFile(element.getContainingFile(), block, settings);
  }

  private static SpacingBuilder createSpacingBuilder(CommonCodeStyleSettings settings) {
    TokenSet rules = TokenSet.create(ERL_RULE, ERL_RECORD_DEFINITION, ERL_INCLUDE, ERL_MACROS_DEFINITION, ERL_ATTRIBUTE);
    TokenSet keywords = TokenSet.create(
      ERL_AFTER, ERL_WHEN, ERL_BEGIN, ERL_END, ERL_OF, ERL_CASE, ERL_QUERY, ERL_CATCH, ERL_IF, ERL_RECEIVE,
      ERL_TRY, ERL_DIV, ERL_REM, ERL_OR, ERL_XOR, ERL_BOR, ERL_BXOR, ERL_BSL, ERL_BSR, ERL_AND, ERL_BAND);

    return new SpacingBuilder(settings.getRootSettings())
      .before(ERL_COMMA).spaceIf(settings.SPACE_BEFORE_COMMA)
      .after(ERL_COMMA).spaceIf(settings.SPACE_AFTER_COMMA)

//      .betweenInside(ERL_OP_EQ, ERL_BINARY_EXPRESSION, ERL_RECORD_FIELD).spaces(1)
//      .betweenInside(ERL_OP_EQ, ERL_BINARY_TYPE, ERL_RECORD_FIELD).spaces(1)
//      .betweenInside(ERL_OP_EQ, ERL_LIST_COMPREHENSION, ERL_RECORD_FIELD).spaces(1)

//      .aroundInside(ERL_OP_EQ, ERL_RECORD_FIELD).none()
//      .aroundInside(ERL_OP_EQ, ERL_TYPED_EXPR).none()
      .around(ERL_OP_EQ).spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)
      .around(ERL_OP_LT_MINUS).spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)
      .around(ERL_OP_EXL).spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)

      .after(ERL_ARROW).spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)
      .before(ERL_CLAUSE_BODY).spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)

      .around(ERL_OP_PLUS).spaceIf(settings.SPACE_AROUND_ADDITIVE_OPERATORS)
      .around(ERL_OP_PLUS_PLUS).spaceIf(settings.SPACE_AROUND_ADDITIVE_OPERATORS)
      .aroundInside(ERL_OP_MINUS, ERL_ADDITIVE_EXPRESSION).spaceIf(settings.SPACE_AROUND_ADDITIVE_OPERATORS)

      .aroundInside(ERL_OP_AR_DIV, ERL_MULTIPLICATIVE_EXPRESSION).spaceIf(settings.SPACE_AROUND_MULTIPLICATIVE_OPERATORS)
      .around(ERL_OP_AR_MUL).spaceIf(settings.SPACE_AROUND_MULTIPLICATIVE_OPERATORS)

      .around(ERL_OP_EQ_EQ).spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)
      .around(ERL_OP_DIV_EQ).spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)
      .around(ERL_OP_EQ_DIV_EQ).spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)
      .around(ERL_OP_EQ_COL_EQ).spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)

      .around(ERL_OP_LT).spaceIf(settings.SPACE_AROUND_LOGICAL_OPERATORS)
      .around(ERL_OP_EQ_LT).spaceIf(settings.SPACE_AROUND_LOGICAL_OPERATORS)
      .around(ERL_OP_GT).spaceIf(settings.SPACE_AROUND_LOGICAL_OPERATORS)
      .around(ERL_OP_GT_EQ).spaceIf(settings.SPACE_AROUND_LOGICAL_OPERATORS)
      .around(ERL_OR_OR).spaceIf(settings.SPACE_AROUND_LOGICAL_OPERATORS)
      .around(ERL_OR).spaceIf(settings.SPACE_AROUND_LOGICAL_OPERATORS)


      .after(ERL_BRACKET_LEFT).none()
      .before(ERL_BRACKET_RIGHT).none()
      .after(ERL_CURLY_LEFT).none()
      .before(ERL_CURLY_RIGHT).none()
      .after(ERL_BIN_START).none()
      .before(ERL_BIN_END).none()
      .before(ERL_ARGUMENT_DEFINITION_LIST).none()
      .before(ERL_ARGUMENT_LIST).none()
      .withinPair(ERL_PAR_LEFT, ERL_PAR_RIGHT).spaceIf(settings.SPACE_WITHIN_METHOD_CALL_PARENTHESES)
      .withinPair(ERL_BRACKET_LEFT, ERL_BRACKET_RIGHT).spaceIf(true)
      .withinPair(ERL_CURLY_LEFT, ERL_CURLY_RIGHT).spaceIf(true)
      .withinPair(ERL_BIN_START, ERL_BIN_END).spaceIf(true)

      .beforeInside(rules, ERL_PAR_LEFT).none()

      .around(keywords).spaces(1)

      .before(ERL_FUN_TYPE_SIGS_BRACES).none()
      .between(ERL_SPEC_FUN, ERL_TYPE_SIG).none()
      .aroundInside(ERL_OP_AR_DIV, ERL_SPEC_FUN).none()

      .beforeInside(ERL_PAR_LEFT, ERL_EXPORT).none()
      .beforeInside(ERL_PAR_LEFT, ERL_EXPORT_TYPE_ATTRIBUTE).none()
      .beforeInside(ERL_PAR_LEFT, ERL_INCLUDE).none()
      .beforeInside(ERL_PAR_LEFT, ERL_RECORD_DEFINITION).none()
      .beforeInside(ERL_PAR_LEFT, ERL_MODULE).none()
      .beforeInside(ERL_PAR_LEFT, ERL_MACROS_DEFINITION).none()
      .beforeInside(ERL_PAR_LEFT, ERL_BEHAVIOUR).none()
      .beforeInside(ERL_TYPED_ATTR_VAL, ERL_ATOM_ATTRIBUTE).spaces(1)
      .afterInside(ERL_Q_ATOM, ERL_ATOM_ATTRIBUTE).none()
      .beforeInside(ERL_PAR_LEFT, ERL_SPECIFICATION).none()
      .beforeInside(ERL_FUN_TYPE_SIGS, ERL_SPECIFICATION).spaces(1)
      .beforeInside(ERL_PAR_LEFT, ERL_CALLBACK_SPEC).none()
      .beforeInside(ERL_FUN_TYPE_SIGS, ERL_CALLBACK_SPEC).spaces(1)

      .aroundInside(ERL_OP_AR_DIV, ERL_FUN_TYPE_SIGS).none()
      .aroundInside(ERL_OP_AR_DIV, ERL_EXPORT_FUNCTION).none()
      .aroundInside(ERL_OP_AR_DIV, ERL_EXPORT_TYPE).none()

      .aroundInside(ERL_COLON_COLON, ERL_FUN_TYPE_SIGS).spaces(1)
      .betweenInside(ERL_COLON_COLON, ERL_TYPE_SIG, ERL_FUN_TYPE_SIGS).spaces(1)
      .between(ERL_FUN_TYPE_ARGUMENTS, ERL_TOP_TYPE_CLAUSE).spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)
      .betweenInside(ERL_Q_ATOM, ERL_PAR_LEFT, ERL_TYPE).none()


      .before(ERL_CLAUSE_GUARD).spaces(1)
      .before(ERL_FUN_TYPE_SIGS).spaces(1)
      .around(ERL_FUN_TYPE).spaces(1)
      .around(ERL_TYPE_SIG).spaces(1)
      .beforeInside(ERL_PAR_LEFT, ERL_FUN_TYPE).spaces(1)
      .afterInside(ERL_PAR_RIGHT, ERL_FUN_TYPE).spaces(1)

      .aroundInside(ERL_COLON, ERL_GLOBAL_FUNCTION_CALL_EXPRESSION).none()
      .around(ERL_COLON_COLON).spaces(1)
      .aroundInside(ERL_DOT, ERL_RECORD_FIELD).none()
      .before(ERL_RECORD_FIELD).none()
      .aroundInside(ERL_RADIX, ERL_RECORD_EXPRESSION).none()
      .before(ERL_DOT).none()
      .around(ERL_QMARK).none()
      .before(ERL_RECORD_TUPLE).none()
      .between(ERL_FUN, ERL_MODULE_REF).spaces(1)
      .between(ERL_FUN, ERL_FUNCTION_WITH_ARITY).spaces(1)
      .after(ERL_FUN).none()
      .before(ERL_END).spaces(1)
      ;
  }

  @Override
  public TextRange getRangeAffectingIndent(PsiFile psiFile, int i, ASTNode astNode) {
    return null;
  }
}
