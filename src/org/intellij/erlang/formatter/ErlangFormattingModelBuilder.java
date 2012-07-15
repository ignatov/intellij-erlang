package org.intellij.erlang.formatter;

import com.intellij.formatting.*;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import org.jetbrains.annotations.NotNull;

import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangFormattingModelBuilder implements FormattingModelBuilder {
  @NotNull
  @Override
  public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
    final ErlangBlock block = new ErlangBlock(element.getNode(), null, Indent.getNoneIndent(), null, settings,
      createSpacingBuilder(settings));
    return FormattingModelProvider.createFormattingModelForPsiFile(element.getContainingFile(), block, settings);
  }

  private static SpacingBuilder createSpacingBuilder(CodeStyleSettings settings) {
    return new SpacingBuilder(settings)
      .before(ERL_COMMA)        .spaceIf(false) //.spaceIf(settings.SPACE_BEFORE_COMMA)
      .after(ERL_COMMA)         .spaceIf(true) //.spaceIf(settings.SPACE_AFTER_COMMA)
      .around(ERL_OP_EQ)        .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_ASSIGNMENT_OPERATORS)
      .around(ERL_OP_PLUS)      .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_ADDITIVE_OPERATORS)
//      .around(ERL_OP_MINUS)     .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_ADDITIVE_OPERATORS)
//      .around(ERL_OP_AR_DIV)    .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_MULTIPLICATIVE_OPERATORS)
      .around(ERL_OP_AR_MUL)    .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_MULTIPLICATIVE_OPERATORS)
      .around(ERL_OP_EQ_EQ)     .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)
      .around(ERL_OP_EQ_DIV_EQ) .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)
      .around(ERL_OP_EQ_COL_EQ) .spaceIf(true) //.spaceIf(settings.SPACE_AROUND_EQUALITY_OPERATORS)
      .around(ERL_OR_OR).spaceIf(true)
      ;
  }

  @Override
  public TextRange getRangeAffectingIndent(PsiFile psiFile, int i, ASTNode astNode) {
    return null;
  }
}
