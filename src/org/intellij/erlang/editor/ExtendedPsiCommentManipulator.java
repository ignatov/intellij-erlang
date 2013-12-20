package org.intellij.erlang.editor;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.impl.source.resolve.reference.impl.manipulators.PsiCommentManipulator;

public class ExtendedPsiCommentManipulator extends PsiCommentManipulator {
  @Override
  public TextRange getRangeInElement(PsiComment element) {
    final String text = element.getText();
    if (text.startsWith("%%%")) return new TextRange(3, element.getTextLength());
    if (text.startsWith("%%")) return new TextRange(2, element.getTextLength());
    if (text.startsWith("%")) return new TextRange(1, element.getTextLength());
    return super.getRangeInElement(element);
  }
}
