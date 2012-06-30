package org.intellij.erlang;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author ignatov
 */
public class ErlangBraceMatcher implements PairedBraceMatcher {

  private static final BracePair[] PAIRS = new BracePair[]{
    new BracePair(ErlangTypes.ERL_PAR_LEFT, ErlangTypes.ERL_PAR_RIGHT, false),
    new BracePair(ErlangTypes.ERL_CURLY_LEFT, ErlangTypes.ERL_CURLY_RIGHT, false),
    new BracePair(ErlangTypes.ERL_BRACKET_LEFT, ErlangTypes.ERL_BRACKET_RIGHT, false),
//    new BracePair(ErlangTypes.ERL_BEGIN, ErlangTypes.ERL_END, false)
  };

  @Override
  public BracePair[] getPairs() {
    return PAIRS;
  }

  @Override
  public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
    return true;
  }

  @Override
  public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
    return openingBraceOffset;
  }
}
