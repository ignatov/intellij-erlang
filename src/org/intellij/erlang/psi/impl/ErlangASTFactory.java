package org.intellij.erlang.psi.impl;

import com.intellij.lang.DefaultASTFactoryImpl;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.PsiCommentImpl;
import com.intellij.psi.tree.IElementType;

public class ErlangASTFactory extends DefaultASTFactoryImpl {
  @Override
  public LeafElement createComment(IElementType type, CharSequence text) {
    return new ErlangCommentImpl(type, text);
  }

  public static class ErlangCommentImpl extends PsiCommentImpl {
    public ErlangCommentImpl(IElementType type, CharSequence text) {
      super(type, text);
    }
  }
}
