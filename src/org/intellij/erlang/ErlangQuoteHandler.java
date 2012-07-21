package org.intellij.erlang;

import com.intellij.codeInsight.editorActions.SimpleTokenSetQuoteHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.highlighter.HighlighterIterator;
import com.intellij.psi.TokenType;

/**
 * @author ignatov
 */
public class ErlangQuoteHandler extends SimpleTokenSetQuoteHandler {
  public ErlangQuoteHandler() {
    super(ErlangTypes.ERL_STRING, TokenType.BAD_CHARACTER);
  }

  @Override
  public boolean hasNonClosedLiteral(Editor editor, HighlighterIterator iterator, int offset) {
    return true;
  }
}
