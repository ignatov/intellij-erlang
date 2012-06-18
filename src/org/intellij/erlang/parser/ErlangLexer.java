package org.intellij.erlang.parser;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.LookAheadLexer;
import com.intellij.lexer.MergingLexerAdapter;

import static org.intellij.erlang.ErlangParserDefinition.COMMENTS;

/**
 * @author ignatov
 */
public class ErlangLexer extends LookAheadLexer {
  public ErlangLexer() {
    super(new MergingLexerAdapter(new FlexAdapter(new _ErlangLexer()), COMMENTS));
  }
}
