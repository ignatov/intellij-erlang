package org.intellij.erlang.parser;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.LookAheadLexer;
import com.intellij.lexer.MergingLexerAdapter;
import com.intellij.psi.tree.TokenSet;

import static org.intellij.erlang.ErlangParserDefinition.COMMENTS;
import static org.intellij.erlang.ErlangTypes.*;

/**
 * @author ignatov
 */
public class ErlangLexer extends LookAheadLexer {
  public static final TokenSet KEYWORDS = TokenSet.create(
    ERL_AFTER, ERL_WHEN, ERL_BEGIN, ERL_END, ERL_OF, ERL_CASE, ERL_FUN, ERL_QUERY, ERL_CATCH, ERL_IF, ERL_RECEIVE, ERL_TRY);

  public ErlangLexer() {
    super(new MergingLexerAdapter(new FlexAdapter(new _ErlangLexer()), COMMENTS));
  }
}
