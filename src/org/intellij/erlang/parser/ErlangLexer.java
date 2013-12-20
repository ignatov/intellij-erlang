/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.parser;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.LookAheadLexer;
import com.intellij.lexer.MergingLexerAdapter;
import com.intellij.psi.tree.TokenSet;

import static org.intellij.erlang.ErlangParserDefinition.COMMENTS;
import static org.intellij.erlang.ErlangTypes.*;

public class ErlangLexer extends LookAheadLexer {
  public static final TokenSet KEYWORDS = TokenSet.create(
    ERL_AFTER, ERL_WHEN, ERL_BEGIN, ERL_END, ERL_OF, ERL_CASE, ERL_FUN, ERL_CATCH, ERL_IF, ERL_RECEIVE, ERL_TRY);

  public ErlangLexer() {
    super(new MergingLexerAdapter(new FlexAdapter(new _ErlangLexer()), COMMENTS));
  }
}
