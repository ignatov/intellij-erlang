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

package org.intellij.erlang;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangBraceMatcher implements PairedBraceMatcher {
  private static final BracePair[] PAIRS = new BracePair[]{
    new BracePair(ErlangTypes.ERL_PAR_LEFT, ErlangTypes.ERL_PAR_RIGHT, false),
    new BracePair(ErlangTypes.ERL_CURLY_LEFT, ErlangTypes.ERL_CURLY_RIGHT, true),
    new BracePair(ErlangTypes.ERL_BRACKET_LEFT, ErlangTypes.ERL_BRACKET_RIGHT, false),
    new BracePair(ErlangTypes.ERL_BIN_START, ErlangTypes.ERL_BIN_END, true),
    new BracePair(ErlangTypes.ERL_IF, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_CASE, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_BEGIN, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_TRY, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_FUN, ErlangTypes.ERL_END, true),
    new BracePair(ErlangTypes.ERL_RECEIVE, ErlangTypes.ERL_END, true),
  };

  @Override
  public BracePair[] getPairs() {
    return PAIRS;
  }

  @Override
  public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType type) {
    return TokenType.WHITE_SPACE == type
      || ErlangParserDefinition.COMMENTS.contains(type)
      || type == ErlangTypes.ERL_SEMI
      || type == ErlangTypes.ERL_COMMA
      || type == ErlangTypes.ERL_PAR_RIGHT
      || type == ErlangTypes.ERL_BRACKET_RIGHT
      || type == ErlangTypes.ERL_CURLY_RIGHT
      || null == type;
  }

  @Override
  public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
    return openingBraceOffset;
  }
}
