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

package org.intellij.erlang.lexer;

import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.ListIterator;

final class ErlangMacroBuilder {
  private String myName = null;
  private List<String> myParameters = null;
  private List<BodyToken> myTokens = ContainerUtil.newArrayList();

  public void setName(String name) {
    myName = name;
  }

  public void setHasParameters(boolean hasParameters) {
    if (hasParameters && myParameters == null) {
      myParameters = ContainerUtil.emptyList();
    }
  }

  public void addParameter(String paramName) {
    if (myParameters == null || myParameters.isEmpty()) {
      myParameters = ContainerUtil.newArrayList();
    }
    myParameters.add(paramName);
  }

  public void addBodyToken(IElementType tokenType, String tokenText) {
    myTokens.add(new BodyToken(tokenType, tokenText));
  }

  public void dropBodyTokens(int tokensToDrop) {
    for (int i = 0; i < tokensToDrop; i++) {
      myTokens.remove(myTokens.size() - 1);
    }
  }

  @Nullable
  public ErlangMacro build() {
    if (myName == null) return null;
    //TODO check if parameter names are unique
    if (myTokens.isEmpty()) return new ErlangMacro(myName, getParametersCount(), ContainerUtil.<ErlangMacro.MacroPart>emptyList());
    List<ErlangMacro.MacroPart> parts = ContainerUtil.newArrayList();
    for (ListIterator<BodyToken> i = myTokens.listIterator(); i.hasNext();) {
      parts.add(buildPart(i));
    }
    return new ErlangMacro(myName, getParametersCount(), parts);
  }

  private int getParametersCount() {
    return myParameters == null ? -1 : myParameters.size();
  }

  private ErlangMacro.MacroPart buildPart(ListIterator<BodyToken> tokensIterator) {
    StringBuilder text = new StringBuilder();
    while (tokensIterator.hasNext() && null == buildArgumentReferenceMacroPart(tokensIterator, false)) {
      text.append(tokensIterator.next().getText());
    }
    if (text.length() == 0 && tokensIterator.hasNext()) {
      ErlangMacro.ArgumentReferenceMacroPart argumentReferenceMacroPart = buildArgumentReferenceMacroPart(tokensIterator, true);
      assert argumentReferenceMacroPart != null;
      return argumentReferenceMacroPart;
    }
    return new ErlangMacro.TextMacroPart(text.toString());
  }

  @Nullable
  private ErlangMacro.ArgumentReferenceMacroPart buildArgumentReferenceMacroPart(ListIterator<BodyToken> tokensIterator, boolean modifyTokensIterator) {
    //TODO support quotes in macro names
    ErlangMacro.ArgumentReferenceMacroPart macroPart = null;
    BodyToken token = tokensIterator.next();
    int tokensToRewind = 1;
    // MACRO_ARG
    if (isArgumentReferenceToken(token)) {
      macroPart = new ErlangMacro.ArgumentReferenceMacroPart(myParameters.indexOf(token.getText()));
    }
    // ??MACRO_ARG
    else if (token.getType() == ErlangTypes.ERL_QMARK && tokensIterator.hasNext()) {
      BodyToken qmark = tokensIterator.next();
      tokensToRewind++;
      if (qmark.getType() == ErlangTypes.ERL_QMARK && tokensIterator.hasNext()) {
        BodyToken macroArgumentRef = tokensIterator.next();
        tokensToRewind++;
        if (isArgumentReferenceToken(macroArgumentRef)) {
          macroPart = new ErlangMacro.StringifyArgumentReferenceMacroPart(myParameters.indexOf(macroArgumentRef.getText()));
        }
      }
    }
    if (!modifyTokensIterator) {
      for ( ; tokensToRewind > 0; tokensToRewind--) {
        tokensIterator.previous();
      }
    }
    return macroPart;
  }

  private boolean isArgumentReferenceToken(BodyToken token) {
    return token.getType() == ErlangTypes.ERL_VAR && myParameters != null && myParameters.contains(token.getText());
  }

  private static final class BodyToken {
    private final IElementType myType;
    private final String myText;

    private BodyToken(IElementType type, String text) {
      myType = type;
      myText = text;
    }

    public IElementType getType() {
      return myType;
    }

    public String getText() {
      return myText;
    }
  }
}
