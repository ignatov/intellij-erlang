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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public final class ErlangMacro {
  private final String myName;
  private final int myParametersCount;
  private final List<MacroPart> myParts;

  ErlangMacro(String name, int parametersCount, List<MacroPart> parts) {
    myName = name;
    myParametersCount = parametersCount;
    myParts = parts;
  }

  /**
   * @return arity of this macro or -1 if this macro was defined without parentheses
   */
  public int getArity() {
    return myParametersCount;
  }

  public boolean requiresArguments() {
    return myParametersCount != -1;
  }

  public String getName() {
    return myName;
  }

  @NotNull
  public String substitute(@Nullable List<String> arguments) {
    if (arguments == null && requiresArguments() ||
      arguments != null && !requiresArguments()) {
      throw new IllegalArgumentException("Macro arguments count mismatch");
    }
    StringBuilder substitution = new StringBuilder();
    for (MacroPart part : myParts) {
      substitution.append(part.getText(arguments));
    }
    return substitution.toString();
  }

  interface MacroPart {
    String getText(@Nullable List<String> macroArguments);
  }

  static class TextMacroPart implements MacroPart {
    private final String myText;

    TextMacroPart(String text) {
      myText = text;
    }

    @Override
    public String getText(@Nullable List<String> macroArguments) {
      return myText;
    }
  }

  static class ArgumentReferenceMacroPart implements MacroPart {
    private final int myArgumentIndex;

    ArgumentReferenceMacroPart(int argumentIndex) {
      assert argumentIndex >= 0;
      myArgumentIndex = argumentIndex;
    }

    @Override
    public String getText(@Nullable List<String> macroArguments) {
      assert macroArguments != null;
      assert myArgumentIndex < macroArguments.size();
      return macroArguments.get(myArgumentIndex);
    }
  }

  static class StringifyArgumentReferenceMacroPart extends ArgumentReferenceMacroPart {
    StringifyArgumentReferenceMacroPart(int argumentIndex) {
      super(argumentIndex);
    }

    @Override
    public String getText(@Nullable List<String> macroArguments) {
      return '"' + super.getText(macroArguments) + '"';
    }
  }
}
