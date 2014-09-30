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

import com.intellij.util.containers.MultiMap;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;


/**
 * @author savenko
 */
final class ErlangMacroContext {
  private final MultiMap<String, ErlangMacro> myDefinitions = new MultiMap<String, ErlangMacro>();
  private final Set<String> myUndefinitions = new HashSet<String>();

  public void defineMacro(@NotNull ErlangMacro m) {
    //TODO handle redefinition
    myDefinitions.putValue(m.getName(), m);
    myUndefinitions.remove(m.getName());
  }

  public void undefineMacro(@NotNull String macroName) {
    myDefinitions.remove(macroName);
    myUndefinitions.add(macroName);
  }

  public ErlangMacroDefinitionState getMacroDefinitionState(@NotNull String macroName) {
    return myUndefinitions.contains(macroName) ? ErlangMacroDefinitionState.UNDEFINED :
      myDefinitions.containsKey(macroName) ? ErlangMacroDefinitionState.DEFINED : ErlangMacroDefinitionState.FREE;
  }

  @Nullable
  public ErlangMacro getParameterlessMacro(@NotNull String macroName) {
    return getParameterizedMacro(macroName, -1);
  }

  @Nullable
  public ErlangMacro getParameterizedMacro(@NotNull String macroName, int arity) {
    ErlangMacro foundMacro = null;
    for (ErlangMacro m : myDefinitions.get(macroName)) {
      if (arity == m.getArity()) {
        foundMacro = m;
        break;
      }
    }
    return foundMacro;
  }
}
