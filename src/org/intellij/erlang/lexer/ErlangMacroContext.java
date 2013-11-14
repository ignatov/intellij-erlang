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


/**
 * @author savenko
 */
class ErlangMacroContext {
  private final MultiMap<String, ErlangMacro> myDefinedMacros = new MultiMap<String, ErlangMacro>();
  private final ErlangMacroContext myParent;

  public ErlangMacroContext() {
    this(null);
  }

  public ErlangMacroContext(@Nullable ErlangMacroContext parent) {
    myParent = parent;
  }

  public void defineMacro(@NotNull ErlangMacro m) {
    //TODO handle redefinition
    myDefinedMacros.putValue(m.getName(), m);
  }

  @Nullable
  public ErlangMacro getParameterlessMacro(String macroName) {
    return getParameterizedMacro(macroName, -1);
  }

  @Nullable
  public ErlangMacro getParameterizedMacro(String macroName, int arity) {
    ErlangMacro foundMacro = null;
    for (ErlangMacro m : myDefinedMacros.get(macroName)) {
      if (arity == m.getArity()) {
        foundMacro = m;
        break;
      }
    }
    if (foundMacro == null && myParent != null) {
      foundMacro = myParent.getParameterizedMacro(macroName, arity);
    }
    return foundMacro;
  }
}
