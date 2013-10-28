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
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author savenko
 */
public final class ErlangMacroBuilder {
  private String myName = null;
  private List<String> myParameters = null;

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
    //TODO detect references to macro parameters and macros
  }

  public void dropBodyTokens(int tokensToDrop) {
    //TODO drop last tokensToDrop tokens
  }

  @Nullable
  public ErlangMacro build() {
    //TODO validate macro and return it if it is valid
    return null;
  }
}
