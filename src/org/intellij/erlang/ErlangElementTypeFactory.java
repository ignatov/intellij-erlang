/*
 * Copyright 2012-2014 Sergey Ignatov
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

import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.stubs.types.*;
import org.jetbrains.annotations.NotNull;

public class ErlangElementTypeFactory {
  private ErlangElementTypeFactory() {
  }

  @NotNull
  public static IElementType factory(@NotNull String name) {
    return switch (name) {
      case "ERL_FUNCTION"          -> new ErlangFunctionStubElementType(name);
      case "ERL_BEHAVIOUR"         -> new ErlangBehaviourStubElementType(name);
      case "ERL_MODULE"            -> new ErlangModuleStubElementType(name);
      case "ERL_TYPE_DEFINITION"   -> new ErlangTypeDefinitionElementType(name);
      case "ERL_MACROS_DEFINITION" -> new ErlangMacrosDefinitionElementType(name);
      case "ERL_RECORD_DEFINITION" -> new ErlangRecordDefinitionElementType(name);
      case "ERL_INCLUDE"           -> new ErlangIncludeElementType(name);
      case "ERL_INCLUDE_LIB"       -> new ErlangIncludeLibElementType(name);
      case "ERL_CALLBACK_SPEC"     -> new ErlangCallbackStubElementType(name);
      case "ERL_CALLBACK_FUNCTION" -> new ErlangCallbackFunctionStubElementType(name);
      case "ERL_SPECIFICATION"     -> new ErlangSpecificationElementType(name);
      default -> throw new RuntimeException("Unknown element type: " + name);
    };
  }
}
