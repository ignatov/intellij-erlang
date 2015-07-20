/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.bif;

import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public class ErlangOperatorTable {
  //Source: http://erlang.org/doc/reference_manual/expressions.html
  //Operators '=', 'andalso', 'orelse' cannot be invoked as functions because of loss of semantics.
  private static final Set<String> OPERATORS_ALLOWED_INVOKING_AS_FUNCTION = ContainerUtil.immutableSet(
    "'!'/2",
    "'=='/2",
    "'/='/2",
    "'=<'/2",
    "'<'/2",
    "'>='/2",
    "'>'/2",
    "'=:='/2",
    "'=/='/2",
    "'+'/1",
    "'-'/1",
    "'+'/2",
    "'-'/2",
    "'*'/2",
    "'/'/2",
    "bnot/2",
    "div/2",
    "rem/2",
    "band/2",
    "bor/2",
    "bxor/2",
    "bsl/2",
    "bsr/2",
    "not/1",
    "and/2",
    "or/2",
    "xor/2",
    "'++'/2",
    "'--'/2"
  );

  public static boolean canBeInvokedAsFunction(@NotNull String moduleName, @NotNull String name, int arity) {
    return canBeInvokedAsFunction(moduleName, ErlangPsiImplUtil.createFunctionPresentation(name, arity));
  }

  public static boolean canBeInvokedAsFunction(@NotNull String moduleName, @NotNull String functionPresentation) {
    return moduleName.equals("erlang") &&
           OPERATORS_ALLOWED_INVOKING_AS_FUNCTION.contains(functionPresentation);
  }

  private ErlangOperatorTable() {
  }
}
