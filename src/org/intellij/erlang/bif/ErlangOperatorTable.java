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
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public class ErlangOperatorTable {
  //Source: http://erlang.org/doc/reference_manual/expressions.html
  private static final Set<Operator> ERLANG_OPERATORS = ContainerUtil.immutableSet(
    new Operator("=", 2), new Operator("!", 2), new Operator("==", 2),
    new Operator("/=", 2), new Operator("=<", 2), new Operator("<", 2),
    new Operator(">=", 2), new Operator(">", 2), new Operator("=:=", 2),
    new Operator("=/=", 2), new Operator("+", 1), new Operator("-", 1),
    new Operator("+", 2), new Operator("-", 2), new Operator("*", 2),
    new Operator("/", 2), new Operator("bnot", 2), new Operator("div", 2),
    new Operator("rem", 2), new Operator("band", 2), new Operator("bor", 2),
    new Operator("bxor", 2), new Operator("bsl", 2), new Operator("bsr", 2),
    new Operator("not", 1), new Operator("and", 2), new Operator("or", 2),
    new Operator("xor", 2), new Operator("++", 2), new Operator("--", 2),
    new Operator("andalso", 2), new Operator("orelse", 2)
    );

  public static boolean isOperator(@NotNull String moduleName, @NotNull String name, int arity) {
    if (moduleName.equals("erlang")) {
      for (Operator operator : ERLANG_OPERATORS) {
        if (operator.getName().equals(name) && operator.getArity() == arity) {
          return true;
        }
      }
    }
    return false;
  }

  private static class Operator {
    private final String name;
    private final int arity;

    public Operator(@NotNull String name, int arity) {
      this.name = name;
      this.arity = arity;
    }

    public String getName() {
      return name;
    }

    public int getArity() {
      return arity;
    }
  }

  private ErlangOperatorTable() {
  }
}
