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

package org.intellij.erlang.compilation;

import com.intellij.openapi.util.Pair;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;

class ErlangModuleTextGenerator {
  private ErlangModuleTextGenerator() {
  }

  @NotNull
  public static ErlangModuleTextBuilder module(@NotNull String moduleName) {
    return new ErlangModuleTextBuilder(moduleName);
  }

  @NotNull
  public static ParseTransformBuilder pt(@NotNull String moduleName) {
    return new ParseTransformBuilder(moduleName);
  }

  @NotNull
  public static BehaviourBuilder behaviour(@NotNull String moduleName) {
    return new BehaviourBuilder(moduleName);
  }

  static class ErlangModuleTextBuilder {
    private final String myModuleName;
    private final List<Pair<String, Integer>> myExports = ContainerUtil.newArrayList();
    private final List<BehaviourBuilder> myBehaviours = ContainerUtil.newArrayList();
    private final List<String> myParseTransforms = ContainerUtil.newArrayList();

    public ErlangModuleTextBuilder(@NotNull String moduleName) {
      myModuleName = moduleName;
    }

    public String getModuleName() {
      return myModuleName;
    }

    public ErlangModuleTextBuilder pt(@NotNull String moduleName) {
      myParseTransforms.add(moduleName);
      return this;
    }

    public ErlangModuleTextBuilder behaviour(@NotNull BehaviourBuilder behaviour) {
      myBehaviours.add(behaviour);
      myExports.addAll(behaviour.myCallbacks);
      return this;
    }

    @NotNull
    public String build() {
      StringBuilder builder = new StringBuilder();
      appendModule(builder);
      appendBehaviour(builder);
      appendParseTransforms(builder);
      appendExports(builder);
      build(builder);
      return builder.toString();
    }

    protected void build(@NotNull StringBuilder builder) {
      appendFunctions(builder);
    }

    @NotNull
    protected static <T> StringBuilder commaSeparated(@NotNull StringBuilder sb,
                                                      @NotNull List<T> items,
                                                      @NotNull ItemFormatter<T> formatter) {
      String separator = ", ";
      for (T item : items) {
        formatter.format(sb, item).append(separator);
      }
      sb.setLength(items.isEmpty() ? sb.length() : sb.length() - separator.length());
      return sb;
    }

    private void appendFunctions(@NotNull StringBuilder builder) {
      for (Pair<String, Integer> functionEntry : myExports) {
        appendFunction(builder, functionEntry.first, functionEntry.second);
      }
    }

    private void appendExports(@NotNull StringBuilder builder) {
      if (myExports.isEmpty()) return;

      builder.append("-export([");
      commaSeparated(builder, myExports, new ItemFormatter<Pair<String, Integer>>() {
        @NotNull
        @Override
        public StringBuilder format(@NotNull StringBuilder sb, @NotNull Pair<String, Integer> export) {
          return sb.append(export.first).append("/").append(export.second);
        }
      });
      builder.append("]).\n");
    }

    private void appendModule(@NotNull StringBuilder builder) {
      builder.append("-module(").append(myModuleName).append(").\n");
    }

    private void appendBehaviour(@NotNull StringBuilder builder) {
      for (BehaviourBuilder behaviour : myBehaviours) {
        builder.append("-behaviour(").append(behaviour.getModuleName()).append(").\n");
      }
    }

    private static void appendFunction(@NotNull StringBuilder builder, @NotNull String functionName, int arity) {
      List<Integer> argumentIndices = ContainerUtil.newArrayListWithCapacity(arity);
      for (int i = 0; i < arity; i++) {
        argumentIndices.add(i);
      }

      builder.append(functionName).append("(");
      commaSeparated(builder, argumentIndices, new ItemFormatter<Integer>() {
        @NotNull
        @Override
        public StringBuilder format(@NotNull StringBuilder sb, @NotNull Integer argumentIdx) {
          return sb.append("_Arg").append(argumentIdx);
        }
      });
      builder.append(") -> ok.\n");
    }

    private void appendParseTransforms(@NotNull StringBuilder builder) {
      if (myParseTransforms.isEmpty()) return;

      builder.append("-compile([");
      commaSeparated(builder, myParseTransforms, new ItemFormatter<String>() {
        @NotNull
        @Override
        public StringBuilder format(@NotNull StringBuilder sb, @NotNull String pt) {
          return sb.append("{parse_transform, ").append(pt).append("}");
        }
      });
      builder.append("]).\n");
    }

    protected interface ItemFormatter<T> {
      @NotNull
      StringBuilder format(@NotNull StringBuilder sb, @NotNull T item);
    }
  }

  static class ParseTransformBuilder extends ErlangModuleTextBuilder {
    public ParseTransformBuilder(@NotNull String moduleName) {
      super(moduleName);
    }

    @Override
    protected void build(@NotNull StringBuilder builder) {
      createTransform(builder);
      super.build(builder);
    }

    private static void createTransform(@NotNull StringBuilder builder) {
      builder.append("-export([parse_transform/2]).\n");
      builder.append("parse_transform(Forms, _Options) -> Forms.\n");
    }
  }

  static class BehaviourBuilder extends ErlangModuleTextBuilder {
    private final List<Pair<String, Integer>> myCallbacks = ContainerUtil.newArrayList();

    public BehaviourBuilder(@NotNull String moduleName) {
      super(moduleName);
    }

    public BehaviourBuilder callback(@NotNull String name, int arity) {
      myCallbacks.add(Pair.createNonNull(name, arity));
      return this;
    }

    @Override
    protected void build(@NotNull StringBuilder builder) {
      builder.append("-export([behaviour_info/1]).\n");
      builder.append("behaviour_info(callbacks) ->[");
      commaSeparated(builder, myCallbacks, new ItemFormatter<Pair<String, Integer>>() {
        @NotNull
        @Override
        public StringBuilder format(@NotNull StringBuilder sb, @NotNull Pair<String, Integer> callback) {
          return sb.append("{").append(callback.first).append(", ").append(callback.second).append("}");
        }
      });
      builder.append("].\n");
      super.build(builder);
    }
  }
}
