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

package org.intellij.erlang.build;

import com.intellij.openapi.util.Pair;
import kotlin.reflect.jvm.internal.impl.utils.SmartList;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
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
    private final List<Pair<String, Integer>> myExports = new SmartList<>();
    private final List<BehaviourBuilder> myBehaviours = new SmartList<>();
    private final List<String> myParseTransforms = new SmartList<>();
    private final List<String> myIncludes = new SmartList<>();
    private final List<String> myIncludeLibs = new SmartList<>();

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

    public ErlangModuleTextBuilder include(@NotNull String modulePath) {
      myIncludes.add(modulePath);
      return this;
    }

    public ErlangModuleTextBuilder includeLib(@NotNull String modulePath) {
      myIncludeLibs.add(modulePath);
      return this;
    }

    @NotNull
    public String build() {
      StringBuilder builder = new StringBuilder();
      appendModule(builder);
      appendIncludes(builder);
      appendIncludeLibs(builder);
      appendBehaviour(builder);
      appendParseTransforms(builder);
      appendExports(builder);
      build(builder);
      return builder.toString();
    }

    private void appendIncludeLibs(StringBuilder builder) {
      for (String includeLib : myIncludeLibs) {
        builder.append("-includeLib(\"").append(includeLib).append("\").\n");
      }
    }

    private void appendIncludes(StringBuilder builder) {
      for (String include : myIncludes) {
        builder.append("-include(\"").append(include).append("\").\n");
      }
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
      commaSeparated(builder, myExports, (sb, export) -> sb.append(export.first).append("/").append(export.second));
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
      List<Integer> argumentIndices = new ArrayList<>(arity);
      for (int i = 0; i < arity; i++) {
        argumentIndices.add(i);
      }

      builder.append(functionName).append("(");
      commaSeparated(builder, argumentIndices, (sb, argumentIdx) -> sb.append("_Arg").append(argumentIdx));
      builder.append(") -> ok.\n");
    }

    private void appendParseTransforms(@NotNull StringBuilder builder) {
      if (myParseTransforms.isEmpty()) return;

      builder.append("-compile([");
      commaSeparated(builder, myParseTransforms, (sb, pt) -> sb.append("{parse_transform, ").append(pt).append("}"));
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
    private final List<Pair<String, Integer>> myCallbacks = new SmartList<>();

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
      commaSeparated(builder, myCallbacks, (sb, callback) -> sb.append("{").append(callback.first).append(", ").append(callback.second).append("}"));
      builder.append("].\n");
      super.build(builder);
    }
  }
}
