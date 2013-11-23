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

package org.intellij.erlang.psi;

import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public interface ErlangFile extends PsiFile {
  @Nullable
  ErlangModule getModule();

  @NotNull
  List<ErlangRule> getRules();

  @NotNull
  List<ErlangAttribute> getAttributes();

  @Nullable
  ErlangCallbackSpec getCallbackByName(@NotNull String fullName);

  @NotNull
  List<ErlangFunction> getFunctions();

  @Nullable
  ErlangFunction getFunction(@NotNull String name, final int argsCount);

  @NotNull
  Collection<ErlangFunction> getFunctionsByName(@NotNull String name);

  @NotNull
  List<ErlangRecordDefinition> getRecords();

  @NotNull
  List<ErlangMacrosDefinition> getMacroses();

  @Nullable
  ErlangMacrosDefinition getMacros(@NotNull String name);

  @NotNull
  List<ErlangTypeDefinition> getTypes();

  @Nullable
  ErlangTypeDefinition getType(@NotNull String name);

  @Nullable
  ErlangRecordDefinition getRecord(String name);

  @NotNull
  List<ErlangInclude> getIncludes();

  @NotNull
  List<ErlangIncludeLib> getIncludeLibs();

  @NotNull
  List<ErlangBehaviour> getBehaviours();

  @NotNull
  List<ErlangSpecification> getSpecifications();

  @NotNull
  Collection<ErlangFunction> getExportedFunctions();

  boolean isExported(@NotNull String signature);

  boolean isExportedAll();

  @NotNull
  ArrayList<ErlangImportFunction> getImportedFunctions();

  @NotNull
  Map<String, ErlangCallbackSpec> getCallbackMap();

  void addDeclaredParseTransforms(@NotNull Set<String> parseTransforms);
}
