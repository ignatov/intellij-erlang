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

package org.intellij.erlang.context;

import com.intellij.openapi.project.Project;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.annotations.*;
import org.jetbrains.annotations.Nullable;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

@Tag("context")
public class ErlangCompileContext implements Serializable {
  @Attribute("name")
  public String name;

  @Tag("definitions")
  @MapAnnotation(
    surroundWithTag = false,
    keyAttributeName = "name",
    valueAttributeName = "definition",
    entryTagName = "macro",
    surroundKeyWithTag = false,
    surroundValueWithTag = false
  )
  public Map<String, String> macroDefinitions;

  @Tag("includePaths")
  @AbstractCollection(
    surroundWithTag = false,
    elementTag = "includePath",
    elementValueAttribute = "path",
    elementTypes = String.class
  )
  public List<String> includePaths;
  //TODO add code path specification here (-pa and -pz compiler options)
  //TODO add other compiler options

  @Transient
  public transient Project project;

  // serialization
  @SuppressWarnings("UnusedDeclaration")
  public ErlangCompileContext() {
    this(null, "", ContainerUtil.<String, String>newHashMap(), ContainerUtil.<String>newArrayList());
  }

  public ErlangCompileContext(String name) {
    this(null, name, ContainerUtil.<String, String>newHashMap(), ContainerUtil.<String>newArrayList());
  }

  public ErlangCompileContext(@Nullable Project project, String name, Map<String, String> macroDefinitions, List<String> includePaths) {
    this.project = project;
    this.name = name;
    this.macroDefinitions = macroDefinitions;
    this.includePaths = includePaths;
  }
}
