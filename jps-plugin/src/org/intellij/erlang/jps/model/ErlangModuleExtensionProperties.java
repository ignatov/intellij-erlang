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

package org.intellij.erlang.jps.model;

import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class ErlangModuleExtensionProperties {
  @Tag("parseTransforms")
  @AbstractCollection(surroundWithTag = false, elementTag = "transform")
  //should not contain duplicate elements
  public List<String> myParseTransforms = new ArrayList<>();

  public ErlangModuleExtensionProperties() {
  }

  public ErlangModuleExtensionProperties(@NotNull ErlangModuleExtensionProperties props) {
    myParseTransforms = new ArrayList<>(props.myParseTransforms);
  }
}
