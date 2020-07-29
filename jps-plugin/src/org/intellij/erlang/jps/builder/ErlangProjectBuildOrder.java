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

package org.intellij.erlang.jps.builder;

import com.intellij.util.xmlb.annotations.Tag;
import com.intellij.util.xmlb.annotations.XCollection;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

@Tag("dependenciesTree")
public class ErlangProjectBuildOrder {
  @Tag("files")
  @XCollection(elementName = "file")
  public List<ErlangFileDescriptor> myErlangFiles = new ArrayList<>();

  @SuppressWarnings("unused") // reflection
  public ErlangProjectBuildOrder() {
  }

  public ErlangProjectBuildOrder(@NotNull List<ErlangFileDescriptor> topologicallySortedErlangFilesDescriptors) {
    myErlangFiles = topologicallySortedErlangFilesDescriptors;
  }
}
