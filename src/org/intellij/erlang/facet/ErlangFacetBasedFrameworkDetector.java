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

package org.intellij.erlang.facet;

import com.intellij.facet.FacetType;
import com.intellij.framework.detection.FacetBasedFrameworkDetector;
import com.intellij.framework.detection.FileContentPattern;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.patterns.ElementPattern;
import com.intellij.util.indexing.FileContent;
import org.intellij.erlang.ErlangFileType;
import org.jetbrains.annotations.NotNull;

public class ErlangFacetBasedFrameworkDetector extends FacetBasedFrameworkDetector<ErlangFacet, ErlangFacetConfiguration> {
  private static final String ID = "Erlang";

  public ErlangFacetBasedFrameworkDetector() {
    super(ID);
  }

  @Override
  public FacetType<ErlangFacet, ErlangFacetConfiguration> getFacetType() {
    return FacetType.findInstance(ErlangFacetType.class);
  }

  @NotNull
  @Override
  public FileType getFileType() {
    return ErlangFileType.MODULE;
  }

  @NotNull
  @Override
  public ElementPattern<FileContent> createSuitableFilePattern() {
    return FileContentPattern.fileContent();
  }
}
