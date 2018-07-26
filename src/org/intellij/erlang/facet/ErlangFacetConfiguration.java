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

import com.intellij.facet.FacetConfiguration;
import com.intellij.facet.ui.FacetEditorContext;
import com.intellij.facet.ui.FacetEditorTab;
import com.intellij.facet.ui.FacetValidatorsManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ui.ErlangFacetEditor;
import org.intellij.erlang.jps.model.ErlangModuleExtensionProperties;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class ErlangFacetConfiguration implements FacetConfiguration, PersistentStateComponent<ErlangModuleExtensionProperties> {
  private ErlangModuleExtensionProperties myState = new ErlangModuleExtensionProperties();

  @Override
  public FacetEditorTab[] createEditorTabs(FacetEditorContext editorContext, FacetValidatorsManager validatorsManager) {
    return new FacetEditorTab[] {new ErlangFacetEditor(editorContext, this)};
  }

  @Nullable
  @Override
  public ErlangModuleExtensionProperties getState() {
    return myState;
  }

  @Override
  public void loadState(@NotNull ErlangModuleExtensionProperties state) {
    myState = state;
  }

  public List<String> getParseTransforms() {
    return Collections.unmodifiableList(myState.myParseTransforms);
  }

  public void setParseTransformsFrom(Iterable<String> parseTransforms) {
    myState.myParseTransforms = ContainerUtil.newArrayList(ContainerUtil.newLinkedHashSet(parseTransforms));
  }
  
  public void addParseTransforms(Collection<String> newParseTransforms) {
    if (newParseTransforms.isEmpty()) return;
    //noinspection unchecked
    setParseTransformsFrom(ContainerUtil.concat(myState.myParseTransforms, newParseTransforms));
  }
}
