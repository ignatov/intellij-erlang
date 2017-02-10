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

import com.intellij.facet.Facet;
import com.intellij.facet.FacetManager;
import com.intellij.facet.FacetType;
import com.intellij.facet.ModifiableFacetModel;
import com.intellij.openapi.module.Module;
import org.intellij.erlang.jps.model.ErlangFacetConstants;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFacet extends Facet<ErlangFacetConfiguration> {
  public ErlangFacet(@NotNull FacetType facetType, @NotNull Module module, @NotNull String name, @NotNull ErlangFacetConfiguration configuration, @Nullable Facet underlyingFacet) {
    super(facetType, module, name, configuration, underlyingFacet);
  }

  @Nullable
  public static ErlangFacet getFacet(@NotNull Module module) {
    return FacetManager.getInstance(module).getFacetByType(ErlangFacetType.TYPE_ID);
  }

  //should only be called from write action
  public static void createFacet(@NotNull Module module) {
    FacetManager facetManager = FacetManager.getInstance(module);
    ErlangFacetType ft = FacetType.findInstance(ErlangFacetType.class);
    ErlangFacet prev = facetManager.getFacetByType(ft.getId());
    if (prev != null) return;
    ErlangFacet facet = facetManager.createFacet(ft, ErlangFacetConstants.NAME, null);
    ModifiableFacetModel facetModel = facetManager.createModifiableModel();
    facetModel.addFacet(facet);
    facetModel.commit();
  }
}
