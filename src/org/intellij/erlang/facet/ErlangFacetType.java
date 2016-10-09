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
import com.intellij.facet.FacetType;
import com.intellij.facet.FacetTypeId;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import org.intellij.erlang.jps.model.ErlangFacetConstants;
import org.intellij.erlang.module.ErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFacetType extends FacetType<ErlangFacet, ErlangFacetConfiguration> {
  public static final FacetTypeId<ErlangFacet> TYPE_ID = new FacetTypeId<>(ErlangFacetConstants.ID);

  public ErlangFacetType() {
    super(TYPE_ID, ErlangFacetConstants.ID, ErlangFacetConstants.NAME);
  }

  @Override
  public ErlangFacetConfiguration createDefaultConfiguration() {
    return new ErlangFacetConfiguration();
  }

  @Override
  public ErlangFacet createFacet(@NotNull Module module, String name, @NotNull ErlangFacetConfiguration configuration, @Nullable Facet underlyingFacet) {
    return new ErlangFacet(this, module, name, configuration, underlyingFacet);
  }

  @Override
  public boolean isSuitableModuleType(ModuleType moduleType) {
    return moduleType instanceof ErlangModuleType;
  }
}
