package org.intellij.erlang.facet;

import com.intellij.facet.Facet;
import com.intellij.facet.FacetType;
import com.intellij.facet.FacetTypeId;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import org.intellij.erlang.ErlangModuleType;
import org.intellij.erlang.jps.model.ErlangFacetConstants;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFacetType extends FacetType<ErlangFacet, ErlangFacetConfiguration> {
  public static final FacetTypeId<ErlangFacet> TYPE_ID = new FacetTypeId<ErlangFacet>(ErlangFacetConstants.ID);

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
