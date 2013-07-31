package org.intellij.erlang.facet;

import com.intellij.facet.Facet;
import com.intellij.facet.FacetManager;
import com.intellij.facet.FacetType;
import com.intellij.facet.ModifiableFacetModel;
import com.intellij.openapi.module.Module;
import org.intellij.erlang.jps.model.ErlangFacetConstants;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author savenko
 */
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
    ErlangFacet facet = facetManager.createFacet(FacetType.findInstance(ErlangFacetType.class), ErlangFacetConstants.NAME, null);
    facet.getConfiguration().addIncludeDirectories(module);
    ModifiableFacetModel facetModel = facetManager.createModifiableModel();
    facetModel.addFacet(facet);
    facetModel.commit();
  }
}
