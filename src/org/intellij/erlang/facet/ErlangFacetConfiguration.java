package org.intellij.erlang.facet;

import com.intellij.facet.FacetConfiguration;
import com.intellij.facet.ui.FacetEditorContext;
import com.intellij.facet.ui.FacetEditorTab;
import com.intellij.facet.ui.FacetValidatorsManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.WriteExternalException;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.facet.ui.ErlangFacetEditor;
import org.intellij.erlang.jps.model.ErlangModuleExtensionProperties;
import org.jdom.Element;
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

  @Override
  public void readExternal(Element element) throws InvalidDataException {
  }

  @Override
  public void writeExternal(Element element) throws WriteExternalException {
  }

  @Nullable
  @Override
  public ErlangModuleExtensionProperties getState() {
    return myState;
  }

  @Override
  public void loadState(ErlangModuleExtensionProperties state) {
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
