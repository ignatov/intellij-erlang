package org.intellij.erlang.jps.model;

import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangModuleExtensionProperties {
  @Tag("parseTransforms")
  @AbstractCollection(surroundWithTag = false, elementTag = "transform")
  //should not contain duplicate elements
  public List<String> myParseTransforms = ContainerUtil.newArrayList();

  public ErlangModuleExtensionProperties() {
  }

  public ErlangModuleExtensionProperties(@NotNull ErlangModuleExtensionProperties props) {
    myParseTransforms = ContainerUtil.newArrayList(props.myParseTransforms);
  }
}
