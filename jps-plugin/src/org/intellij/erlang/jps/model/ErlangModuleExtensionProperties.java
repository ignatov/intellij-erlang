package org.intellij.erlang.jps.model;

import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangModuleExtensionProperties {
  @Tag("includePaths")
  @AbstractCollection(surroundWithTag = false, elementTag = "path")
  public List<String> myIncludePaths = ContainerUtil.newArrayList();

  @Tag("parseTransforms")
  @AbstractCollection(surroundWithTag = false, elementTag = "transform")
  public List<String> myParseTransforms = ContainerUtil.newArrayList();

  public ErlangModuleExtensionProperties() {
  }

  public ErlangModuleExtensionProperties(@NotNull ErlangModuleExtensionProperties props) {
    myIncludePaths = ContainerUtil.newArrayList(props.myIncludePaths);
    myParseTransforms = ContainerUtil.newArrayList(props.myParseTransforms);
  }
}
