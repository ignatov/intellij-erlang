package org.intellij.erlang.jps.model;

import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;

import java.util.ArrayList;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangModuleExtensionProperties {
  public ErlangModuleExtensionProperties() {
    myIncludePaths = new ArrayList<String>();
  }

  public ErlangModuleExtensionProperties(ErlangModuleExtensionProperties props) {
    myIncludePaths = new ArrayList<String>(props.myIncludePaths.size());
    myIncludePaths.addAll(props.myIncludePaths);
  }

  @Tag("includePaths")
  @AbstractCollection(surroundWithTag = false, elementTag = "path")
  public List<String> myIncludePaths;
}
