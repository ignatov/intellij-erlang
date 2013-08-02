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
    myParseTransforms = new ArrayList<String>();
  }

  public ErlangModuleExtensionProperties(ErlangModuleExtensionProperties props) {
    myIncludePaths = new ArrayList<String>(props.myIncludePaths.size());
    myIncludePaths.addAll(props.myIncludePaths);
    myParseTransforms = new ArrayList<String>(props.myParseTransforms.size());
    myParseTransforms.addAll(props.myParseTransforms);
  }

  @Tag("includePaths")
  @AbstractCollection(surroundWithTag = false, elementTag = "path")
  public List<String> myIncludePaths;

  @Tag("parseTransforms")
  @AbstractCollection(surroundWithTag = false, elementTag = "transform")
  public List<String> myParseTransforms;
}
