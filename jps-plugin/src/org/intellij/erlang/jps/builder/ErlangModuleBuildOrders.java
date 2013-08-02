package org.intellij.erlang.jps.builder;

import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;

import java.util.ArrayList;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangModuleBuildOrders {
  //reflection
  public ErlangModuleBuildOrders() {
    myModuleBuildOrderDescriptors = new ArrayList<ErlangModuleBuildOrderDescriptor>();
  }

  public ErlangModuleBuildOrders(int descriptorsCount) {
    myModuleBuildOrderDescriptors = new ArrayList<ErlangModuleBuildOrderDescriptor>(descriptorsCount);
  }

  @Tag("modules")
  @AbstractCollection(surroundWithTag = false)
  public List<ErlangModuleBuildOrderDescriptor> myModuleBuildOrderDescriptors;
}
