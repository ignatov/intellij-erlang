/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.jps.builder;

import com.intellij.openapi.util.Condition;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.graph.GraphGenerator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class ErlangDependencySolver {


  private final List<ErlangModuleDescriptor> myModuleDescriptors;
  private final List<String> mySortedDirtyModules;


  ErlangDependencySolver(@NotNull List<ErlangModuleDescriptor> sortedModuleDescriptors,
                         @NotNull List<String> dirtyModules) {
    myModuleDescriptors = sortedModuleDescriptors;
    mySortedDirtyModules = findSortedModules(dirtyModules);
  }

  @NotNull
  public List<String> getSortedDirtyModules() {
    return mySortedDirtyModules;
  }

  @NotNull
  private List<String> findSortedModules(@NotNull List<String> dirtyModules) {
    final SortedModuleDependencyGraph semiGraph = new SortedModuleDependencyGraph();
    final GraphGenerator<ErlangModuleDescriptor> myGraph = GraphGenerator.create(semiGraph);
    for (String moduleName : dirtyModules) {
      ErlangModuleDescriptor module = semiGraph.getDescriptor(moduleName);
      if (module != null) {
        markDirtyModules(module, myGraph);
      }
    }

    List<ErlangModuleDescriptor> result = ContainerUtil.filter(myModuleDescriptors, new Condition<ErlangModuleDescriptor>() {
      @Override
      public boolean value(ErlangModuleDescriptor descriptor) {
        return descriptor.isDirty;
      }
    });

    return ContainerUtil.mapNotNull(result, new Function<ErlangModuleDescriptor, String>() {
      @Override
      public String fun(ErlangModuleDescriptor descriptor) {
        return descriptor.erlangModuleName;
      }
    });

  }

  private static void markDirtyModules(ErlangModuleDescriptor descriptor,
                                       GraphGenerator<ErlangModuleDescriptor> myGraph) {
    descriptor.isDirty = true;
    Iterator<ErlangModuleDescriptor> childIterator = myGraph.getOut(descriptor);
    while (childIterator.hasNext()) {
      markDirtyModules(childIterator.next(), myGraph);
    }
  }

  private class SortedModuleDependencyGraph implements GraphGenerator.SemiGraph<ErlangModuleDescriptor> {

    private final Map<String, ErlangModuleDescriptor> myModuleIndexMap = new LinkedHashMap<String, ErlangModuleDescriptor>();

    private SortedModuleDependencyGraph() {
      buildMap();
    }

    private void buildMap() {
      for (ErlangModuleDescriptor descriptor : myModuleDescriptors) {
        myModuleIndexMap.put(descriptor.erlangModuleName, descriptor);
      }
    }

    @Override
    public Collection<ErlangModuleDescriptor> getNodes() {
      return myModuleDescriptors;
    }

    @Override
    public Iterator<ErlangModuleDescriptor> getIn(final ErlangModuleDescriptor erlangModuleDescriptor) {
      return new Iterator<ErlangModuleDescriptor>() {

        final Iterator<String> iterator = erlangModuleDescriptor.dependencies.iterator();

        @Override
        public boolean hasNext() {
          return iterator.hasNext();
        }

        @Override
        public ErlangModuleDescriptor next() {
          String nextModuleName = iterator.next();
          return myModuleIndexMap.get(nextModuleName);
        }

        @Override
        public void remove() {
          iterator.remove();
        }
      };
    }

    @Nullable
    public ErlangModuleDescriptor getDescriptor(@NotNull String moduleName) {
      return myModuleIndexMap.get(moduleName);
    }
  }
}
