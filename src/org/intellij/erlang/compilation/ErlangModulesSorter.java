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

package org.intellij.erlang.compilation;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.graph.DFSTBuilder;
import com.intellij.util.graph.GraphGenerator;
import org.intellij.erlang.jps.builder.ErlangModuleDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class ErlangModulesSorter {
  private final Collection<ErlangFile> myModules;
  private final List<String> myGlobalParseTransforms;
  private boolean isAcyclic = true;
  private List<ErlangModuleDescriptor> sortedModules;

  public ErlangModulesSorter(Collection<ErlangFile> modules, List<String> globalParseTransforms) {
    myModules = modules;
    myGlobalParseTransforms = globalParseTransforms;
  }

  public static ErlangModuleDescriptor getModuleDescriptor(ErlangModuleNode node) {
    ErlangModuleDescriptor result = new ErlangModuleDescriptor();
    result.erlangModuleName = getErlangFileName(node);
    result.dependencies = ContainerUtil.mapNotNull(node.getDependencies(), new Function<ErlangModuleNode, String>() {
      @Nullable
      @Override
      public String fun(ErlangModuleNode node) {
        return getErlangFileName(node);
      }
    });

    return result;
  }

  @Nullable
  private static String getErlangFileName(ErlangModuleNode node) {
    VirtualFile virtualFile = node.getModuleFile().getVirtualFile();
    return virtualFile != null ? virtualFile.getPath() : null;
  }

  public List<ErlangModuleDescriptor> getSortedModules() throws CyclicDependencyFoundException {
    if (!isAcyclic) throw new CyclicDependencyFoundException();
    return sortedModules == null ? doGetSortedModules() : sortedModules;
  }

  private List<ErlangModuleDescriptor> doGetSortedModules() throws CyclicDependencyFoundException {
    GraphGenerator<ErlangModuleNode> graph = createModulesGraph();
    DFSTBuilder<ErlangModuleNode> builder = new DFSTBuilder<ErlangModuleNode>(graph);
    builder.buildDFST();
    if (!builder.isAcyclic()) {
      isAcyclic = false;
      throw new CyclicDependencyFoundException();
    }
    sortedModules = ContainerUtil.mapNotNull(builder.getSortedNodes(), new Function<ErlangModuleNode, ErlangModuleDescriptor>() {
      @Override
      public ErlangModuleDescriptor fun(ErlangModuleNode node) {
        return getModuleDescriptor(node);
      }
    });
    return sortedModules;
  }

  private GraphGenerator<ErlangModuleNode> createModulesGraph() {
    return GraphGenerator.create(new ErlangModulesDependencyGraph(myModules, myGlobalParseTransforms));
  }

  public static class ErlangModulesDependencyGraph implements GraphGenerator.SemiGraph<ErlangModuleNode> {

    private final HashMap<String, ErlangModuleNode> myNamesToNodesMap;

    public ErlangModulesDependencyGraph(Collection<ErlangFile> modules, List<String> globalParseTransforms) {
      myNamesToNodesMap = new HashMap<String, ErlangModuleNode>(modules.size());
      for (ErlangFile moduleFile : modules) {
        ErlangModuleNode node = new ErlangModuleNode(moduleFile);
        myNamesToNodesMap.put(node.getModuleName(), node);
      }
      buildDependencies(globalParseTransforms);
    }

    private void buildDependencies(List<String> globalParseTransforms) {
      List<ErlangModuleNode> globalPtNodes = getModuleNodes(globalParseTransforms);
      for (ErlangModuleNode module : myNamesToNodesMap.values()) {
        Set<String> moduleNames = new HashSet<String>();
        moduleNames.addAll(ErlangPsiImplUtil.getAppliedParseTransformModuleNames(module.getModuleFile()));
        moduleNames.addAll(ErlangPsiImplUtil.getImplementedBehaviourModuleNames(module.getModuleFile()));
        List<ErlangModuleNode> dependencies = getModuleNodes(moduleNames);
        module.addDependencies(dependencies);
        module.addDependencies(globalPtNodes);
      }
    }

    private List<ErlangModuleNode> getModuleNodes(Collection<String> parseTransforms) {
      ArrayList<ErlangModuleNode> ptNodes = new ArrayList<ErlangModuleNode>(parseTransforms.size());
      for (String pt : parseTransforms) {
        ContainerUtil.addIfNotNull(myNamesToNodesMap.get(pt), ptNodes);
      }
      return ptNodes;
    }

    @Override
    public Collection<ErlangModuleNode> getNodes() {
      return myNamesToNodesMap.values();
    }

    @Override
    public Iterator<ErlangModuleNode> getIn(ErlangModuleNode node) {
      return node.getDependencies().iterator();
    }

  }

  public static class ErlangModuleNode {
    private final ErlangFile myModuleFile;
    private final ArrayList<ErlangModuleNode> myDependencies = new ArrayList<ErlangModuleNode>();

    ErlangModuleNode(ErlangFile moduleFile) {
      myModuleFile = moduleFile;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (o == null || getClass() != o.getClass()) return false;

      ErlangModuleNode node = (ErlangModuleNode) o;

      if (!myModuleFile.getName().equals(node.myModuleFile.getName())) return false;

      return true;
    }

    @Override
    public int hashCode() {
      return myModuleFile.getName().hashCode();
    }

    ErlangFile getModuleFile() {
      return myModuleFile;
    }

    String getModuleName() {
      return FileUtil.getNameWithoutExtension(myModuleFile.getName());
    }

    void addDependency(@Nullable ErlangModuleNode dep) {
      if (dep != null && dep != this) {
        myDependencies.add(dep);
      }
    }

    public void addDependencies(@NotNull Collection<ErlangModuleNode> deps) {
      for (ErlangModuleNode dep : deps) {
        addDependency(dep);
      }
    }

    @NotNull
    List<ErlangModuleNode> getDependencies() {
      return myDependencies;
    }
  }
}
