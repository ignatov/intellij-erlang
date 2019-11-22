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

import com.intellij.openapi.util.Conditions;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.graph.Graph;
import com.intellij.util.graph.GraphGenerator;
import com.intellij.util.graph.InboundSemiGraph;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.BuildRootIndex;
import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.intellij.erlang.jps.builder.ErlangBuilderUtil.*;

public class ErlangModuleBuildOrderBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangModuleBuildOrderTarget> {
  private static final String NAME = "Build order builder";

  public ErlangModuleBuildOrderBuilder() {
    super(Collections.singletonList(ErlangModuleBuildOrderTargetType.INSTANCE));
  }

  @Override
  public void build(@NotNull ErlangModuleBuildOrderTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangModuleBuildOrderTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws IOException {
    LOG.info("Computing dirty files");
    LOG.debug("Load project build order.");
    ErlangProjectBuildOrder projectBuildOrder = loadProjectBuildOrder(context);
    if (projectBuildOrder == null) {
      addPrepareDependenciesFailedMessage(context);
      return;
    }

    setEmptyBuildOrders(context);

    LOG.debug("Collect dirty files.");
    List<String> dirtyErlangFilePaths = new DirtyFilesProcessor<String, ErlangModuleBuildOrderTarget>() {
      @Nullable
      @Override
      protected String getDirtyElement(@NotNull File file) {
        String fileName = file.getName();
        return isSource(fileName) || isHeader(fileName) ? ErlangBuilderUtil.getPath(file) : null;
      }
    }.collectDirtyElements(holder);

    if (dirtyErlangFilePaths.isEmpty()) {
      LOG.debug("There are no dirty .erl or .hrl files.");
    }
    else {
      LOG.debug("Search dirty modules.");
      List<String> sortedDirtyModules = getSortedDirtyModules(projectBuildOrder, dirtyErlangFilePaths);
      addFilesToBuildTarget(context, sortedDirtyModules);
    }
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  private static void setEmptyBuildOrders(@NotNull CompileContext context) {
    List<BuildTarget<?>> allTargets = context.getProjectDescriptor().getBuildTargetIndex().getAllTargets();
    List<BuildTarget<?>> erlangTargets = ContainerUtil.filter(allTargets, Conditions.instanceOf(ErlangTarget.class));
    for (BuildTarget<?> erlangTarget : erlangTargets) {
      ((ErlangTarget) erlangTarget).setBuildOrder(new ErlangModuleBuildOrder());
    }
  }

  @Nullable
  private static ErlangProjectBuildOrder loadProjectBuildOrder(@NotNull CompileContext context) {
    return readFromXML(context, BUILD_ORDER_FILE_NAME, ErlangProjectBuildOrder.class);
  }

  @NotNull
  private static List<String> getSortedDirtyModules(@NotNull ErlangProjectBuildOrder projectBuildOrder,
                                                    @NotNull List<String> dirtyErlangFilePaths) {
    Set<String> allDirtyFiles = getAllDirtyFiles(projectBuildOrder, dirtyErlangFilePaths);
    return getSortedDirtyModules(projectBuildOrder.myErlangFiles, allDirtyFiles);
  }

  private static void addPrepareDependenciesFailedMessage(@NotNull CompileContext context) {
    context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.WARNING, "The project will be fully rebuilt due to errors."));
  }

  @NotNull
  private static Set<String> getAllDirtyFiles(@NotNull ErlangProjectBuildOrder projectBuildOrder,
                                              @NotNull List<String> dirtyFiles) {
    Graph<String> dependencies = GraphGenerator.generate(new SortedModuleDependencyGraph(projectBuildOrder));
    Set<String> allDirtyFiles = new HashSet<>();
    for (String dirtyFile : dirtyFiles) {
      collectDirtyFiles(dirtyFile, dependencies, allDirtyFiles);
    }
    return allDirtyFiles;
  }

  private static void collectDirtyFiles(@NotNull String filePath,
                                        @NotNull Graph<String> dependenciesGraph,
                                        @NotNull Set<String> dirtyFiles) {
    if (dirtyFiles.contains(filePath)) return;
    dirtyFiles.add(filePath);
    if (!dependenciesGraph.getNodes().contains(filePath)) {
      LOG.warn("Unexpected dirty file detected. " +
               "Please, report to https://github.com/ignatov/intellij-erlang/issues/685. " +
               "Path: " + filePath);
      return;
    }
    Iterator<String> dependentFilesIterator = dependenciesGraph.getOut(filePath);
    while (dependentFilesIterator.hasNext()) {
      collectDirtyFiles(dependentFilesIterator.next(), dependenciesGraph, dirtyFiles);
    }
  }

  @NotNull
  private static List<String> getSortedDirtyModules(@NotNull List<ErlangFileDescriptor> sortedFiles,
                                                    @NotNull final Set<String> allDirtyFiles) {
    return ContainerUtil.mapNotNull(sortedFiles, node -> isSource(node.myPath) && allDirtyFiles.contains(node.myPath) ? node.myPath : null);
  }

  private static void addFilesToBuildTarget(@NotNull CompileContext context,
                                            @NotNull List<String> sortedDirtyErlangModules) {
    List<ErlangTargetType> targetTypes = Collections.singletonList(ErlangTargetType.INSTANCE);
    BuildRootIndex buildRootIndex = context.getProjectDescriptor().getBuildRootIndex();
    for (String filePath : sortedDirtyErlangModules) {
      ErlangSourceRootDescriptor root = buildRootIndex.findParentDescriptor(new File(filePath), targetTypes, context);
      if (root == null) {
        LOG.error("Source root not found.");
        return;
      }
      ErlangTarget target = (ErlangTarget) root.getTarget();

      ErlangModuleBuildOrder buildOrder = target.getBuildOrder();
      if (buildOrder == null) {
        LOG.error("buildOrder for erlang module target are not set.");
        return;
      }

      if (root.isTests()) {
        buildOrder.myOrderedErlangTestFilePaths.add(filePath);
      }
      else {
        buildOrder.myOrderedErlangFilePaths.add(filePath);
      }
    }
  }

  private static class SortedModuleDependencyGraph implements InboundSemiGraph<String> {
    private final LinkedHashMap<String, List<String>> myPathsToDependenciesMap = new LinkedHashMap<>();

    public SortedModuleDependencyGraph(@NotNull ErlangProjectBuildOrder projectBuildOrder) {
      for (ErlangFileDescriptor node : projectBuildOrder.myErlangFiles) {
        myPathsToDependenciesMap.put(node.myPath, node.myDependencies);
      }
    }

    @NotNull
    @Override
    public Collection<String> getNodes() {
      return myPathsToDependenciesMap.keySet();
    }

    @Override
    @NotNull
    public Iterator<String> getIn(String node) {
      return myPathsToDependenciesMap.get(node).iterator();
    }
  }
}
