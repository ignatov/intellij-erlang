/*
 * Copyright 2012-2014 Sergey Ignatov
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

import com.intellij.compiler.server.BuildManager;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileTask;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.Function;
import com.intellij.util.ObjectUtils;
import com.intellij.util.SystemProperties;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.graph.DFSTBuilder;
import com.intellij.util.graph.GraphGenerator;
import com.intellij.util.xmlb.SkipDefaultValuesSerializationFilters;
import com.intellij.util.xmlb.XmlSerializationException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.jps.builder.ErlangBuilder;
import org.intellij.erlang.jps.builder.ErlangFileDescriptor;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrderDescriptor;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrders;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jdom.Document;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.File;
import java.io.IOException;
import java.util.*;


public class ErlangPrepareDependenciesCompileTask implements CompileTask {
  @Override
  public boolean execute(final CompileContext context) {
    Project project = context.getProject();
    if (ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled()) {
      // delegate dependencies resolution to rebar
      return true;
    }

    File projectSystemDirectory = BuildManager.getInstance().getProjectSystemDirectory(project);
    if (projectSystemDirectory == null) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }

    ErlangModuleBuildOrders buildOrders = ApplicationManager.getApplication().runReadAction(new Computable<ErlangModuleBuildOrders>() {
      @Nullable
      @Override
      public ErlangModuleBuildOrders compute() {
        return getModuleBuildOrders(context);
      }
    });
    if (buildOrders == null) {
      return false; // errors are reported to context.
    }
    try {
      Document serializedBuildOrders = new Document(XmlSerializer.serialize(buildOrders, new SkipDefaultValuesSerializationFilters()));
      File file = new File(projectSystemDirectory, ErlangBuilder.DEPENDENCIES_CONFIG_FILE_PATH);
      //noinspection ResultOfMethodCallIgnored
      file.getParentFile().mkdirs();
      JDOMUtil.writeDocument(serializedBuildOrders, file, SystemProperties.getLineSeparator());
    }
    catch (XmlSerializationException e) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }
    catch (IOException e) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }

    return true;
  }

  public static void addPrepareDependenciesFailedMessage(CompileContext context) {
    context.addMessage(CompilerMessageCategory.WARNING, "Failed to submit dependencies info to compiler. Parse transform failures may occur.", null, -1, -1);
  }

  @TestOnly
  static ErlangModuleBuildOrderDescriptor getModuleBuildOrder(Module module) throws CyclicDependencyFoundException {
    return getModuleBuildOrderInner(module);
  }

  @Nullable
  private static ErlangModuleBuildOrders getModuleBuildOrders(CompileContext context) {
    Module[] modulesToCompile = context.getCompileScope().getAffectedModules();
    ErlangModuleBuildOrders buildOrders = new ErlangModuleBuildOrders(modulesToCompile.length);
    try {
      for (Module module : modulesToCompile) {
        buildOrders.myModuleBuildOrderDescriptors.add(getModuleBuildOrderInner(module));
      }
    }
    catch (CyclicDependencyFoundException e) {
      context.addMessage(CompilerMessageCategory.ERROR, "Cyclic erlang module dependency detected. Check parse_transform usages.", null, -1, -1);
      return null;
    }
    return buildOrders;
  }

  private static ErlangModuleBuildOrderDescriptor getModuleBuildOrderInner(Module module) throws CyclicDependencyFoundException {
    ErlangModuleBuildOrderDescriptor buildOrder = new ErlangModuleBuildOrderDescriptor();
    buildOrder.myModuleName = module.getName();
    buildOrder.myOrderedErlangFilePaths = getTopologicallySortedErlangFilesPaths(module, false);
    buildOrder.myOrderedErlangTestFilePaths = getTopologicallySortedErlangFilesPaths(module, true);
    return buildOrder;
  }

  @NotNull
  private static List<String> getGlobalParseTransform(@NotNull Module module) {
    ErlangFacet erlangFacet = ErlangFacet.getFacet(module);
    return erlangFacet != null ? erlangFacet.getConfiguration().getParseTransforms() : ContainerUtil.<String>emptyList();
  }

  @NotNull
  private static List<ErlangFileDescriptor> getTopologicallySortedErlangFilesPaths(@NotNull Module module,
                                                                                   boolean onlyTestModules) throws CyclicDependencyFoundException {
    final ErlangFilesDependencyGraph semiGraph = ErlangFilesDependencyGraph.createSemiGraph(module, onlyTestModules);
    DFSTBuilder<String> builder = new DFSTBuilder<String>(GraphGenerator.create(semiGraph));
    builder.buildDFST();
    if (!builder.isAcyclic()) {
      throw new CyclicDependencyFoundException();
    }
    return ContainerUtil.map(builder.getSortedNodes(), new Function<String, ErlangFileDescriptor>() {
      @Override
      public ErlangFileDescriptor fun(String filePath) {
        List<String> dependencies = semiGraph.getDependencies(filePath);
        return new ErlangFileDescriptor(filePath, dependencies);
      }
    });
  }

  private static class ErlangFilesDependencyGraph implements GraphGenerator.SemiGraph<String> {

    private final Map<String, String> myNamesToPathsMap = ContainerUtil.newHashMap();
    private final Map<String, List<String>> myPathsToDependenciesMap = ContainerUtil.newHashMap();
    private final PsiManager myPsiManager;

    private ErlangFilesDependencyGraph(@NotNull Collection<VirtualFile> erlangModules,
                                       @NotNull Collection<VirtualFile> erlangHeaders,
                                       @NotNull List<String> globalParseTransforms,
                                       @NotNull PsiManager psiManager) {
      myPsiManager = psiManager;
      buildNamesMap(erlangModules);
      buildForHeaders(erlangHeaders);
      buildForModules(erlangModules, globalParseTransforms);
    }

    @NotNull
    public static ErlangFilesDependencyGraph createSemiGraph(@NotNull Module module,
                                                             boolean onlyTestModules) {
      Collection<VirtualFile> erlangModules = ErlangModulesUtil.getErlangModuleFiles(module, onlyTestModules);
      Collection<VirtualFile> erlangHeaders = ErlangModulesUtil.getErlangHeaderFiles(module, onlyTestModules);

      return new ErlangFilesDependencyGraph(erlangModules, erlangHeaders, getGlobalParseTransform(module), PsiManager.getInstance(module.getProject()));
    }

    private void buildForModules(@NotNull Collection<VirtualFile> erlangModules,
                                 @NotNull List<String> globalParseTransforms) {
      for (VirtualFile erlangModule : erlangModules) {
        Set<String> dependencies = ContainerUtil.newHashSet();
        ErlangFile erlangFile = getErlangFile(erlangModule);
        addDeclaredDependencies(erlangFile, dependencies);
        dependencies.addAll(getPathsFromNames(globalParseTransforms));
        myPathsToDependenciesMap.put(erlangModule.getPath(), ContainerUtil.newArrayList(dependencies));
      }
    }

    @NotNull
    private ErlangFile getErlangFile(@NotNull VirtualFile virtualFile) {
      PsiFile psiFile = myPsiManager.findFile(virtualFile);
      ErlangFile erlangFile = ObjectUtils.tryCast(psiFile, ErlangFile.class);
      assert erlangFile != null;
      return erlangFile;
    }

    private void buildForHeaders(@NotNull Collection<VirtualFile> erlangHeaders) {
      for (VirtualFile header : erlangHeaders) {
        Set<String> dependencies = ContainerUtil.newHashSet();
        ErlangFile erlangFile = getErlangFile(header);
        addDeclaredDependencies(erlangFile, dependencies);
        myPathsToDependenciesMap.put(header.getPath(), ContainerUtil.newArrayList(dependencies));
      }
    }

    private void addDeclaredDependencies(@NotNull ErlangFile erlangModule, @NotNull Set<String> dependencies) {
      dependencies.addAll(getDeclaredParseTransformPaths(erlangModule));
      dependencies.addAll(getDeclaredBehaviourPaths(erlangModule));
      dependencies.addAll(getDeclaredIncludePaths(erlangModule));
    }

    @NotNull
    private static List<String> getDeclaredIncludePaths(@NotNull ErlangFile file) {
      return ContainerUtil.mapNotNull(ErlangPsiImplUtil.getDirectlyIncludedFiles(file), new Function<ErlangFile, String>() {
        @Nullable
        @Override
        public String fun(ErlangFile erlangFile) {
          VirtualFile virtualFile = erlangFile.getVirtualFile();
          return virtualFile != null ? virtualFile.getPath() : null;
        }
      });
    }

    @NotNull
    private List<String> getDeclaredBehaviourPaths(@NotNull ErlangFile erlangModule) {
      Set<String> behaviours = ContainerUtil.newHashSet();
      ErlangPsiImplUtil.addDeclaredBehaviourModuleNames(erlangModule, behaviours);
      return getPathsFromNames(behaviours);
    }

    @NotNull
    private List<String> getDeclaredParseTransformPaths(@NotNull ErlangFile erlangModule) {
      Set<String> pt = ContainerUtil.newHashSet();
      erlangModule.addDeclaredParseTransforms(pt);
      return getPathsFromNames(pt);
    }

    private void buildNamesMap(@NotNull Collection<VirtualFile> erlangModules) {
      for (VirtualFile erlangModule : erlangModules) {
        myNamesToPathsMap.put(erlangModule.getNameWithoutExtension(), erlangModule.getPath());
      }
    }

    @NotNull
    private List<String> getPathsFromNames(@NotNull Collection<String> erlangModuleNames) {
      return ContainerUtil.mapNotNull(erlangModuleNames, new Function<String, String>() {
        @Override
        public String fun(String name) {
          return myNamesToPathsMap.get(name);
        }
      });
    }

    @Override
    public Collection<String> getNodes() {
      return myPathsToDependenciesMap.keySet();
    }

    @Override
    public Iterator<String> getIn(String filePath) {
      return myPathsToDependenciesMap.get(filePath).iterator();
    }

    @NotNull
    public List<String> getDependencies(@NotNull String filePath) {
      List<String> dependencies = myPathsToDependenciesMap.get(filePath);
      assert dependencies != null;
      return dependencies;
    }
  }

  static class CyclicDependencyFoundException extends Exception {
    CyclicDependencyFoundException() {
    }
  }
}