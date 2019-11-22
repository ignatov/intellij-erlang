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
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.Couple;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
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
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.jps.builder.ErlangBuilderUtil;
import org.intellij.erlang.jps.builder.ErlangFileDescriptor;
import org.intellij.erlang.jps.builder.ErlangProjectBuildOrder;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jdom.Document;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.File;
import java.io.IOException;
import java.util.*;

import static org.intellij.erlang.utils.ErlangModulesUtil.getErlangHeaderFiles;
import static org.intellij.erlang.utils.ErlangModulesUtil.getErlangModuleFiles;

public class ErlangPrepareDependenciesCompileTask implements CompileTask {
  private static final Logger LOG = Logger.getInstance(ErlangPrepareDependenciesCompileTask.class);

  @Override
  public boolean execute(final CompileContext context) {
    Project project = context.getProject();
    if (ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled()) {
      // delegate dependencies resolution to rebar
      return true;
    }
    LOG.info("Prepare build order for project " + project.getName());

    File projectSystemDirectory = BuildManager.getInstance().getProjectSystemDirectory(project);
    if (projectSystemDirectory == null) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }
    ErlangProjectBuildOrder projectBuildOrder = ApplicationManager.getApplication().runReadAction((Computable<ErlangProjectBuildOrder>) () -> getProjectBuildOrder(context));
    if (projectBuildOrder == null) {
      return false; // errors are reported to context.
    }
    writeBuildOrder(context, projectSystemDirectory, projectBuildOrder);
    return true;
  }

  private static void writeBuildOrder(@NotNull CompileContext context,
                                      @NotNull File projectSystemDirectory,
                                      @NotNull ErlangProjectBuildOrder projectBuildOrder) {
    try {
      LOG.debug("Serialize build order");
      Document serializedDocument = new Document(XmlSerializer.serialize(projectBuildOrder, new SkipDefaultValuesSerializationFilters()));
      File parentDir = new File(projectSystemDirectory, ErlangBuilderUtil.BUILDER_DIRECTORY);
      //noinspection ResultOfMethodCallIgnored
      parentDir.mkdirs();
      File file = new File(parentDir, ErlangBuilderUtil.BUILD_ORDER_FILE_NAME);
      LOG.debug("Write build order to " + file.getAbsolutePath());
      JDOMUtil.writeDocument(serializedDocument, file, SystemProperties.getLineSeparator());
    }
    catch (XmlSerializationException e) {
      LOG.error("Can't serialize build order object.", e);
      addPrepareDependenciesFailedMessage(context);
    }
    catch (IOException e) {
      LOG.warn("Some I/O errors occurred while writing build orders to file", e);
      addPrepareDependenciesFailedMessage(context);
    }
  }

  private static void addPrepareDependenciesFailedMessage(@NotNull CompileContext context) {
    context.addMessage(CompilerMessageCategory.WARNING, "Failed to submit dependencies info to compiler.", null, -1, -1);
  }

  @TestOnly
  @NotNull
  static List<ErlangFileDescriptor> getBuildOrder(@NotNull Module module) throws CyclicDependencyFoundException {
    return getTopologicallySortedFileDescriptors(module);
  }

  @Nullable
  private static ErlangProjectBuildOrder getProjectBuildOrder(@NotNull CompileContext context) {
    try {
      Module[] modulesToCompile = context.getCompileScope().getAffectedModules();
      return new ErlangProjectBuildOrder(getTopologicallySortedFileDescriptors(modulesToCompile));
    }
    catch (CyclicDependencyFoundException e) {
      String message = "Cyclic erlang module dependency detected. Check files " +
                       e.getFirstFileInCycle() + " and " + e.getLastFileInCycle() +
                       "or their dependencies(parse_transform, behaviour, include)";
      LOG.debug(message, e);
      context.addMessage(CompilerMessageCategory.ERROR, message, null, -1, -1);
      return null;
    }
  }

  @NotNull
  private static List<String> getGlobalParseTransforms(@NotNull Module module) {
    ErlangFacet erlangFacet = ErlangFacet.getFacet(module);
    return erlangFacet != null ? erlangFacet.getConfiguration().getParseTransforms() : ContainerUtil.emptyList();
  }

  @NotNull
  private static List<ErlangFileDescriptor> getTopologicallySortedFileDescriptors(@NotNull Module... modulesToCompile) throws CyclicDependencyFoundException {
    final ErlangFilesDependencyGraph semiGraph = ErlangFilesDependencyGraph.createSemiGraph(modulesToCompile);
    DFSTBuilder<String> builder = new DFSTBuilder<>(GraphGenerator.generate(semiGraph));
    if (!builder.isAcyclic()) {
      throw new CyclicDependencyFoundException(builder.getCircularDependency());
    }
    return ContainerUtil.map(builder.getSortedNodes(), filePath -> new ErlangFileDescriptor(filePath, semiGraph.getDependencies(filePath)));
  }

  @NotNull
  private static String getPath(@NotNull VirtualFile file) {
    File ioFile = VfsUtilCore.virtualToIoFile(file);
    return ErlangBuilderUtil.getPath(ioFile);
  }

  private static class ErlangFilesDependencyGraph implements com.intellij.util.graph.InboundSemiGraph<String> {
    private final Project myProject;
    private final PsiManager myPsiManager;
    private final Set<String> myHeaders;
    private final Map<String, List<String>> myPathsToDependenciesMap = ContainerUtil.newHashMap();

    private ErlangFilesDependencyGraph(@NotNull Module[] modulesToCompile) {
      assert modulesToCompile.length > 0;
      myProject = modulesToCompile[0].getProject();
      myPsiManager = PsiManager.getInstance(myProject);
      myHeaders = collectHeaderPaths(modulesToCompile);
      for (Module module : modulesToCompile) {
        buildDependenciesMap(module);
      }
    }

    @NotNull
    public static ErlangFilesDependencyGraph createSemiGraph(@NotNull Module[] modulesToCompile) {
      return new ErlangFilesDependencyGraph(modulesToCompile);
    }

    @NotNull
    private static Set<String> collectHeaderPaths(@NotNull Module[] modulesToCompile) {
      Set<String> erlangHeaders = new HashSet<>();
      for (Module module : modulesToCompile) {
        erlangHeaders.addAll(getHeaders(module, false));
        erlangHeaders.addAll(getHeaders(module, true));
      }
      return erlangHeaders;
    }

    @NotNull
    private static List<String> getHeaders(Module module, boolean onlyTestModules) {
      return ContainerUtil.map(getErlangHeaderFiles(module, onlyTestModules), ErlangPrepareDependenciesCompileTask::getPath);
    }

    @NotNull
    @Override
    public Collection<String> getNodes() {
      return myPathsToDependenciesMap.keySet();
    }

    @Override
    @NotNull
    public Iterator<String> getIn(@NotNull String filePath) {
      return myPathsToDependenciesMap.get(filePath).iterator();
    }

    @NotNull
    public List<String> getDependencies(@NotNull String filePath) {
      return ObjectUtils.assertNotNull(myPathsToDependenciesMap.get(filePath));
    }

    private void buildDependenciesMap(@NotNull Module module) {
      List<String> globalParseTransform = resolvePathsFromNames(getGlobalParseTransforms(module), module);
      buildDependenciesMap(module, getErlangHeaderFiles(module, false), ContainerUtil.emptyList());
      buildDependenciesMap(module, getErlangHeaderFiles(module, true), ContainerUtil.emptyList());
      buildDependenciesMap(module, getErlangModuleFiles(module, false), globalParseTransform);
      buildDependenciesMap(module, getErlangModuleFiles(module, true), globalParseTransform);
    }

    private void buildDependenciesMap(@NotNull Module module,
                                      @NotNull Collection<VirtualFile> erlangFiles,
                                      @NotNull List<String> globalParseTransforms) {
      for (VirtualFile file : erlangFiles) {
        Set<String> dependencies = new HashSet<>();
        ErlangFile psi = getErlangFile(file);
        addDeclaredDependencies(module, psi, dependencies);
        dependencies.addAll(globalParseTransforms);
        myPathsToDependenciesMap.put(getPath(file), new ArrayList<>(dependencies));
      }
    }

    @NotNull
    private ErlangFile getErlangFile(@NotNull VirtualFile virtualFile) {
      PsiFile psiFile = myPsiManager.findFile(virtualFile);
      return ObjectUtils.assertNotNull(ObjectUtils.tryCast(psiFile, ErlangFile.class));
    }

    private void addDeclaredDependencies(@NotNull Module module,
                                         @NotNull ErlangFile erlangModule,
                                         @NotNull Set<String> dependencies) {
      dependencies.addAll(getDeclaredParseTransformPaths(module, erlangModule));
      dependencies.addAll(getDeclaredBehaviourPaths(module, erlangModule));
      dependencies.addAll(getDeclaredIncludePaths(erlangModule));
    }

    @NotNull
    private List<String> getDeclaredParseTransformPaths(@NotNull Module module, @NotNull ErlangFile erlangModule) {
      Set<String> pt = new HashSet<>();
      erlangModule.addDeclaredParseTransforms(pt);
      return resolvePathsFromNames(pt, module);
    }

    @NotNull
    private List<String> getDeclaredBehaviourPaths(@NotNull Module module, @NotNull ErlangFile erlangModule) {
      Set<String> behaviours = new HashSet<>();
      ErlangPsiImplUtil.addDeclaredBehaviourModuleNames(erlangModule, behaviours);
      return resolvePathsFromNames(behaviours, module);
    }

    @NotNull
    private List<String> resolvePathsFromNames(@NotNull Collection<String> erlangModuleNames, @NotNull Module module) {
      List<String> paths = new ArrayList<>();
      for (String erlangModuleName : erlangModuleNames) {
        paths.addAll(getPathsFromModuleName(erlangModuleName, module));
      }
      return paths;
    }

    @NotNull
    private List<String> getDeclaredIncludePaths(@NotNull ErlangFile file) {
      return ContainerUtil.mapNotNull(ErlangPsiImplUtil.getDirectlyIncludedFiles(file), erlangFile -> {
        VirtualFile file1 = erlangFile.getVirtualFile();
        String path = file1 != null ? getPath(file1) : null;
        return path != null && myHeaders.contains(path) ? path : null;
      });
    }

    @NotNull
    private List<String> getPathsFromModuleName(@NotNull String erlangModuleName, @NotNull Module module) {
      List<ErlangFile> filesByName = ErlangModuleIndex.getFilesByName(myProject,
                                                                      erlangModuleName,
                                                                      GlobalSearchScope.moduleWithDependenciesScope(module));
      return ContainerUtil.mapNotNull(filesByName, erlangFile -> {
        VirtualFile virtualFile = erlangFile.getVirtualFile();
        return virtualFile != null ? getPath(virtualFile) : null;
      });
    }
  }

  static class CyclicDependencyFoundException extends Exception {
    private final Couple<String> myCyclicDependencies;

    CyclicDependencyFoundException(@NotNull Couple<String> cyclicDependencies) {
      this.myCyclicDependencies = cyclicDependencies;
    }

    @NotNull
    public String getFirstFileInCycle() {
      return myCyclicDependencies.getFirst();
    }

    @NotNull
    public String getLastFileInCycle() {
      return myCyclicDependencies.getSecond();
    }
  }
}