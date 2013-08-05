package org.intellij.erlang.compilation;

import com.intellij.compiler.server.BuildManager;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileTask;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModuleFileIndex;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.util.Function;
import com.intellij.util.Processor;
import com.intellij.util.SystemProperties;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Convertor;
import com.intellij.util.xmlb.SkipDefaultValuesSerializationFilters;
import com.intellij.util.xmlb.XmlSerializationException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.jps.builder.ErlangBuilder;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrderDescriptor;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrders;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jdom.Document;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * @author savenko
 */

public class ErlangPrepareDependenciesCompileTask implements CompileTask {
  @Override
  public boolean execute(final CompileContext context) {
    File projectSystemDirectory = BuildManager.getInstance().getProjectSystemDirectory(context.getProject());
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
      file.getParentFile().mkdirs();
      JDOMUtil.writeDocument(serializedBuildOrders, file, SystemProperties.getLineSeparator());
    } catch (XmlSerializationException e) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    } catch (IOException e) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }

    return true;
  }

  public static void addPrepareDependenciesFailedMessage(CompileContext context) {
    context.addMessage(CompilerMessageCategory.WARNING, "Failed to submit dependencies info to compiler. Parse transform failures may occur.", null, -1, -1);
  }

  @Nullable
  private static ErlangModuleBuildOrders getModuleBuildOrders(CompileContext context) {
    Module[] modulesToCompile = context.getCompileScope().getAffectedModules();
    ErlangModuleBuildOrders buildOrders = new ErlangModuleBuildOrders(modulesToCompile.length);
    try {
      for (Module module : modulesToCompile) {
        buildOrders.myModuleBuildOrderDescriptors.add(getModuleBuildOrder(module));
      }
    } catch (CyclicDependencyFoundException e) {
      context.addMessage(CompilerMessageCategory.ERROR, "Cyclic erlang module dependency detected. Check parse_transform usages.", null, -1, -1);
      return null;
    }
    return buildOrders;
  }

  // It's public for test purposes only
  public static ErlangModuleBuildOrderDescriptor getModuleBuildOrder(Module module) throws CyclicDependencyFoundException {
    ErlangModuleBuildOrderDescriptor buildOrder = new ErlangModuleBuildOrderDescriptor();
    ErlangFacet erlangFacet = ErlangFacet.getFacet(module);
    List<String> globalParseTransforms = erlangFacet != null ? erlangFacet.getConfiguration().getParseTransforms() : ContainerUtil.<String>emptyList();
    buildOrder.myModuleName = module.getName();
    buildOrder.myOrderedErlangModulePaths = getTopologicallySortedErlangModulePaths(getErlangModules(module, false), globalParseTransforms);
    buildOrder.myOrderedErlangTestModulePaths = getTopologicallySortedErlangModulePaths(getErlangModules(module, true), ContainerUtil.<String>emptyList());
    return buildOrder;
  }

  private static Collection<ErlangFile> getErlangModules(Module module, boolean onlyTestModules) {
    ModuleRootManager rootManager = ModuleRootManager.getInstance(module);
    ModuleFileIndex moduleFileIndex = rootManager.getFileIndex();
    Set<ErlangFile> erlangFiles = new HashSet<ErlangFile>();

    Processor<VirtualFile> modulesCollector = getErlangModulesCollector(PsiManager.getInstance(module.getProject()), erlangFiles);
    Convertor<VirtualFile, Boolean> sourceDirectoriesFilter = onlyTestModules ? getTestDirectoriesFilter(moduleFileIndex) : getSourceDirectoriesFilter(moduleFileIndex);

    for (VirtualFile sourceRoot : rootManager.getSourceRoots(onlyTestModules)) {
      VfsUtil.processFilesRecursively(sourceRoot, modulesCollector, sourceDirectoriesFilter);
    }

    return erlangFiles;
  }

  private static Convertor<VirtualFile, Boolean> getSourceDirectoriesFilter(final ModuleFileIndex moduleFileIndex) {
    return new Convertor<VirtualFile, Boolean>() {
      @Override
      public Boolean convert(VirtualFile dir) {
        return moduleFileIndex.isInSourceContent(dir);
      }
    };
  }

  private static Convertor<VirtualFile, Boolean> getTestDirectoriesFilter(final ModuleFileIndex moduleFileIndex) {
    return new Convertor<VirtualFile, Boolean>() {
      @Override
      public Boolean convert(VirtualFile dir) {
        return moduleFileIndex.isInTestSourceContent(dir);
      }
    };
  }

  private static Processor<VirtualFile> getErlangModulesCollector(final PsiManager psiManager, final Collection<ErlangFile> erlangFiles) {
    return new Processor<VirtualFile>() {
      @Override
      public boolean process(VirtualFile virtualFile) {
        if (virtualFile.getFileType() == ErlangFileType.MODULE) {
          PsiFile psiFile = psiManager.findFile(virtualFile);
          if (psiFile instanceof ErlangFile) {
            erlangFiles.add((ErlangFile) psiFile);
          }
        }
        return true;
      }
    };
  }

  private static List<String> getTopologicallySortedErlangModulePaths(Collection<ErlangFile> erlangModules, List<String> globalParseTransforms) throws CyclicDependencyFoundException {
    if (true) return ContainerUtil.map(erlangModules, new Function<ErlangFile, String>() {
      @Override
      public String fun(ErlangFile erlangFile) {
        return FileUtil.getNameWithoutExtension(erlangFile.getName());
      }
    });  // todo: tmp
    ErlangModulesDependencyGraph depsGraph = new ErlangModulesDependencyGraph(erlangModules, globalParseTransforms);
    return ContainerUtil.mapNotNull(depsGraph.getSortedModules(), new Function<ErlangFile, String>() {
      @Nullable
      @Override
      public String fun(ErlangFile erlangFile) {
        VirtualFile virtualFile = erlangFile.getVirtualFile();
        return virtualFile != null ? virtualFile.getPath() : null;
      }
    });
  }

  private static class ErlangModulesDependencyGraph {
    private final Map<String, Node> myNamesToNodesMap;

    public ErlangModulesDependencyGraph(Collection<ErlangFile> modules, List<String> globalParseTransforms) {
      myNamesToNodesMap = new HashMap<String, Node>(modules.size());
      for (ErlangFile moduleFile : modules) {
        Node node = new Node(moduleFile);
        myNamesToNodesMap.put(node.getModuleName(), node);
      }
      buildDependencies(globalParseTransforms);
    }

    public List<ErlangFile> getSortedModules() throws CyclicDependencyFoundException {
      List<ErlangFile> result = new ArrayList<ErlangFile>(myNamesToNodesMap.size());
      for (Node node : myNamesToNodesMap.values()) {
        if (Node.Status.NONE == node.getStatus()) {
          dfs(node, result);
        }
      }
      return result;
    }

    private static void dfs(Node node, List<ErlangFile> visitedModules) throws CyclicDependencyFoundException {
      node.setStatus(Node.Status.STARTED);
      for (Node dep : node.getDependencies()) {
        switch (dep.getStatus()) {
          case NONE: {
            dfs(dep, visitedModules);
          }
          case COMPLETED: {
            break;
          }
          case STARTED: {
            throw new CyclicDependencyFoundException();
          }
        }
      }
      node.setStatus(Node.Status.COMPLETED);
      visitedModules.add(node.getModuleFile());
    }

    private void buildDependencies(List<String> globalParseTransforms) {
      List<Node> globalPtNodes = getParseTransformNodes(globalParseTransforms);
      for (Node module : myNamesToNodesMap.values()) {
        Set<String> ptModuleNames = ErlangPsiImplUtil.getAppliedParseTransformModuleNames(module.getModuleFile());
        List<Node> ptNodes = getParseTransformNodes(ptModuleNames);
        module.addDependencies(ptNodes);
        module.addDependencies(globalPtNodes);
      }
    }

    private List<Node> getParseTransformNodes(Collection<String> parseTransforms) {
      ArrayList<Node> ptNodes = new ArrayList<Node>(parseTransforms.size());
      for (String pt : parseTransforms) {
        ContainerUtil.addIfNotNull(myNamesToNodesMap.get(pt), ptNodes);
      }
      return ptNodes;
    }

    private static class Node {
      enum Status {STARTED, COMPLETED, NONE}

      private Status myStatus = Status.NONE;
      private final ErlangFile myModuleFile;
      private final ArrayList<Node> myDependencies = new ArrayList<Node>();

      Node(ErlangFile moduleFile) {
        myModuleFile = moduleFile;
      }

      private Status getStatus() {
        return myStatus;
      }

      private void setStatus(Status status) {
        myStatus = status;
      }

      ErlangFile getModuleFile() {
        return myModuleFile;
      }

      String getModuleName() {
        return StringUtil.trimEnd(myModuleFile.getName(), "." + ErlangFileType.MODULE.getDefaultExtension());
      }

      void addDependency(@Nullable Node dep) {
        if (dep != null && dep != this) {
          myDependencies.add(dep);
        }
      }

      void addDependencies(Collection<Node> deps) {
        for (Node dep : deps) {
          addDependency(dep);
        }
      }

      List<Node> getDependencies() {
        return myDependencies;
      }
    }
  }

  public static class CyclicDependencyFoundException extends Exception {
    CyclicDependencyFoundException() {
    }
  }
}