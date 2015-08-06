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

package org.intellij.erlang.jps.builder;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.CommonProcessors;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.graph.GraphGenerator;
import com.intellij.util.xmlb.XmlSerializationException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.jps.model.*;
import org.jdom.Document;
import org.jdom.JDOMException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.builders.FileProcessor;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.incremental.resources.ResourcesBuilder;
import org.jetbrains.jps.incremental.resources.StandardResourceBuilderEnabler;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.java.JavaSourceRootType;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.*;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.*;

public class ErlangBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  public static final String DEPENDENCIES_CONFIG_FILE_PATH = "erlang-builder/deps-config.xml";
  public static final String NAME = "erlc";
  private static final Logger LOG = Logger.getInstance(ErlangBuilder.class);

  public ErlangBuilder() {
    super(Collections.singletonList(ErlangTargetType.INSTANCE));

    //TODO provide a way to copy erlang resources
    //disables java resource builder for erlang modules
    ResourcesBuilder.registerEnabler(new StandardResourceBuilderEnabler() {
      @Override
      public boolean isResourceProcessingEnabled(@NotNull JpsModule module) {
        return !(module.getModuleType() instanceof JpsErlangModuleType);
      }
    });
  }

  @Override
  public void build(@NotNull ErlangTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws ProjectBuildException, IOException {
    LOG.debug(target.getPresentableName());
    if (!holder.hasDirtyFiles() && !holder.hasRemovedFiles()) return;

    JpsModule module = target.getModule();
    JpsProject project = module.getProject();
    ErlangCompilerOptions compilerOptions = JpsErlangCompilerOptionsExtension.getOrCreateExtension(project).getOptions();
    if (compilerOptions.myUseRebarCompiler) return;

    File sourceOutput = getBuildOutputDirectory(module, false, context);
    File testOutput = getBuildOutputDirectory(module, true, context);

    buildSources(target, context, compilerOptions, holder, outputConsumer, sourceOutput, false);
    buildSources(target, context, compilerOptions, holder, outputConsumer, testOutput, true);

    processAppConfigFiles(holder, outputConsumer, context, sourceOutput, testOutput);
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  private static void reportProgress(@NotNull CompileContext context, String message) {
    context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.PROGRESS, message));
  }

  private static void buildSources(ErlangTarget target,
                                   CompileContext context,
                                   ErlangCompilerOptions compilerOptions,
                                   DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                                   BuildOutputConsumer outputConsumer,
                                   File outputDir,
                                   final boolean isTests) throws IOException, ProjectBuildException {
    List<String> dirtyErlangFilePaths = new DirtyFileProcessor<String>() {
      @Nullable
      @Override
      protected String getDirtyElement(@NotNull ErlangTarget target,
                                       @NotNull File file,
                                       @NotNull ErlangSourceRootDescriptor root) throws IOException {
        return (isTests || !root.isTests()) && isSourceOrHeader(file) ? file.getAbsolutePath() : null;
      }
    }.collectDirtyElements(holder);

    List<String> erlangModulePathsToCompile = getErlangModulePaths(target, context, dirtyErlangFilePaths, isTests);
    if (erlangModulePathsToCompile.isEmpty()) {
      reportProgress(context, "Source is up to date");
      return;
    }
    String message = isTests ? "Compile tests for module " : "Compile source code for module ";
    reportProgress(context, message + target.getModule().getName());
    runErlc(target, context, compilerOptions, erlangModulePathsToCompile, outputConsumer, outputDir, isTests);
  }

  private static boolean isSourceOrHeader(@NotNull File file) {
    return file.getName().endsWith(".erl") || file.getName().endsWith(".hrl");
  }

  @NotNull
  private static File getBuildOutputDirectory(@NotNull JpsModule module,
                                              boolean forTests,
                                              @NotNull CompileContext context) throws ProjectBuildException {
    JpsJavaExtensionService instance = JpsJavaExtensionService.getInstance();
    File outputDirectory = instance.getOutputDirectory(module, forTests);
    if (outputDirectory == null) {
      String errorMessage = "No output dir for module " + module.getName();
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }
    if (!outputDirectory.exists()) {
      FileUtil.createDirectory(outputDirectory);
    }
    return outputDirectory;
  }

  private static void processAppConfigFiles(DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                                            BuildOutputConsumer outputConsumer,
                                            CompileContext context,
                                            File... outputDirectories) throws IOException {
    List<File> appConfigFiles = new DirtyFileProcessor<File>() {
      @Nullable
      @Override
      protected File getDirtyElement(@NotNull ErlangTarget target,
                                     @NotNull File file,
                                     @NotNull ErlangSourceRootDescriptor root) throws IOException {
        return isAppConfigFileName(file.getName()) ? file : null;
      }
    }.collectDirtyElements(holder);

    for (File appConfigSrc : appConfigFiles) {
      for (File outputDir : outputDirectories) {
        File appConfigDst = new File(outputDir, getAppConfigDestinationFileName(appConfigSrc.getName()));
        FileUtil.copy(appConfigSrc, appConfigDst);
        reportProgress(context, String.format("Copy %s to %s",appConfigDst.getAbsolutePath(),outputDir.getAbsolutePath()));
        outputConsumer.registerOutputFile(appConfigDst, Collections.singletonList(appConfigSrc.getAbsolutePath()));
      }
    }
  }

  private static boolean isAppConfigFileName(String fileName) {
    return fileName.endsWith(".app") || fileName.endsWith(".app.src");
  }

  @NotNull
  private static String getAppConfigDestinationFileName(String sourceFileName) {
    return StringUtil.trimEnd(sourceFileName, ".src");
  }

  private static void runErlc(ErlangTarget target,
                              CompileContext context,
                              ErlangCompilerOptions compilerOptions,
                              List<String> erlangModulePathsToCompile,
                              BuildOutputConsumer outputConsumer,
                              File outputDirectory,
                              boolean isTest) throws ProjectBuildException, IOException {
    GeneralCommandLine commandLine = getErlcCommandLine(target, context, compilerOptions, outputDirectory, erlangModulePathsToCompile, isTest);
    Process process;
    try {
      process = commandLine.createProcess();
    }
    catch (ExecutionException e) {
      throw new ProjectBuildException("Failed to launch erlang compiler", e);
    }
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, commandLine.getCommandLineString(), Charset.defaultCharset());
    ProcessAdapter adapter = new ErlangCompilerProcessAdapter(context, NAME, "");
    handler.addProcessListener(adapter);
    handler.startNotify();
    handler.waitFor();
    consumeFiles(outputConsumer, getOutputErlangModuleFiles(erlangModulePathsToCompile, outputDirectory));
  }

  private static GeneralCommandLine getErlcCommandLine(ErlangTarget target,
                                                       CompileContext context,
                                                       ErlangCompilerOptions compilerOptions,
                                                       File outputDirectory,
                                                       List<String> erlangModulePaths,
                                                       boolean isTest) throws ProjectBuildException {
    GeneralCommandLine commandLine = new GeneralCommandLine();
    JpsModule module = target.getModule();
    JpsSdk<JpsDummyElement> sdk = ErlangTargetBuilderUtil.getSdk(context, module);
    File executable = JpsErlangSdkType.getByteCodeCompilerExecutable(sdk.getHomePath());
    commandLine.withWorkDirectory(outputDirectory);
    commandLine.setExePath(executable.getAbsolutePath());
    addCodePath(commandLine, module, target, context);
    addParseTransforms(commandLine, module);
    addDebugInfo(commandLine, compilerOptions.myAddDebugInfoEnabled);
    addIncludePaths(commandLine, module);
    addMacroDefinitions(commandLine, isTest);
    commandLine.addParameters(erlangModulePaths);
    return commandLine;
  }

  private static void addMacroDefinitions(GeneralCommandLine commandLine, boolean isTests) {
    if (isTests) {
      commandLine.addParameters("-DTEST");
    }
  }

  private static void addDebugInfo(@NotNull GeneralCommandLine commandLine, boolean addDebugInfoEnabled) {
    if (addDebugInfoEnabled) {
      commandLine.addParameter("+debug_info");
    }
  }

  private static void addIncludePaths(@NotNull GeneralCommandLine commandLine, @Nullable JpsModule module) {
    if (module == null) return;
    for (JpsTypedModuleSourceRoot<JpsDummyElement> includeDirectory : module.getSourceRoots(ErlangIncludeSourceRootType.INSTANCE)) {
      commandLine.addParameters("-I", includeDirectory.getFile().getPath());
    }
  }

  @NotNull
  private static List<String> getErlangModulePaths(@NotNull ErlangTarget target,
                                                   @NotNull CompileContext context,
                                                   @NotNull List<String> dirtyFilePaths,
                                                   boolean isTest) {
    List<ErlangFileDescriptor> fileDescriptors = getErlangFileDescriptorFromConfig(target, context, isTest);
    return fileDescriptors != null ?
           getSortedErlangModulePathsToCompile(fileDescriptors, dirtyFilePaths) :
           getErlangModulePathsDefault(target, isTest);
  }

  @NotNull
  private static List<String> getSortedErlangModulePathsToCompile(@NotNull List<ErlangFileDescriptor> sortedModuleDescriptors,
                                                                  @NotNull List<String> dirtyModules) {
    SortedModuleDependencyGraph semiGraph = new SortedModuleDependencyGraph(sortedModuleDescriptors);
    GraphGenerator<Node> graph = GraphGenerator.create(semiGraph);
    markDirtyNodes(semiGraph.getNodesByName(dirtyModules), graph);
    return ContainerUtil.mapNotNull(semiGraph.getNodes(), new Function<Node, String>() {
      @Nullable
      @Override
      public String fun(Node node) {
        return node.myDirty && FileUtilRt.extensionEquals(node.myErlangModulePath, "erl") ? node.myErlangModulePath : null;
      }
    });
  }

  @NotNull
  private static List<String> getErlangModulePathsDefault(@NotNull ErlangTarget target, boolean isTests) {
    CommonProcessors.CollectProcessor<File> erlFilesCollector = new CommonProcessors.CollectProcessor<File>() {
      @Override
      protected boolean accept(@NotNull File file) {
        return !file.isDirectory() && FileUtilRt.extensionEquals(file.getName(), "erl");
      }
    };
    List<JpsModuleSourceRoot> sourceRoots = ContainerUtil.newArrayList();
    JpsModule module = target.getModule();
    ContainerUtil.addAll(sourceRoots, module.getSourceRoots(JavaSourceRootType.SOURCE));
    if (isTests) {
      ContainerUtil.addAll(sourceRoots, module.getSourceRoots(JavaSourceRootType.TEST_SOURCE));
    }
    for (JpsModuleSourceRoot root : sourceRoots) {
      FileUtil.processFilesRecursively(root.getFile(), erlFilesCollector);
    }
    return ContainerUtil.map(erlFilesCollector.getResults(), new Function<File, String>() {
      @NotNull
      @Override
      public String fun(@NotNull File file) {
        return file.getAbsolutePath();
      }
    });
  }

  @Nullable
  private static List<ErlangFileDescriptor> getErlangFileDescriptorFromConfig(@NotNull ErlangTarget target,
                                                                              @NotNull CompileContext context,
                                                                              boolean isTests) {
    File dataStorageRoot = context.getProjectDescriptor().dataManager.getDataPaths().getDataStorageRoot();
    File depsConfigFile = new File(dataStorageRoot, DEPENDENCIES_CONFIG_FILE_PATH);
    if (!depsConfigFile.exists()) return null;
    ErlangModuleBuildOrders buildOrders;
    try {
      Document document = JDOMUtil.loadDocument(depsConfigFile);
      buildOrders = XmlSerializer.deserialize(document, ErlangModuleBuildOrders.class);
    }
    catch (XmlSerializationException e) {
      return null;
    }
    catch (JDOMException e) {
      return null;
    }
    catch (IOException e) {
      return null;
    }
    if (buildOrders == null) return null;
    for (ErlangModuleBuildOrderDescriptor buildOrder : buildOrders.myModuleBuildOrderDescriptors) {
      if (StringUtil.equals(buildOrder.myModuleName, target.getModule().getName())) {
        List<ErlangFileDescriptor> modules = buildOrder.myOrderedErlangFilePaths;
        if (isTests) {
          modules = ContainerUtil.concat(modules, buildOrder.myOrderedErlangTestFilePaths);
        }
        return modules;
      }
    }
    return null;
  }

  private static void addParseTransforms(@NotNull GeneralCommandLine commandLine,
                                         @Nullable JpsModule module) throws ProjectBuildException {
    JpsErlangModuleExtension extension = JpsErlangModuleExtension.getExtension(module);
    List<String> parseTransforms = extension != null ? extension.getParseTransforms() : Collections.<String>emptyList();
    if (parseTransforms.isEmpty()) return;
    for (String ptModule : parseTransforms) {
      commandLine.addParameter("+{parse_transform, " + ptModule + "}");
    }
  }

  private static void addCodePath(@NotNull GeneralCommandLine commandLine,
                                  @NotNull JpsModule module,
                                  @NotNull ErlangTarget target,
                                  @NotNull CompileContext context) throws ProjectBuildException {
    List<JpsModule> codePathModules = ContainerUtil.newArrayList();
    collectDependentModules(module, codePathModules, ContainerUtil.<String>newHashSet());
    addModuleToCodePath(commandLine, module, target.isTests(), context);
    for (JpsModule codePathModule : codePathModules) {
      if (codePathModule != module) {
        addModuleToCodePath(commandLine, codePathModule, false, context);
      }
    }
  }

  private static void collectDependentModules(@NotNull JpsModule module,
                                              @NotNull Collection<JpsModule> addedModules,
                                              @NotNull Set<String> addedModuleNames) {
    String moduleName = module.getName();
    if (addedModuleNames.contains(moduleName)) return;
    addedModuleNames.add(moduleName);
    addedModules.add(module);
    for (JpsDependencyElement dependency : module.getDependenciesList().getDependencies()) {
      if (!(dependency instanceof JpsModuleDependency)) continue;
      JpsModuleDependency moduleDependency = (JpsModuleDependency) dependency;
      JpsModule depModule = moduleDependency.getModule();
      if (depModule != null) {
        collectDependentModules(depModule, addedModules, addedModuleNames);
      }
    }
  }

  private static void addModuleToCodePath(@NotNull GeneralCommandLine commandLine,
                                          @NotNull JpsModule module,
                                          boolean forTests,
                                          @NotNull CompileContext context) throws ProjectBuildException {
    File outputDirectory = getBuildOutputDirectory(module, forTests, context);
    commandLine.addParameters("-pa", outputDirectory.getPath());
    for (String rootUrl : module.getContentRootsList().getUrls()) {
      try {
        String path = new URL(rootUrl).getPath();
        commandLine.addParameters("-pa", path);
      }
      catch (MalformedURLException e) {
        context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, "Failed to find content root for module: " + module.getName()));
      }
    }
  }

  private static void markDirtyNodes(@NotNull List<Node> dirtyModules,
                                     @NotNull GraphGenerator<Node> graph) {
    for (Node node : dirtyModules) {
      if (node != null) {
        markDirtyNodes(node, graph);
      }
    }
  }

  private static void markDirtyNodes(@NotNull Node node, @NotNull GraphGenerator<Node> graph) {
    if (node.myDirty) return;
    node.myDirty = true;
    Iterator<Node> childIterator = graph.getOut(node);
    while (childIterator.hasNext()) {
      markDirtyNodes(childIterator.next(), graph);
    }
  }

  @NotNull
  private static List<File> getOutputErlangModuleFiles(@NotNull List<String> erlangModulePathsToCompile,
                                                       @NotNull final File outputDirectory) {
    return ContainerUtil.map(erlangModulePathsToCompile, new Function<String, File>() {
      @Override
      public File fun(String filePath) {
        String name = FileUtil.getNameWithoutExtension(filePath);
        return new File(outputDirectory.getAbsolutePath() + name + ".beam");
      }
    });
  }

  private static void consumeFiles(@NotNull BuildOutputConsumer outputConsumer,
                                   @NotNull List<File> dirtyFilePaths) throws IOException {
    for (File outputFile : dirtyFilePaths) {
      if (outputFile.exists()) {
        outputConsumer.registerOutputFile(outputFile, Collections.singletonList(outputFile.getAbsolutePath()));
      }
    }
  }

  private static abstract class DirtyFileProcessor<T> implements FileProcessor<ErlangSourceRootDescriptor, ErlangTarget> {
    private final List<T> myDirtyElements = ContainerUtil.newArrayList();

    public List<T> collectDirtyElements(@NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder) throws IOException {
      holder.processDirtyFiles(this);
      return myDirtyElements;
    }

    @Override
    public boolean apply(ErlangTarget erlangTarget,
                         File file,
                         ErlangSourceRootDescriptor erlangSourceRootDescriptor) throws IOException {
      ContainerUtil.addIfNotNull(myDirtyElements, getDirtyElement(erlangTarget, file, erlangSourceRootDescriptor));
      return true;
    }

    @Nullable
    protected abstract T getDirtyElement(@NotNull ErlangTarget target,
                                         @NotNull File file,
                                         @NotNull ErlangSourceRootDescriptor root) throws IOException;
  }

  private static class Node {
    final String myErlangModulePath;
    final List<Node> myDependencies = ContainerUtil.newArrayList();
    public boolean myDirty = false;

    Node(String nodeName) {
      myErlangModulePath = nodeName;
    }
  }

  private static class SortedModuleDependencyGraph implements GraphGenerator.SemiGraph<Node> {
    private final LinkedHashMap<String, Node> myNodePathsMap;

    public SortedModuleDependencyGraph(List<ErlangFileDescriptor> moduleDescriptors) {
      myNodePathsMap = getPathsMap(moduleDescriptors);
      addDependencies(moduleDescriptors);
    }

    @Override
    public Collection<Node> getNodes() {
      return myNodePathsMap.values();
    }

    @Override
    public Iterator<Node> getIn(Node node) {
      return node.myDependencies.iterator();
    }

    @Nullable
    public Node getNode(@NotNull String moduleName) {
      return myNodePathsMap.get(moduleName);
    }

    private void addDependencies(List<ErlangFileDescriptor> moduleDescriptors) {
      for (ErlangFileDescriptor descriptor : moduleDescriptors) {
        Node node = myNodePathsMap.get(descriptor.myErlangModulePath);
        node.myDependencies.addAll(getNodesByName(descriptor.myDependencies));
      }
    }

    @NotNull
    private List<Node> getNodesByName(@NotNull Collection<String> nodes) {
      return ContainerUtil.mapNotNull(nodes, new Function<String, Node>() {
        @Override
        public Node fun(String dependencyName) {
          return myNodePathsMap.get(dependencyName);
        }
      });
    }

    private static LinkedHashMap<String, Node> getPathsMap(List<ErlangFileDescriptor> moduleDescriptors) {
      LinkedHashMap<String, Node> nodePathsMap = ContainerUtil.newLinkedHashMap();
      for (ErlangFileDescriptor descriptor : moduleDescriptors) {
        nodePathsMap.put(descriptor.myErlangModulePath, new Node(descriptor.myErlangModulePath));
      }
      return nodePathsMap;
    }
  }
}
