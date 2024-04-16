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
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.CommonProcessors;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.incremental.resources.ResourcesBuilder;
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

import static org.intellij.erlang.jps.builder.ErlangBuilderUtil.LOG;
import static org.intellij.erlang.jps.builder.ErlangBuilderUtil.isSource;

public class ErlangBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  public static final String NAME = "erlc";

  public ErlangBuilder() {
    super(Collections.singletonList(ErlangTargetType.INSTANCE));

    //TODO provide a way to copy erlang resources
    //disables java resource builder for erlang modules
    ResourcesBuilder.registerEnabler(module -> !(module.getModuleType() instanceof JpsErlangModuleType));
  }

  @Override
  public void build(@NotNull ErlangTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws ProjectBuildException, IOException {

    JpsModule module = target.getModule();
    JpsProject project = module.getProject();
    ErlangCompilerOptions compilerOptions = ErlangBuilderUtil.getCompilerOptions(project);
    if (compilerOptions.myUseRebarCompiler) return;

    LOG.info("Build module " + target.getPresentableName());
    File sourceOutput = getBuildOutputDirectory(module, false, context);
    File testOutput = getBuildOutputDirectory(module, true, context);

    buildSources(target, context, compilerOptions, outputConsumer, sourceOutput, false);
    buildSources(target, context, compilerOptions, outputConsumer, testOutput, true);

    processAppConfigFiles(holder, outputConsumer, context, sourceOutput, testOutput);
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  private static void buildSources(@NotNull ErlangTarget target,
                                   @NotNull CompileContext context,
                                   @NotNull ErlangCompilerOptions compilerOptions,
                                   @NotNull BuildOutputConsumer outputConsumer,
                                   @NotNull File outputDir,
                                   final boolean isTests) throws IOException, ProjectBuildException {
    List<String> erlangModulePathsToCompile = getErlangModulePaths(target, context, isTests);
    if (erlangModulePathsToCompile.isEmpty()) {
      String message = isTests ? "Test sources is up to date for module" : "Sources is up to date for module";
      reportMessageForModule(context, message, target.getModule().getName());
      return;
    }
    String message = isTests ? "Compile tests for module" : "Compile source code for module";
    reportMessageForModule(context, message, target.getModule().getName());
    runErlc(target, context, compilerOptions, erlangModulePathsToCompile, outputConsumer, outputDir, isTests);
  }

  private static void reportMessageForModule(@NotNull CompileContext context,
                                             @NotNull String messagePrefix,
                                             @NotNull String moduleName) {
    String message = messagePrefix + " \"" + moduleName + "\".";
    reportMessage(context, message);
  }

  private static void reportMessage(@NotNull CompileContext context, @NotNull String message) {
    LOG.info(message);
    context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.PROGRESS, message));
    context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.INFO, message));
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
    List<File> appConfigFiles = new DirtyFilesProcessor<File, ErlangTarget>() {
      @Nullable
      @Override
      protected File getDirtyElement(@NotNull File file) {
        return ErlangBuilderUtil.isAppConfigFileName(file.getName()) ? file : null;
      }
    }.collectDirtyElements(holder);

    for (File appConfigSrc : appConfigFiles) {
      for (File outputDir : outputDirectories) {
        File appConfigDst = getDestinationAppConfig(outputDir, appConfigSrc.getName());
        LOG.warn("Using AppFileConverter to generate app file");
        new AppFileConverter(context, appConfigSrc, outputDir).writeToFile(appConfigDst);
        reportMessage(context, String.format("Copy AppFileConverter %s to %s", ErlangBuilderUtil.getPath(appConfigSrc), ErlangBuilderUtil.getPath(outputDir)));
        outputConsumer.registerOutputFile(appConfigDst, Collections.singletonList(ErlangBuilderUtil.getPath(appConfigSrc)));
      }
    }
  }

  @NotNull
  private static File getDestinationAppConfig(File outputDir, @NotNull String fileName) {
    return new File(outputDir, getAppConfigDestinationFileName(fileName));
  }

  @NotNull
  private static String getAppConfigDestinationFileName(@NotNull String sourceFileName) {
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
    LOG.debug("Run erlc compiler with command " + commandLine.getCommandLineString());
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
    registerBeams(outputConsumer, erlangModulePathsToCompile, outputDirectory);
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
    commandLine.addParameters(compilerOptions.myAdditionalErlcArguments);
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
                                                   boolean isTest) {
    List<String> erlangDirtyModules = getErlangModulePathsFromTarget(target, isTest);
    if (erlangDirtyModules != null) {
      return erlangDirtyModules;
    }

    String message = "Erlang module " + target.getModule().getName() + " will be fully rebuilt.";
    LOG.warn(message);
    context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.WARNING, message));
    return getErlangModulePathsDefault(target, isTest);
  }

  @Nullable
  private static List<String> getErlangModulePathsFromTarget(@NotNull ErlangTarget target, boolean isTests) {
    ErlangModuleBuildOrder buildOrder = target.getBuildOrder();
    if (buildOrder == null) return null;

    List<String> modules = buildOrder.myOrderedErlangFilePaths;
    return isTests ? ContainerUtil.concat(modules, buildOrder.myOrderedErlangTestFilePaths) : modules;
  }

  @NotNull
  private static List<String> getErlangModulePathsDefault(@NotNull ErlangTarget target, boolean isTests) {
    CommonProcessors.CollectProcessor<File> erlFilesCollector = new CommonProcessors.CollectProcessor<>() {
      @Override
      protected boolean accept(@NotNull File file) {
        return !file.isDirectory() && isSource(file.getName());
      }
    };
    List<JpsModuleSourceRoot> sourceRoots = new SmartList<>();
    JpsModule module = target.getModule();
    ContainerUtil.addAll(sourceRoots, module.getSourceRoots(JavaSourceRootType.SOURCE));
    if (isTests) {
      ContainerUtil.addAll(sourceRoots, module.getSourceRoots(JavaSourceRootType.TEST_SOURCE));
    }
    for (JpsModuleSourceRoot root : sourceRoots) {
      FileUtil.processFilesRecursively(root.getFile(), erlFilesCollector);
    }
    return ContainerUtil.map(erlFilesCollector.getResults(), ErlangBuilderUtil::getPath);
  }

  private static void addParseTransforms(@NotNull GeneralCommandLine commandLine,
                                         @Nullable JpsModule module) {
    JpsErlangModuleExtension extension = JpsErlangModuleExtension.getExtension(module);
    List<String> parseTransforms = extension != null ? extension.getParseTransforms() : Collections.emptyList();
    List<String> flags = extension != null ? extension.getExtraFlags() : Collections.emptyList();
    if (parseTransforms.isEmpty()) return;
    for (String ptModule : parseTransforms) {
      commandLine.addParameter("+{parse_transform, " + ptModule + "}");
    }
    for (String ptModule : flags) {
      commandLine.addParameter(ptModule);
    }
  }

  private static void addCodePath(@NotNull GeneralCommandLine commandLine,
                                  @NotNull JpsModule module,
                                  @NotNull ErlangTarget target,
                                  @NotNull CompileContext context) throws ProjectBuildException {
    List<JpsModule> codePathModules = new SmartList<>();
    collectDependentModules(module, codePathModules, new HashSet<>());
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
      if (!(dependency instanceof JpsModuleDependency moduleDependency)) continue;
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

  private static void registerBeams(@NotNull BuildOutputConsumer outputConsumer,
                                    @NotNull List<String> erlPaths,
                                    @NotNull File outputDir) throws IOException {
    for (String erlPath : erlPaths) {
      File beam = getBeam(outputDir, erlPath);
      if (beam.exists()) {
        outputConsumer.registerOutputFile(beam, ContainerUtil.createMaybeSingletonList(erlPath));
      }
    }
  }

  @NotNull
  private static File getBeam(@NotNull File outputDirectory, @NotNull String erlPath) {
    String name = FileUtil.getNameWithoutExtension(new File(erlPath));
    return new File(outputDirectory, name + ".beam");
  }
}