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

package org.intellij.erlang.jps.rebar;

import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.jps.builder.*;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;
import org.jetbrains.jps.builders.BuildOutputConsumer;
import org.jetbrains.jps.builders.DirtyFilesHolder;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.ProjectBuildException;
import org.jetbrains.jps.incremental.TargetBuilder;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsProject;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.util.JpsPathUtil;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.*;

public class RebarBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  private static final String NAME = "rebar";
  public static final String REBAR_CONFIG_FILE_NAME = "rebar.config";
  private static final String ERL_COMPILER_OPTIONS = "ERL_COMPILER_OPTIONS";
  private static final String TESTS_REBAR_PATH_PROPERTY = "rebar.path";
  private static final List<String> REBAR_WRAPPER_EXTENSIONS = Arrays.asList(".cmd", ".bat", ".exe", ".ps1");

  public RebarBuilder() {
    super(Collections.singleton(ErlangTargetType.INSTANCE));
  }

  @Override
  public void build(@NotNull ErlangTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws ProjectBuildException, IOException {
    if (!holder.hasDirtyFiles() && !holder.hasRemovedFiles()) return;

    JpsModule module = target.getModule();
    JpsProject project = module.getProject();
    ErlangCompilerOptions compilerOptions = ErlangBuilderUtil.getCompilerOptions(project);
    if (!compilerOptions.myUseRebarCompiler) return;

    if (!hasRebarConfig(module)) {
      // ErlangBuilder compiles such modules with erlc instead of skipping them, see #1028.
      String messageText = "rebar.config is not found in module '" + module.getName() + "', so it is compiled with erlc. " +
                           "Add rebar.config to the module content root to compile it with rebar, " +
                           "or turn off 'Compile project with rebar' in the Erlang Compiler settings.";
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.WARNING, messageText));
      return;
    }

    String rebarPath = getRebarExecutablePath(project);
    if (rebarPath == null) {
      String errorMessage = "Rebar path is not set";
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }

    JpsSdk<JpsDummyElement> sdk = ErlangTargetBuilderUtil.getSdk(context, module);
    for (String contentRootUrl : module.getContentRootsList().getUrls()) {
      if (!hasRebarConfig(contentRootUrl)) continue;
      runRebar(sdk.getHomePath(), rebarPath, JpsPathUtil.urlToPath(contentRootUrl), compilerOptions.myAddDebugInfoEnabled, context);
    }
  }

  public static boolean hasRebarConfig(@NotNull JpsModule module) {
    for (String contentRootUrl : module.getContentRootsList().getUrls()) {
      if (hasRebarConfig(contentRootUrl)) return true;
    }
    return false;
  }

  private static boolean hasRebarConfig(@NotNull String contentRootUrl) {
    return new File(JpsPathUtil.urlToFile(contentRootUrl), REBAR_CONFIG_FILE_NAME).isFile();
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  private static void runRebar(@NotNull String sdkHome,
                               @NotNull String rebarPath,
                               @Nullable String contentRootPath,
                               boolean addDebugInfo,
                               @NotNull CompileContext context) throws ProjectBuildException {
    List<String> command = new ArrayList<>();
    Map<String, String> environment = new HashMap<>();
    if (isEscript(rebarPath)) {
      command.add(JpsErlangSdkType.getScriptInterpreterExecutable(sdkHome).getAbsolutePath());
    }
    else {
      environment.put("PATH", getPathWithSdkFirst(sdkHome, System.getenv("PATH")));
    }
    command.add(rebarPath);
    command.add("compile");

    if (addDebugInfo && System.getenv(ERL_COMPILER_OPTIONS) == null) {
      // The compiler reads its default options from ERL_COMPILER_OPTIONS, so this works for rebar and rebar3 and
      // overrides no_debug_info in erl_opts. ERL_FLAGS="+debug_info", used before, is an emulator flag and had no effect.
      environment.put(ERL_COMPILER_OPTIONS, "[debug_info]");
    }

    File workingDirectory = contentRootPath != null ? new File(contentRootPath) : null;
    BuilderProcessAdapter adapter = new RebarProcessAdapter(context, NAME, contentRootPath != null ? contentRootPath : "");
    
    int exitCode = ProcessRunner.runProcess(command, workingDirectory, environment, adapter);
    
    if (exitCode != 0) {
      throw new ProjectBuildException("Rebar process finished with exit code " + exitCode);
    }
  }

  /**
   * The rebar or rebar3 escript for the tests that run a real rebar build, set with -Drebar.path=...; null if unset.
   */
  @TestOnly
  @Nullable
  public static String getTestsRebarPath() {
    return StringUtil.nullize(System.getProperty(TESTS_REBAR_PATH_PROPERTY), true);
  }

  @Nullable
  private static String getRebarExecutablePath(@Nullable JpsProject project) {
    JpsRebarConfigurationExtension rebarConfigurationExtension = JpsRebarConfigurationExtension.getExtension(project);
    String rebarPath = rebarConfigurationExtension != null ? rebarConfigurationExtension.getRebarPath() : null;
    return StringUtil.isEmptyOrSpaces(rebarPath) ? null : getRebarEscriptPath(rebarPath);
  }

  /**
   * The rebar escript to run with escript for the rebar path of the settings. A wrapper such as rebar3.cmd, which the
   * rebar3 docs tell Windows users to create next to the escript, is not an escript, so the escript next to it (the path
   * without the extension) is used instead. Any other path, and a wrapper without such an escript, is returned as is.
   */
  @NotNull
  public static String getRebarEscriptPath(@NotNull String rebarPath) {
    String lowerCasePath = rebarPath.toLowerCase(Locale.ROOT);
    for (String extension : REBAR_WRAPPER_EXTENSIONS) {
      if (lowerCasePath.endsWith(extension)) {
        File escript = new File(rebarPath.substring(0, rebarPath.length() - extension.length()));
        return escript.isFile() ? escript.getPath() : rebarPath;
      }
    }
    return rebarPath;
  }

  /**
   * Whether the rebar path is run with the escript of the Erlang SDK. A script for another interpreter, whose first line
   * is a shebang that does not name escript, such as the sh script that `rebar3 local install` writes, is run directly
   * instead, with {@link #getPathWithSdkFirst} as PATH, so that the erl it runs is the SDK's.
   */
  public static boolean isEscript(@NotNull String rebarPath) {
    try (BufferedReader reader = Files.newBufferedReader(Paths.get(rebarPath), StandardCharsets.ISO_8859_1)) {
      String firstLine = reader.readLine();
      return firstLine == null || !firstLine.startsWith("#!") || firstLine.contains("escript");
    }
    catch (IOException | InvalidPathException e) {
      return true;
    }
  }

  /**
   * The PATH with the bin directory of the Erlang SDK first.
   */
  @NotNull
  public static String getPathWithSdkFirst(@NotNull String sdkHome, @Nullable String path) {
    String sdkBin = JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkHome).getParent();
    return StringUtil.isEmpty(path) ? sdkBin : sdkBin + File.pathSeparator + path;
  }
}
