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

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.jps.builder.*;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
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

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Paths;
import java.util.Collections;

public class RebarBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  private static final String NAME = "rebar";
  private static final String REBAR_CONFIG_FILE_NAME = "rebar.config";
  private static final String REBAR_CONFIG_SCRIPT_SUFFIX = ".script";

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

    String rebarPath = getRebarExecutablePath(project);
    if (rebarPath == null) {
      String errorMessage = "Rebar path is not set";
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }

    JpsSdk<JpsDummyElement> sdk = ErlangTargetBuilderUtil.getSdk(context, module);
    String escriptPath = JpsErlangSdkType.getScriptInterpreterExecutable(sdk.getHomePath()).getAbsolutePath();
    String customRebarConfig = StringUtil.isEmptyOrSpaces(compilerOptions.myCustomRebarConfig) ? null : compilerOptions.myCustomRebarConfig;
    String customEunitRebarConfig = StringUtil.isEmptyOrSpaces(compilerOptions.myCustomEunitRebarConfig) ? customRebarConfig : compilerOptions.myCustomEunitRebarConfig;
    boolean isRebarRun = false;
    for (String contentRootUrl : module.getContentRootsList().getUrls()) {
      String contentRootPath = getPathFromURL(contentRootUrl);
      if (customRebarConfig == null) {
        if (!isRebarConfigExists(contentRootPath, REBAR_CONFIG_FILE_NAME)) continue;
      }
      runRebar(escriptPath, rebarPath, contentRootPath, customRebarConfig, false, compilerOptions.myAddDebugInfoEnabled, context);
      runRebar(escriptPath, rebarPath, contentRootPath, customEunitRebarConfig, true, true, context);
      isRebarRun = true;
    }
    if (!isRebarRun) {
      String messageText = "Skipped module \'" + module.getName() + "\' because rebar.config is not found.";
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.INFO, messageText));
    }
  }

  @NotNull
  @Override
  public String getPresentableName() {
    return NAME;
  }

  private static void runRebar(@NotNull String escriptPath,
                               @NotNull String rebarPath,
                               @Nullable String contentRootPath,
                               @Nullable String customRebarConfig,
                               boolean isTest,
                               boolean addDebugInfo,
                               @NotNull CompileContext context) throws ProjectBuildException {
    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.withWorkDirectory(contentRootPath);
    commandLine.setExePath(escriptPath);
    commandLine.addParameter(rebarPath);

    if (isTest) {
      commandLine.addParameter("eunit");
      commandLine.addParameter("compile_only=true");
    } else {
      commandLine.addParameter("compile");
    }

    if (customRebarConfig != null) {
      if (!isRebarConfigExists(contentRootPath, customRebarConfig)) {
        throw new ProjectBuildException("Custom rebar config file is not found: '" + customRebarConfig + "'");
      }

      commandLine.addParameter("-C");
      commandLine.addParameter(customRebarConfig);
    }

    if (addDebugInfo) {
      commandLine.getEnvironment().put("ERL_FLAGS", "+debug_info");
    }

    Process process;
    try {
      process = commandLine.createProcess();
    }
    catch (ExecutionException e) {
      throw new ProjectBuildException("Failed to run rebar", e);
    }
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, commandLine.getCommandLineString(), Charset.defaultCharset());
    ProcessAdapter adapter = new RebarProcessAdapter(context, NAME, commandLine.getWorkDirectory().getPath());
    handler.addProcessListener(adapter);
    handler.startNotify();
    handler.waitFor();
    if (process.exitValue() != 0) {
      throw new ProjectBuildException("Rebar process finished with exit code " + process.exitValue());
    }
  }

  private static boolean isRebarConfigExists(@Nullable String contentRootDir, String rebarConfigPath) {
    return isFileExists(contentRootDir, rebarConfigPath) ||
            isFileExists(contentRootDir, rebarConfigPath + REBAR_CONFIG_SCRIPT_SUFFIX);
  }

  private static boolean isFileExists(@Nullable String baseDir, String filePath) {
    File file = new File(filePath);
    File absoluteFile;
    if (baseDir == null || file.isAbsolute()) {
      absoluteFile = file;
    } else {
      absoluteFile = new File(baseDir, filePath);
    }

    return absoluteFile.exists();
  }

  @Nullable
  private static String getPathFromURL(String url) {
    try {
      URI uri = new URL(url).toURI();
      String authority = uri.getAuthority();
      if (!authority.isEmpty()) {
        // Workaround for Windows UNC urls like file://C:/...
        uri = new URI("file:///" + authority + uri.getPath());
      }
      return Paths.get(uri).toString();
    } catch (URISyntaxException | MalformedURLException ignored) {
      return null;
    }
  }

  @Nullable
  private static String getRebarExecutablePath(@Nullable JpsProject project) {
    JpsRebarConfigurationExtension rebarConfigurationExtension = JpsRebarConfigurationExtension.getExtension(project);
    String rebarPath = rebarConfigurationExtension != null ? rebarConfigurationExtension.getRebarPath() : null;
    return StringUtil.isEmptyOrSpaces(rebarPath) ? null : rebarPath;
  }
}
