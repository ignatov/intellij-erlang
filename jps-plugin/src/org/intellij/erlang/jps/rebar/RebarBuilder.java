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

import com.intellij.execution.process.BaseOSProcessHandler;
import com.intellij.execution.process.ProcessAdapter;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.jps.builder.*;
import org.intellij.erlang.jps.execution.ExecutionException;
import org.intellij.erlang.jps.execution.GeneralCommandLine;
import org.intellij.erlang.jps.model.ErlangCompilerOptions;
import org.intellij.erlang.jps.model.JpsErlangCompilerOptionsExtension;
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
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Collections;

public class RebarBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  private static final String NAME = "rebar";
  private static final String REBAR_CONFIG_FILE_NAME = "rebar.config";
  public RebarBuilder() {
    super(Collections.singleton(ErlangTargetType.PRODUCTION));
  }

  @Override
  public void build(@NotNull ErlangTarget target,
                    @NotNull DirtyFilesHolder<ErlangSourceRootDescriptor, ErlangTarget> holder,
                    @NotNull BuildOutputConsumer outputConsumer,
                    @NotNull CompileContext context) throws ProjectBuildException, IOException {
    if (!holder.hasDirtyFiles() && !holder.hasRemovedFiles()) return;

    JpsModule module = target.getModule();
    JpsProject project = module.getProject();
    ErlangCompilerOptions compilerOptions = JpsErlangCompilerOptionsExtension.getOrCreateExtension(project).getOptions();
    if (!compilerOptions.myUseRebarCompiler) return;

    String rebarPath = getRebarExecutablePath(project);
    if (rebarPath == null) {
      String errorMessage = "Rebar path is not set";
      context.processMessage(new CompilerMessage(NAME, BuildMessage.Kind.ERROR, errorMessage));
      throw new ProjectBuildException(errorMessage);
    }

    JpsSdk<JpsDummyElement> sdk = ErlangTargetBuilderUtil.getSdk(context, module);
    String escriptPath = JpsErlangSdkType.getScriptInterpreterExecutable(sdk.getHomePath()).getAbsolutePath();

    for (String contentRootUrl : module.getContentRootsList().getUrls()) {
      String contentRootPath = new URL(contentRootUrl).getPath();
      File contentRootDir = new File(contentRootPath);
      File rebarConfigFile = new File(contentRootDir, REBAR_CONFIG_FILE_NAME);
      if (!rebarConfigFile.exists()) continue;
      runRebar(escriptPath, rebarPath, contentRootPath, compilerOptions.myAddDebugInfoEnabled, context);
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
                               boolean addDebugInfo,
                               @NotNull CompileContext context) throws ProjectBuildException {
    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setWorkDirectory(contentRootPath);
    commandLine.setExePath(escriptPath);
    commandLine.addParameter(rebarPath);
    commandLine.addParameter("compile");

    if (addDebugInfo) {
      commandLine.getEnvironment().put("ERL_FLAGS", "+debug_info");
    }

    Process process;
    try {
      process = commandLine.createProcess();
    } catch (ExecutionException e) {
      throw new ProjectBuildException("Failed to run rebar", e);
    }
    BaseOSProcessHandler handler = new BaseOSProcessHandler(process, commandLine.getCommandLineString(), Charset.defaultCharset());
    ProcessAdapter adapter = new ErlangCompilerProcessAdapter(context, NAME, commandLine.getWorkDirectory().getPath()); //TODO provide rebar messages handling
    handler.addProcessListener(adapter);
    handler.startNotify();
    handler.waitFor();
  }

  @Nullable
  private static String getRebarExecutablePath(@Nullable JpsProject project) {
    JpsRebarConfigurationExtension rebarConfigurationExtension = JpsRebarConfigurationExtension.getExtension(project);
    String rebarPath = rebarConfigurationExtension != null ? rebarConfigurationExtension.getRebarPath() : null;
    return StringUtil.isEmptyOrSpaces(rebarPath) ? null : rebarPath;
  }
}
