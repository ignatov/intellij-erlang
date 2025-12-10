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

import java.io.File;
import java.io.IOException;
import java.util.*;

public class RebarBuilder extends TargetBuilder<ErlangSourceRootDescriptor, ErlangTarget> {
  private static final String NAME = "rebar";
  private static final String REBAR_CONFIG_FILE_NAME = "rebar.config";

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
    boolean isRebarRun = false;
    for (String contentRootUrl : module.getContentRootsList().getUrls()) {
      String contentRootPath = JpsPathUtil.urlToPath(contentRootUrl);
      File contentRootDir = JpsPathUtil.urlToFile(contentRootUrl);
      File rebarConfigFile = new File(contentRootDir, REBAR_CONFIG_FILE_NAME);
      if (!rebarConfigFile.exists()) continue;
      runRebar(escriptPath, rebarPath, contentRootPath, compilerOptions.myAddDebugInfoEnabled, context);
      isRebarRun = true;
    }
    if (!isRebarRun) {
      String messageText = "Skipped module '" + module.getName() + "' because rebar.config is not found.";
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
                               boolean addDebugInfo,
                               @NotNull CompileContext context) throws ProjectBuildException {
    List<String> command = new ArrayList<>();
    command.add(escriptPath);
    command.add(rebarPath);
    command.add("compile");

    Map<String, String> environment = null;
    if (addDebugInfo) {
      environment = new HashMap<>();
      environment.put("ERL_FLAGS", "+debug_info");
    }

    File workingDirectory = contentRootPath != null ? new File(contentRootPath) : null;
    BuilderProcessAdapter adapter = new RebarProcessAdapter(context, NAME, contentRootPath != null ? contentRootPath : "");
    
    int exitCode = ProcessRunner.runProcess(command, workingDirectory, environment, adapter);
    
    if (exitCode != 0) {
      throw new ProjectBuildException("Rebar process finished with exit code " + exitCode);
    }
  }

  @Nullable
  private static String getRebarExecutablePath(@Nullable JpsProject project) {
    JpsRebarConfigurationExtension rebarConfigurationExtension = JpsRebarConfigurationExtension.getExtension(project);
    String rebarPath = rebarConfigurationExtension != null ? rebarConfigurationExtension.getRebarPath() : null;
    return StringUtil.isEmptyOrSpaces(rebarPath) ? null : rebarPath;
  }
}
