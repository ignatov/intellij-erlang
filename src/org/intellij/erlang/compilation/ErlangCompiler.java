/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.compiler.TranslatingCompiler;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Chunk;
import com.intellij.util.PathUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.jps.builder.ErlangCompilerError;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.nio.charset.Charset;
import java.util.List;

public class ErlangCompiler implements TranslatingCompiler {
  @NotNull
  @Override
  public String getDescription() {
    return "Erlang compiler";
  }

  @Override
  public boolean validateConfiguration(CompileScope compileScope) {
    return true;
  }

  @Override
  public boolean isCompilableFile(VirtualFile file, CompileContext context) {
    String extension = file.getExtension();
    if (extension == null) return false;
    return extension.equals("erl") && file.getFileType() == ErlangFileType.MODULE;
  }

  @Override
  public void compile(CompileContext context, Chunk<Module> moduleChunk, VirtualFile[] files, OutputSink outputSink) {
    context.getProgressIndicator().pushState();
    context.getProgressIndicator().setText("Hardcore compile action...");

    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setWorkDirectory(PathUtil.getParentPath(context.getProject().getProjectFilePath()));
    commandLine.setPassParentEnvironment(true);

    for (Module module : moduleChunk.getNodes()) {
      ModuleRootManager moduleRootManager = ModuleRootManager.getInstance(module);

      for (VirtualFile sourceRoot : moduleRootManager.getSourceRoots()) {
        commandLine.addParameter("-I");
        String path = sourceRoot.getCanonicalPath();
        if (path != null) {
          commandLine.addParameter(path);
        }
      }

      VirtualFile outDir = context.getModuleOutputDirectory(module);
      if (outDir == null) {
        context.addMessage(CompilerMessageCategory.ERROR, "No output dir for module: " + module.getName(), null, -1, -1);
        return;
      }
      commandLine.setWorkDirectory(outDir.getCanonicalPath());

      for (VirtualFile o : files) {
        String canonicalPath = o.getCanonicalPath();
        if (canonicalPath == null) continue;
        commandLine.addParameter(canonicalPath);
      }

//      commandLine.addParameters("+warn_unused_vars", "+nowarn_shadow_vars", "+warn_unused_import");

      Sdk sdk = moduleRootManager.getSdk();

      if (sdk == null) {
        context.addMessage(CompilerMessageCategory.ERROR, "No SDK for module: " + module.getName(), null, -1, -1);
        return;
      }

      if (sdk.getSdkType() != ErlangSdkType.getInstance()) {
        context.addMessage(CompilerMessageCategory.ERROR, "Not a Erlang SDK for module: " + module.getName(), null, -1, -1);
        return;
      }

      String sdkHomePath = sdk.getHomePath();

      if (sdkHomePath == null) {
        context.addMessage(CompilerMessageCategory.ERROR, "No home path for Erlang SDK: " + sdk.getName(), null, -1, -1);
        return;
      }

      String erlc = FileUtil.toSystemDependentName(JpsErlangSdkType.getByteCodeCompilerExecutable(sdkHomePath).getAbsolutePath());

      commandLine.setExePath(erlc);

//      System.out.println(commandLine.getCommandLineString());

      ProcessOutput output = null;
      try {
        output = new CapturingProcessHandler(commandLine.createProcess(), Charset.defaultCharset(), commandLine.getCommandLineString()).runProcess();
      } catch (ExecutionException e) {
        context.addMessage(CompilerMessageCategory.ERROR, "process throw exception: " + e.getMessage(), null, -1, -1);
      }

      if (output != null) {
        fillContext(module, context, output.getStdoutLines());
      }
    }
  }

  private static void fillContext(Module module, CompileContext context, List<String> errors) {
    for (String error : errors) {
      addErrorToContext(module, error, context);
    }
  }

  private static void addErrorToContext(Module module, String error, CompileContext context) {

    ErlangCompilerError compilerError = ErlangCompilerError.create(PathUtil.getParentPath(module.getModuleFilePath()), error);
    if (compilerError == null) {
      context.addMessage(CompilerMessageCategory.ERROR, error, null, -1, -1);
      return;
    }

    context.addMessage(compilerError.getCategory(), compilerError.getErrorMessage(), compilerError.getUrl(), compilerError.getLine(), -1);
  }
}

