/*
 * Copyright 2012 Sergey Ignatov
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

/*
 * Copyright 2012 Sergey Ignatov
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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.compiler.TranslatingCompiler;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.util.Chunk;
import com.intellij.util.PathUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.charset.Charset;
import java.util.List;

/**
 * @author ignatov
 */
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

    final GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.setWorkDirectory(PathUtil.getParentPath(context.getProject().getProjectFilePath()));
    commandLine.setPassParentEnvs(true);

    for (Module module : moduleChunk.getNodes()) {
//      commandLine.addParameter("-I../include");
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

      final ModuleRootManager moduleRootManager = ModuleRootManager.getInstance(module);
      final Sdk sdk = moduleRootManager.getSdk();

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

      String erlc = FileUtil.toSystemDependentName(ErlangSdkType.getByteCodeCompilerExecutable(sdkHomePath).getAbsolutePath());

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

  public static void fillContext(Module module, CompileContext context, List<String> errors) {
    for (String error : errors) {
      addErrorToContext(module, error, context);
    }
  }

  private static void addErrorToContext(Module module, String error, CompileContext context) {

    final ErlangCompilerError compilerError = ErlangCompilerError.create(PathUtil.getParentPath(module.getModuleFilePath()), error);
    if (compilerError == null) {
      context.addMessage(CompilerMessageCategory.ERROR, error, null, -1, -1);
      return;
    }

    context.addMessage(compilerError.getCategory(), compilerError.getErrorMessage(), compilerError.getUrl(), compilerError.getLine(), -1);
  }
}

class ErlangCompilerError {
  private final String errorMessage;
  private final String url;
  private final int line;
  private final CompilerMessageCategory category;

  public ErlangCompilerError(String errorMessage, String url, int line, CompilerMessageCategory category) {
    this.errorMessage = errorMessage;
    this.url = url;
    this.line = line;
    this.category = category;
  }

  public String getErrorMessage() {
    return errorMessage;
  }

  public String getUrl() {
    return url;
  }

  public int getLine() {
    return line;
  }

  @Nullable
  public static ErlangCompilerError create(String rootPath, final String erlcMessage) {
    List<String> split = StringUtil.split(erlcMessage, ":");

    // a small hack for such messages: c:\User\test.erl:7:Warning:Message
    if (SystemInfo.isWindows) {
      String combine = split.get(0) + ":" + split.get(1);
      split.remove(0);
      split.remove(0);
      split.add(0, combine);
    }

    if (split.size() < 3) return null;

    String path = split.get(0);
    String line = split.get(1);

    String url = FileUtil.toSystemIndependentName(path);
    if (!StringUtil.startsWithIgnoreCase(path, rootPath)) {
      url = rootPath + "/" + url;
    }
    url = VfsUtil.pathToUrl(url);
    if (!ApplicationManager.getApplication().isUnitTestMode() && VirtualFileManager.getInstance().findFileByUrl(url) == null) {
      return null;
    }

    boolean warning = StringUtil.equalsIgnoreCase(split.get(2).trim(), "warning");
    String messageForUser = StringUtil.replaceIgnoreCase(erlcMessage, path + ":" + line + (warning ? ": warning: " : ": "), "");
    return new ErlangCompilerError(messageForUser, url, StringUtil.parseInt(split.get(1), -1),
      warning ? CompilerMessageCategory.WARNING : CompilerMessageCategory.ERROR);
  }

  public CompilerMessageCategory getCategory() {
    return category;
  }
}
