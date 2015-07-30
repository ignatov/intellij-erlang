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

import com.intellij.execution.process.ProcessAdapter;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFileManager;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;

public class ErlangCompilerProcessAdapter extends ProcessAdapter {
  private final CompileContext myContext;
  protected final String myBuilderName;
  private final String myCompileTargetRootPath;

  public ErlangCompilerProcessAdapter(@NotNull CompileContext context,
                                      @NotNull String builderName,
                                      @NotNull String compileTargetRootPath) {
    myContext = context;
    myBuilderName = builderName;
    myCompileTargetRootPath = compileTargetRootPath;
  }

  @Override
  public void onTextAvailable(ProcessEvent event, Key outputType) {
    showMessage(createCompilerMessage(event.getText()));
  }

  protected void showMessage(@NotNull CompilerMessage message) {
    myContext.processMessage(message);
  }

  @NotNull
  protected CompilerMessage createCompilerMessage(String text) {
    BuildMessage.Kind kind = BuildMessage.Kind.INFO;
    String messageText = text;
    String sourcePath = null;
    long line = -1L;

    ErlangCompilerError error = ErlangCompilerError.create(myCompileTargetRootPath, text);
    if (error != null) {
      boolean isError = error.getCategory() == CompilerMessageCategory.ERROR;
      kind = isError ? BuildMessage.Kind.ERROR : BuildMessage.Kind.WARNING;
      messageText = error.getErrorMessage();
      sourcePath = VirtualFileManager.extractPath(error.getUrl());
      line = error.getLine();
    }

    return new CompilerMessage(myBuilderName, kind, messageText, sourcePath, -1L, -1L, -1L, line, -1L);
  }
}
