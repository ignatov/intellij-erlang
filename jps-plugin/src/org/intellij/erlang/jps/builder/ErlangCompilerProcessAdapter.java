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

import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.util.Key;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.messages.BuildMessage;
import org.jetbrains.jps.incremental.messages.CompilerMessage;

public class ErlangCompilerProcessAdapter extends BuilderProcessAdapter {
  public ErlangCompilerProcessAdapter(@NotNull CompileContext context,
                                      @NotNull String builderName,
                                      @NotNull String compileTargetRootPath) {
    super(context, builderName, compileTargetRootPath);
  }

  @Override
  public void onTextAvailable(@NotNull ProcessEvent event, @NotNull Key outputType) {
    showMessage(createCompilerMessage(myBuilderName, myCompileTargetRootPath, event.getText()));
  }

  @NotNull
  public static CompilerMessage createCompilerMessage(@NotNull String builderName,
                                                      @NotNull String compileTargetRootPath,
                                                      @NotNull String text) {
    BuildMessage.Kind kind = BuildMessage.Kind.INFO;
    String messageText = text;
    String sourcePath = null;
    long line = -1L;

    ErlangCompilerError error = ErlangCompilerError.create(compileTargetRootPath, text);
    if (error != null) {
      kind = error.getKind();
      messageText = error.getErrorMessage();

      sourcePath = extractPath(error.getUrl());
      line = error.getLine();
    }
    return new CompilerMessage(builderName, kind, messageText, sourcePath, -1L, -1L, -1L, line, -1L);
  }

  private static String extractPath(String url) {
    int index = url.indexOf("://");
    return index >= 0 ? url.substring(index + "://".length()) : url;
  }
}
