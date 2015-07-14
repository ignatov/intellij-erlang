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
  private final String myBuilderName;
  private final String myCompileTargetRootPath;

  public ErlangCompilerProcessAdapter(@NotNull CompileContext context, @NotNull String builderName, @NotNull String compileTargetRootPath) {
    myContext = context;
    myBuilderName = builderName;
    myCompileTargetRootPath = compileTargetRootPath;
  }

  @Override
  public void onTextAvailable(@NotNull ProcessEvent event, Key outputType) {
    ErlangCompilerError error = ErlangCompilerError.create(myCompileTargetRootPath, event.getText());
    if (error != null) {
      boolean isError = error.getCategory() == CompilerMessageCategory.ERROR;
      BuildMessage.Kind kind = isError ? BuildMessage.Kind.ERROR : BuildMessage.Kind.WARNING;
      CompilerMessage msg = new CompilerMessage(myBuilderName, kind, error.getErrorMessage(),
        VirtualFileManager.extractPath(error.getUrl()), -1, -1, -1, error.getLine(), -1);
      myContext.processMessage(msg);
    }else{
      myContext.processMessage(new CompilerMessage(myBuilderName, BuildMessage.Kind.INFO,event.getText()));
    }
  }
}
