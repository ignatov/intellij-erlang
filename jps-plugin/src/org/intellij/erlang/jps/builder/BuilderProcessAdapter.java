/*
 * Copyright 2012-2015 Sergey Ignatov
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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.messages.CompilerMessage;


public class BuilderProcessAdapter extends ProcessAdapter {
  private final CompileContext myContext;
  protected final String myBuilderName;
  protected final String myCompileTargetRootPath;

  protected BuilderProcessAdapter(@NotNull CompileContext context,
                                  @NotNull String builderName,
                                  @NotNull String compileTargetRootPath) {
    myContext = context;
    myBuilderName = builderName;
    myCompileTargetRootPath = compileTargetRootPath;
  }

  protected void showMessage(@NotNull CompilerMessage message) {
    myContext.processMessage(message);
  }
}
