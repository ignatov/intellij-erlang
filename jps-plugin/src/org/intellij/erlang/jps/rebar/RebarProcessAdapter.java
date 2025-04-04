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

package org.intellij.erlang.jps.rebar;

import com.intellij.execution.process.ProcessEvent;
import com.intellij.openapi.util.Key;
import org.intellij.erlang.jps.builder.BuilderProcessAdapter;
import org.intellij.erlang.jps.builder.ErlangCompilerProcessAdapter;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.incremental.messages.CompilerMessage;

public class RebarProcessAdapter extends BuilderProcessAdapter {
  private final StringBuilder myMessageBuilder = new StringBuilder();

  public RebarProcessAdapter(@NotNull CompileContext context,
                             @NotNull String builderName,
                             @NotNull String compileTargetRootPath) {
    super(context, builderName, compileTargetRootPath);
  }

  @Override
  public void onTextAvailable(@NotNull ProcessEvent event, @NotNull Key outputType) {
    addToProcessing(event.getText());
  }

  @Override
  public void processTerminated(@NotNull ProcessEvent event) {
    super.processTerminated(event);
    if (myMessageBuilder.length() > 0) {
      processMessage(myMessageBuilder.toString());
    }
  }

  private void processMessage(@NotNull String message) {
    showMessage(createCompilerMessage(message));
  }

  @NotNull
  private CompilerMessage createCompilerMessage(@NotNull String messageText) {
    RebarMessage message = RebarMessage.create(messageText);
    return message != null ? new CompilerMessage(myBuilderName, message.getKind(), message.getDetails())
                           : getDefaultCompilerMessage(messageText);
  }

  @NotNull
  private CompilerMessage getDefaultCompilerMessage(@NotNull String messageText) {
    return ErlangCompilerProcessAdapter.createCompilerMessage(myBuilderName, myCompileTargetRootPath, messageText);
  }

  private void addToProcessing(@NotNull String messagePart) {
    if (!isMessageContinue(messagePart) && myMessageBuilder.length() > 0) {
      processMessage(myMessageBuilder.toString());
      myMessageBuilder.setLength(0);
    }
    myMessageBuilder.append(messagePart.trim());
  }


  private static boolean isMessageContinue(@NotNull String messagePart) {
    return messagePart.isEmpty() || Character.isWhitespace(messagePart.charAt(0));
  }
}
