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

package org.intellij.erlang.console;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.Set;

final class ErlangConsoleViewDirectory {
  private static final ErlangConsoleViewDirectory outInstance = new ErlangConsoleViewDirectory();

  private Set<ErlangConsoleView> consoleViews = new HashSet<ErlangConsoleView>();

  private ErlangConsoleViewDirectory() {
  }

  public static ErlangConsoleViewDirectory getInstance() {
    return outInstance;
  }

  public synchronized void addConsole(@NotNull ErlangConsoleView console) {
    consoleViews.add(console);
  }

  public synchronized void delConsole(@NotNull ErlangConsoleView console) {
    consoleViews.remove(console);
  }

  @Nullable
  public synchronized ErlangConsoleView getConsole(@NotNull Editor editor) {
    for (ErlangConsoleView consoleView : consoleViews) {
      if (editor == consoleView.getConsole().getConsoleEditor()) {
        return consoleView;
      }
    }
    return null;
  }

  @Nullable
  public synchronized ErlangConsoleView getConsole(@NotNull Project project) {
    for (ErlangConsoleView consoleView : consoleViews) {
      if (project == consoleView.getProject()) {
        return consoleView;
      }
    }
    return null;
  }
}
