/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.statistics;

import com.intellij.openapi.actionSystem.*;
import com.intellij.openapi.actionSystem.ex.AnActionListener;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebuggerManager;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.debugger.xdebug.ErlangXDebugProcess;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Set;

import static java.util.Map.entry;
import static org.intellij.erlang.statistics.ErlangUsageKey.*;

/**
 * Counts the IDE actions performed on an Erlang file, by action id. Only the action id is looked at, never the file.
 */
public final class ErlangUsageActionListener implements AnActionListener {
  static final Map<String, ErlangUsageKey> ERLANG_FILE_ACTIONS = Map.ofEntries(
    entry("GotoDeclaration", GO_TO_DECLARATION),
    entry("GotoDeclarationOnly", GO_TO_DECLARATION),
    entry("FindUsages", FIND_USAGES),
    entry("ShowUsages", FIND_USAGES),
    entry("GotoImplementation", GO_TO_IMPLEMENTATION),
    entry("GotoSuperMethod", GO_TO_SUPER),
    entry("FileStructurePopup", FILE_STRUCTURE_POPUP),
    entry("QuickJavaDoc", QUICK_DOCUMENTATION),
    entry("ParameterInfo", PARAMETER_INFO),
    entry("ReformatCode", REFORMAT),
    entry("org.intellij.erlang.actions.ErlangEmacsFormatAction", EMACS_FORMAT),
    entry("RenameElement", RENAME),
    entry("Inline", INLINE),
    entry("IntroduceVariable", INTRODUCE_VARIABLE),
    entry("ExtractMethod", EXTRACT_FUNCTION),
    entry("SafeDelete", SAFE_DELETE),
    entry("Erlang.SendSelectionToConsole", CONSOLE_SEND_SELECTION),
    entry("EUnitGenerateTestMethod", EUNIT_GENERATE_TEST)
  );
  private static final Set<String> EVALUATE_ACTIONS = Set.of("EvaluateExpression", "QuickEvaluateExpression");

  @Override
  public void afterActionPerformed(@NotNull AnAction action, @NotNull AnActionEvent event, @NotNull AnActionResult result) {
    if (!result.isPerformed() || !ErlangUsageCounters.isCounting()) return;
    String id = ActionManager.getInstance().getId(action);
    if (id == null) return;
    ErlangUsageKey key = ERLANG_FILE_ACTIONS.get(id);
    if (key != null) {
      if (isErlangFile(event.getData(CommonDataKeys.PSI_FILE), event.getData(CommonDataKeys.VIRTUAL_FILE))) {
        ErlangUsageCounters.count(key);
      }
    }
    else if (EVALUATE_ACTIONS.contains(id) && isErlangDebugSession(event.getProject())) {
      ErlangUsageCounters.count(EVALUATE_EXPRESSION);
    }
  }

  static boolean isErlangFile(@Nullable PsiFile psiFile, @Nullable VirtualFile file) {
    if (psiFile != null) return psiFile instanceof ErlangFile;
    return file != null && file.getFileType() instanceof ErlangFileType;
  }

  private static boolean isErlangDebugSession(@Nullable Project project) {
    if (project == null || project.isDisposed()) return false;
    XDebugSession session = XDebuggerManager.getInstance(project).getCurrentSession();
    return session != null && session.getDebugProcess() instanceof ErlangXDebugProcess;
  }
}
