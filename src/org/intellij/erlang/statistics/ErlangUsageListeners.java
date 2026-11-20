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

import com.intellij.codeInsight.lookup.*;
import com.intellij.codeInsight.template.TemplateManagerListener;
import com.intellij.codeInsight.template.impl.TemplateImpl;
import com.intellij.codeInsight.template.impl.TemplateState;
import com.intellij.execution.ExecutionListener;
import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.executors.DefaultDebugExecutor;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.ProjectActivity;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowId;
import com.intellij.openapi.wm.ex.ToolWindowManagerListener;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.task.ProjectTaskListener;
import com.intellij.task.ProjectTaskManager;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebuggerManagerListener;
import kotlin.Unit;
import kotlin.coroutines.Continuation;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.application.ErlangApplicationConfiguration;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.console.ErlangConsoleRunConfiguration;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.debugger.xdebug.ErlangXDebugProcess;
import org.intellij.erlang.eunit.ErlangUnitRunConfiguration;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.rebar.runner.RebarEunitRunConfiguration;
import org.intellij.erlang.rebar.runner.RebarRunConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.HashSet;
import java.util.Set;

import static org.intellij.erlang.statistics.ErlangUsageKey.*;

/**
 * The platform listeners of the usage statistics. Each one maps an event to an {@link ErlangUsageKey} and drops the rest.
 */
public final class ErlangUsageListeners {
  private ErlangUsageListeners() {
  }

  public static final class Startup implements ProjectActivity {
    @Override
    public @Nullable Object execute(@NotNull Project project, @NotNull Continuation<? super Unit> continuation) {
      ErlangUsageStatistics.getInstance().projectOpened(project);
      return Unit.INSTANCE;
    }
  }

  public static final class Completion implements LookupManagerListener {
    @Override
    public void activeLookupChanged(@Nullable Lookup oldLookup, @Nullable Lookup newLookup) {
      if (newLookup == null || !ErlangUsageCounters.isCounting() || !(newLookup.getPsiFile() instanceof ErlangFile)) return;
      newLookup.addLookupListener(new LookupListener() {
        @Override
        public void itemSelected(@NotNull LookupEvent event) {
          LookupElement item = event.getItem();
          if (item == null) return;
          ErlangUsageCounters.count(COMPLETION);
          ErlangUsageCounters.count(completionKind(item));
        }
      });
    }
  }

  private static final Set<String> KEYWORDS = new HashSet<>();

  static {
    for (IElementType type : ErlangLexer.KEYWORDS.getTypes()) {
      KEYWORDS.add(type.toString());
    }
  }

  @NotNull
  static ErlangUsageKey completionKind(@NotNull LookupElement item) {
    PsiElement element = item.getPsiElement();
    if (element instanceof ErlangFunction) return COMPLETION_FUNCTION;
    if (element instanceof ErlangModule || element instanceof ErlangFile) return COMPLETION_MODULE;
    if (element instanceof ErlangRecordDefinition) return COMPLETION_RECORD;
    if (element instanceof ErlangMacrosDefinition) return COMPLETION_MACRO;
    if (element instanceof ErlangQVar) return COMPLETION_VARIABLE;
    if (element instanceof ErlangTypeDefinition) return COMPLETION_TYPE;
    if (KEYWORDS.contains(item.getLookupString())) return COMPLETION_KEYWORD;
    // many items are plain strings; their icon tells what they are
    LookupElementPresentation presentation = LookupElementPresentation.renderElement(item);
    Icon icon = presentation.getIcon();
    if (icon == ErlangIcons.FUNCTION) return COMPLETION_FUNCTION;
    if (icon == ErlangIcons.MODULE || icon == ErlangIcons.FILE) return COMPLETION_MODULE;
    if (icon == ErlangIcons.RECORD) return COMPLETION_RECORD;
    if (icon == ErlangIcons.MACROS) return COMPLETION_MACRO;
    if (icon == ErlangIcons.VARIABLE) return COMPLETION_VARIABLE;
    if (icon == ErlangIcons.TYPE) return COMPLETION_TYPE;
    if (icon == null && presentation.isItemTextBold()) return COMPLETION_KEYWORD;
    return COMPLETION_OTHER;
  }

  public static final class Execution implements ExecutionListener {
    @Override
    public void processStarted(@NotNull String executorId, @NotNull ExecutionEnvironment env, @NotNull ProcessHandler handler) {
      ErlangUsageKey key = runKey(executorId, env.getRunProfile());
      if (key != null) ErlangUsageCounters.count(key);
    }
  }

  @Nullable
  static ErlangUsageKey runKey(@NotNull String executorId, @Nullable RunProfile profile) {
    boolean debug = DefaultDebugExecutor.EXECUTOR_ID.equals(executorId);
    if (!debug && !DefaultRunExecutor.EXECUTOR_ID.equals(executorId)) return null;
    if (profile instanceof ErlangApplicationConfiguration) return debug ? DEBUG_APPLICATION : RUN_APPLICATION;
    if (profile instanceof ErlangConsoleRunConfiguration) return debug ? DEBUG_CONSOLE : RUN_CONSOLE;
    if (profile instanceof ErlangUnitRunConfiguration) return debug ? DEBUG_EUNIT : RUN_EUNIT;
    if (profile instanceof RebarEunitRunConfiguration) return debug ? DEBUG_REBAR_EUNIT : RUN_REBAR_EUNIT;
    if (profile instanceof RebarRunConfiguration) return debug ? DEBUG_REBAR : RUN_REBAR;
    if (profile instanceof ErlangRemoteDebugRunConfiguration) return debug ? DEBUG_REMOTE_NODE : RUN_REMOTE_NODE;
    return null;
  }

  public static final class Debugger implements XDebuggerManagerListener {
    @Override
    public void processStarted(@NotNull XDebugProcess debugProcess) {
      if (debugProcess instanceof ErlangXDebugProcess) ErlangUsageCounters.count(DEBUG_SESSION);
    }
  }

  public static final class Build implements ProjectTaskListener {
    private final Project myProject;

    public Build(@NotNull Project project) {
      myProject = project;
    }

    @Override
    public void finished(@NotNull ProjectTaskManager.Result result) {
      if (result.isAborted() || !ErlangUsageCounters.isCounting() || myProject.isDisposed()) return;
      for (Module module : ModuleManager.getInstance(myProject).getModules()) {
        if (ModuleType.get(module) instanceof ErlangModuleType) {
          boolean rebar = ErlangCompilerSettings.getInstance(myProject).isUseRebarCompilerEnabled();
          ErlangUsageCounters.count(rebar ? BUILD_REBAR : BUILD_BUILT_IN);
          return;
        }
      }
    }
  }

  public static final class StructureView implements ToolWindowManagerListener {
    private final Project myProject;

    public StructureView(@NotNull Project project) {
      myProject = project;
    }

    @Override
    public void toolWindowShown(@NotNull ToolWindow toolWindow) {
      if (ToolWindowId.STRUCTURE_VIEW.equals(toolWindow.getId()) && isErlangEditor(myProject)) {
        ErlangUsageCounters.count(STRUCTURE_VIEW);
      }
    }
  }

  public static final class LiveTemplates implements TemplateManagerListener {
    private final Project myProject;

    public LiveTemplates(@NotNull Project project) {
      myProject = project;
    }

    @Override
    public void templateStarted(@NotNull TemplateState state) {
      if (!ErlangUsageCounters.isCounting()) return;
      // a live template has an abbreviation; the templates of in-place rename and of other code have none
      TemplateImpl template = state.getTemplate();
      if (template != null && !StringUtil.isEmpty(template.getKey()) && isErlangEditor(myProject)) {
        ErlangUsageCounters.count(LIVE_TEMPLATE);
      }
    }
  }

  public static final class OpenedFiles implements FileEditorManagerListener {
    @Override
    public void fileOpened(@NotNull FileEditorManager source, @NotNull VirtualFile file) {
      ErlangUsageKey key = openedFileKey(file);
      if (key != null) ErlangUsageCounters.count(key);
    }
  }

  @Nullable
  static ErlangUsageKey openedFileKey(@NotNull VirtualFile file) {
    if (!(file.getFileType() instanceof ErlangFileType type)) return null;
    if (type == ErlangFileType.MODULE) return "erl".equals(file.getExtension()) ? OPENED_ERL : OPENED_ESCRIPT;
    if (type == ErlangFileType.HEADER) return OPENED_HRL;
    if (type == ErlangFileType.APP) return OPENED_APP;
    return OPENED_CONFIG;
  }

  private static boolean isErlangEditor(@NotNull Project project) {
    if (project.isDisposed()) return false;
    Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
    VirtualFile file = editor != null ? FileDocumentManager.getInstance().getFile(editor.getDocument()) : null;
    return file != null && file.getFileType() instanceof ErlangFileType;
  }
}
