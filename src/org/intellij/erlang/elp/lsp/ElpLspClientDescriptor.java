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

package org.intellij.erlang.elp.lsp;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessEvent;
import com.intellij.execution.process.ProcessListener;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServerDescriptor;
import com.intellij.platform.lsp.api.LspServerListener;
import com.intellij.platform.lsp.api.customization.LspCustomization;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.eclipse.lsp4j.InitializeResult;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServerEnvironment;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.nio.file.Path;
import java.util.Map;

/**
 * Starts {@code elp server} over stdio for one workspace root. The initialization options are left empty, as ELP's own
 * VS Code client does; ELP reads the project from {@code rebar.config} in the root.
 */
final class ElpLspClientDescriptor extends LspServerDescriptor {
  private static final Logger LOG = Logger.getInstance(ElpLspClientDescriptor.class);

  private final ElpConfiguration myConfiguration;
  private LspCustomization myCustomization;
  private volatile Path myTmpDir;

  ElpLspClientDescriptor(@NotNull Project project, @NotNull VirtualFile root, @NotNull ElpConfiguration configuration) {
    super(project, "ELP", root);
    myConfiguration = configuration;
  }

  @NotNull
  ElpConfiguration getConfiguration() {
    return myConfiguration;
  }

  @Override
  public boolean isSupportedFile(@NotNull VirtualFile file) {
    return ElpFiles.isElpFile(file);
  }

  /**
   * The command line of the server with its environment, see {@link ElpServerEnvironment}. Every call creates a new temporary
   * directory for the process; {@link #startServerProcess()} deletes it when the process ends.
   */
  @NotNull
  @Override
  @RequiresBackgroundThread
  public GeneralCommandLine createCommandLine() throws ExecutionException {
    GeneralCommandLine commandLine = new GeneralCommandLine(myConfiguration.path(), "server");
    commandLine.setWorkDirectory(getRoots()[0].getPath());
    // No temporary directory for a server that cannot start.
    if (!new File(myConfiguration.path()).canExecute()) throw new ExecutionException(myConfiguration.path() + " is not an executable file");
    Path tmpDir = ElpServerEnvironment.createSessionDirectory();
    myTmpDir = tmpDir;
    Map<String, String> environment = ElpServerEnvironment.forProject(getProject(), tmpDir);
    commandLine.withEnvironment(environment);
    LOG.info("ELP (" + myConfiguration.mode() + "): " + commandLine.getCommandLineString() +
             " in " + commandLine.getWorkDirectory() + " with " + environment);
    return commandLine;
  }

  @NotNull
  @Override
  @RequiresBackgroundThread
  public OSProcessHandler startServerProcess() throws ExecutionException {
    OSProcessHandler handler;
    try {
      handler = super.startServerProcess();
    }
    catch (ExecutionException | RuntimeException e) {
      deleteTmpDir();
      throw e;
    }
    handler.addProcessListener(new ProcessListener() {
      @Override
      public void processTerminated(@NotNull ProcessEvent event) {
        deleteTmpDir();
      }
    });
    return handler;
  }

  private void deleteTmpDir() {
    Path tmpDir = myTmpDir;
    myTmpDir = null;
    if (tmpDir != null) ElpServerEnvironment.deleteSessionDirectory(tmpDir);
  }

  /** The temporary directory of the server process started last, until it ends. */
  @Nullable
  Path getTmpDir() {
    return myTmpDir;
  }

  @NotNull
  @Override
  public String getLanguageId(@NotNull VirtualFile file) {
    return "erlang";
  }

  @NotNull
  @Override
  public synchronized LspCustomization getLspCustomization() {
    if (myCustomization == null) {
      ElpDiagnosticsSupport diagnostics = new ElpDiagnosticsSupport(myConfiguration.eqwalizer());
      myCustomization = new ElpEngineCustomization(diagnostics);
    }
    return myCustomization;
  }

  /**
   * In the {@code ELP} position the plugin's features switch off once this server runs and back on once it stops, see
   * {@link ElpEngine}; the open editors are refreshed at both moments.
   */
  @Nullable
  @Override
  public LspServerListener getLspServerListener() {
    if (myConfiguration.mode() != ElpMode.ELP) return null;
    return new LspServerListener() {
      @Override
      public void serverInitialized(@NotNull InitializeResult params) {
        ElpEngine.refreshProject(getProject());
      }

      @Override
      public void serverStopped(boolean shutdownNormally) {
        ElpEngine.refreshProject(getProject());
      }
    };
  }
}
