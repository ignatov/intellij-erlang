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

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspClientDescriptor;
import com.intellij.platform.lsp.api.LspServerListener;
import com.intellij.platform.lsp.api.customization.LspCustomization;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.eclipse.lsp4j.InitializeResult;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpEnvironment;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpMode;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Starts {@code elp server} over stdio for one workspace root. The initialization options are left empty, as ELP's own
 * VS Code client does; ELP reads the project from {@code rebar.config} in the root.
 */
final class ElpLspClientDescriptor extends LspClientDescriptor {
  private static final Logger LOG = Logger.getInstance(ElpLspClientDescriptor.class);

  private final ElpConfiguration myConfiguration;
  private LspCustomization myCustomization;

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

  @NotNull
  @Override
  @RequiresBackgroundThread
  public GeneralCommandLine createCommandLine() {
    GeneralCommandLine commandLine = new GeneralCommandLine(myConfiguration.path(), "server");
    commandLine.setWorkDirectory(getRoots()[0].getPath());
    String path = ElpEnvironment.serverPath(getProject());
    if (path != null) {
      commandLine.withEnvironment("PATH", path);
    }
    LOG.info("ELP (" + myConfiguration.mode() + "): " + commandLine.getCommandLineString() +
             " in " + commandLine.getWorkDirectory() + " with PATH=" + path);
    return commandLine;
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
      myCustomization = myConfiguration.mode() == ElpMode.ELP ? new ElpEngineCustomization(diagnostics)
                                                               : new ElpDiagnosticsOnlyCustomization(diagnostics);
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
