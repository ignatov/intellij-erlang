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
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.platform.lsp.api.Lsp4jClient;
import com.intellij.platform.lsp.api.LspServer;
import com.intellij.platform.lsp.api.LspServerDescriptor;
import com.intellij.platform.lsp.api.LspServerListener;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.platform.lsp.api.LspServerNotificationsHandler;
import com.intellij.platform.lsp.api.LspServerState;
import com.intellij.platform.lsp.api.customization.LspCustomization;
import com.intellij.util.concurrency.AppExecutorUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.io.URLUtil;
import org.eclipse.lsp4j.ClientCapabilities;
import org.eclipse.lsp4j.InitializeResult;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpFiles;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpServedRoots;
import org.intellij.erlang.elp.ElpServerEnvironment;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Starts {@code elp server} over stdio for one workspace root. The initialization options are left empty, as ELP's own
 * VS Code client does; ELP reads the project from {@code rebar.config} in the root.
 */
final class ElpLspClientDescriptor extends LspServerDescriptor {
  private static final Logger LOG = Logger.getInstance(ElpLspClientDescriptor.class);

  private final ElpConfiguration myConfiguration;
  private LspCustomization myCustomization;
  private volatile Path myTmpDir;
  private volatile RealRoot myRealRoot;

  /**
   * The workspace root as the project knows it ({@code link}) and its real path when that differs ({@code real}), with their URIs.
   */
  private record RealRoot(@NotNull String link, @NotNull String real, @NotNull String linkUri, @NotNull String realUri) {
  }

  private final ElpLoadState myLoadState = new ElpLoadState(this::loadStateChanged);
  private volatile Map<String, String> myEnvironmentForTests;
  private volatile long myPid = -1;
  private volatile OSProcessHandler myHandler;
  private volatile ScheduledFuture<?> myProcessWatch;
  private volatile long myOrphanSince;
  private volatile boolean myKilledAsOrphan;

  /**
   * How long a server process may outlive its client before the plugin kills it with its children. The platform's client waits 10 s
   * for the answer to {@code shutdown} and then destroys the process tree; it does not stop a process whose client is stopped
   * while it initializes.
   */
  static final long ORPHAN_GRACE_MS = 12_000;

  ElpLspClientDescriptor(@NotNull Project project, @NotNull VirtualFile root, @NotNull ElpConfiguration configuration) {
    super(project, "ELP", root);
    myConfiguration = configuration;
  }

  /** Whether ELP has loaded the project of this server, see {@link ElpLoadState}. */
  @NotNull
  ElpLoadState getLoadState() {
    return myLoadState;
  }

  /**
   * ELP is asked only once it has loaded the project. The first time, the root enters {@link ElpServedRoots} and the plugin, which
   * answered until then, goes silent there for good. At that moment, and when ELP stops serving, the open editors are refreshed, so
   * that ELP's diagnostics, folding and parameter hints replace the plugin's at once.
   */
  private void loadStateChanged(@NotNull ElpLoadState loadState, @NotNull ElpLoadState.State oldState, @NotNull ElpLoadState.State newState) {
    Project project = getProject();
    LOG.info("ELP " + getRoots()[0].getPath() + ": " + oldState + " -> " + newState);
    if (project.isDisposed()) return;
    // From the first Running on, the root is ELP's for good: a crash or a restart does not give it back to the plugin.
    if (newState == ElpLoadState.State.RUNNING && myConfiguration.mode() == ElpMode.ELP) {
      ElpServedRoots.getInstance().add(getRoots()[0].getPath());
    }
    if (newState == ElpLoadState.State.RUNNING || oldState == ElpLoadState.State.RUNNING) ElpEngine.refreshProject(project);
    if (newState == ElpLoadState.State.FAILED) {
      ElpNotifications.couldNotLoad(project, loadState, getRoots()[0].getPresentableUrl(), isServedRoot());
    }
    if (newState == ElpLoadState.State.RUNNING) {
      String error = loadState.takeErrorToReport();
      if (error != null) ElpNotifications.elpError(project, error);
    }
  }

  /** Whether the root of this server is ELP's, so that the plugin stays silent there whatever this server does. */
  private boolean isServedRoot() {
    return ElpServedRoots.getInstance().contains(getRoots()[0].getPath());
  }

  @NotNull
  @Override
  public ClientCapabilities getClientCapabilities() {
    ClientCapabilities capabilities = super.getClientCapabilities();
    // ELP sends elp/status (Loading, Running, ShuttingDown) only to a client that asks for it.
    capabilities.setExperimental(Map.of("serverStatusNotification", true));
    return capabilities;
  }

  @NotNull
  @Override
  public Lsp4jClient createLsp4jClient(@NotNull LspServerNotificationsHandler handler) {
    return new ElpLsp4jClient(handler, myLoadState);
  }

  /** Variables that a test puts over the server's environment, for example a PATH without {@code erl}. */
  @TestOnly
  void setEnvironmentForTests(@Nullable Map<String, String> environment) {
    myEnvironmentForTests = environment;
  }

  @NotNull
  ElpConfiguration getConfiguration() {
    return myConfiguration;
  }

  /** The Erlang modules and headers of this server's ELP, see {@link ElpFiles#serverGets}. */
  @Override
  public boolean isSupportedFile(@NotNull VirtualFile file) {
    if (!ElpFiles.isElpFile(file)) return false;
    RealRoot realRoot = myRealRoot;
    return ReadAction.computeBlocking(() -> ElpFiles.serverGets(getProject(), getRoots()[0], realRoot != null ? realRoot.real() : null, file));
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
    resolveRealRoot();
    Path tmpDir = ElpServerEnvironment.createSessionDirectory();
    myTmpDir = tmpDir;
    Map<String, String> environment = new LinkedHashMap<>(ElpServerEnvironment.forProject(getProject(), tmpDir));
    Map<String, String> forTests = myEnvironmentForTests;
    if (forTests != null) environment.putAll(forTests);
    commandLine.withEnvironment(environment);
    LOG.info("ELP (" + myConfiguration.mode() + "): " + commandLine.getCommandLineString() +
             " in " + commandLine.getWorkDirectory() + " with " + environment);
    return commandLine;
  }

  @NotNull
  @Override
  @RequiresBackgroundThread
  public OSProcessHandler startServerProcess() throws ExecutionException {
    myLoadState.started(System.currentTimeMillis());
    myPid = -1;
    myOrphanSince = 0;
    myKilledAsOrphan = false;
    OSProcessHandler handler;
    try {
      handler = super.startServerProcess();
    }
    catch (ExecutionException | RuntimeException e) {
      myLoadState.stopped();
      deleteTmpDir();
      ElpRestarts.didNotStart(getProject(), myConfiguration.path(), String.valueOf(e.getMessage()), myLoadState.lastLogLines());
      throw e;
    }
    myPid = handler.getProcess().pid();
    myHandler = handler;
    handler.addProcessListener(new ProcessListener() {
      @Override
      public void onTextAvailable(@NotNull ProcessEvent event, @NotNull Key outputType) {
        if (outputType == ProcessOutputTypes.STDERR) myLoadState.log(event.getText());
      }

      @Override
      public void processTerminated(@NotNull ProcessEvent event) {
        boolean initialized = myLoadState.wasInitialized();
        myLoadState.stopped();
        cancelProcessWatch();
        deleteTmpDir();
        // Once the client has seen the end of the process too.
        AppExecutorUtil.getAppScheduledExecutorService().schedule(() -> processEnded(initialized, event.getExitCode()), 1, TimeUnit.SECONDS);
      }
    });
    myProcessWatch = AppExecutorUtil.getAppScheduledExecutorService().scheduleWithFixedDelay(this::watchProcess, 2, 2, TimeUnit.SECONDS);
    return handler;
  }

  /**
   * Kills the process with its children when the platform's client no longer has it, see {@link #ORPHAN_GRACE_MS}.
   */
  private void watchProcess() {
    OSProcessHandler handler = myHandler;
    if (handler == null || handler.isProcessTerminated()) {
      cancelProcessWatch();
      return;
    }
    LspServer server = getProject().isDisposed() ? null : findClient();
    if (server != null && server.getState() != LspServerState.ShutdownNormally && server.getState() != LspServerState.ShutdownUnexpectedly) {
      myOrphanSince = 0;
      return;
    }
    long now = System.currentTimeMillis();
    if (myOrphanSince == 0) {
      myOrphanSince = now;
    }
    else if (now - myOrphanSince >= ORPHAN_GRACE_MS) {
      LOG.info("ELP " + getRoots()[0].getPath() + ": the client is " + (server == null ? "gone" : server.getState()) +
               ", killing the server process " + myPid + " and its children");
      myKilledAsOrphan = true;
      cancelProcessWatch();
      handler.destroyProcess();
    }
  }

  /**
   * The process ended. When the client did not ask for that, ELP either did not start (it ended before it was initialized) or
   * crashed, see {@link ElpRestarts}.
   */
  private void processEnded(boolean initialized, int exitCode) {
    if (myKilledAsOrphan || getProject().isDisposed()) return;
    LspServer server = findClient();
    if (server == null || server.getState() != LspServerState.ShutdownUnexpectedly) return;
    if (initialized) {
      ElpRestarts.crashed(getProject(), exitCode, myLoadState.lastLogLines(), isServedRoot());
    }
    else {
      ElpRestarts.didNotStart(getProject(), myConfiguration.path(), "it exited with code " + exitCode + " before it was initialized",
                              myLoadState.lastLogLines());
    }
  }

  /** The platform's client of this descriptor, if it still has one. */
  @Nullable
  private LspServer findClient() {
    for (LspServer server : LspServerManager.getInstance(getProject()).getServersForProvider(ElpLspIntegrationProvider.class)) {
      if (server.getDescriptor() == this) return server;
    }
    return null;
  }

  private void cancelProcessWatch() {
    ScheduledFuture<?> watch = myProcessWatch;
    myProcessWatch = null;
    if (watch != null) watch.cancel(false);
  }

  private void deleteTmpDir() {
    Path tmpDir = myTmpDir;
    myTmpDir = null;
    if (tmpDir != null) ElpServerEnvironment.deleteSessionDirectory(tmpDir);
  }

  /**
   * Finds out whether the workspace root lies under a symlink. ELP reports {@code L0003 Unknown application} on every file whose
   * URI goes through a symlink and resolves nothing there, so it gets the real paths of the files under the root, and the files it
   * names under the real root are mapped back to the root the project knows; see {@link #getFileUri} and
   * {@link #findLocalFileByPath}. Only the URIs of the documents and requests matter to ELP, not its working directory.
   */
  @RequiresBackgroundThread
  void resolveRealRoot() {
    VirtualFile root = getRoots()[0];
    RealRoot realRoot = null;
    if (root.isInLocalFileSystem()) {
      try {
        String link = root.getPath();
        String real = FileUtil.toSystemIndependentName(root.toNioPath().toRealPath().toString());
        if (!real.equals(link)) realRoot = new RealRoot(link, real, super.getFileUri(root), uri(real));
      }
      catch (IOException | UnsupportedOperationException e) {
        LOG.info("Cannot find the real path of " + root, e);
      }
    }
    myRealRoot = realRoot;
    // The plugin goes silent for the same files, see ElpFiles.serverGets.
    ElpServedRoots.getInstance().setRealPath(root.getPath(), realRoot != null ? realRoot.real() : null);
    if (realRoot != null) LOG.info("ELP gets the files under " + realRoot.link() + " by their real path " + realRoot.real());
  }

  @NotNull
  @Override
  public String getFileUri(@NotNull VirtualFile file) {
    String uri = super.getFileUri(file);
    RealRoot realRoot = myRealRoot;
    if (realRoot == null || !isUnder(uri, realRoot.linkUri())) return uri;
    return realRoot.realUri() + uri.substring(realRoot.linkUri().length());
  }

  /**
   * Every file that ELP names, in diagnostics, locations and edits, is found through this method, see {@code findFileByUri}.
   */
  @Nullable
  @Override
  protected VirtualFile findLocalFileByPath(@NotNull String path) {
    RealRoot realRoot = myRealRoot;
    if (realRoot != null && isUnder(path, realRoot.real())) path = realRoot.link() + path.substring(realRoot.real().length());
    return super.findLocalFileByPath(path);
  }

  private static boolean isUnder(@NotNull String path, @NotNull String root) {
    return path.equals(root) || path.startsWith(root) && path.charAt(root.length()) == '/';
  }

  /** As the platform builds the URI of a local file. */
  @NotNull
  private static String uri(@NotNull String path) {
    URI uri = VfsUtil.toUri(VirtualFileManager.constructUrl(URLUtil.FILE_PROTOCOL, URLUtil.encodePath(path)));
    return uri != null ? uri.toString() : VirtualFileManager.constructUrl(URLUtil.FILE_PROTOCOL, path);
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
      myCustomization = new ElpEngineCustomization(diagnostics, myLoadState::isRunning);
    }
    return myCustomization;
  }

  /**
   * Feeds {@link ElpLoadState}, which decides when the plugin's features switch off and back on, see {@link #loadStateChanged}.
   */
  @NotNull
  @Override
  public LspServerListener getLspServerListener() {
    return new LspServerListener() {
      @Override
      public void serverInitialized(@NotNull InitializeResult params) {
        myLoadState.initialized();
      }

      @Override
      public void serverStopped(boolean shutdownNormally) {
        myLoadState.stopped();
      }
    };
  }

  /** The process id of the server started last, or -1. */
  long getProcessPid() {
    return myPid;
  }
}
