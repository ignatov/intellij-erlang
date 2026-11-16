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

import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * The server process through the platform's LSP client, with shell scripts standing in for ELP: one that exits at once, as a
 * text file with the executable bit does, and one that ignores {@code shutdown}, {@code exit} and its standard input and has a
 * child, as ELP does while a load hangs.
 */
public class ElpServerProcessTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final List<Notification> myNotifications = new CopyOnWriteArrayList<>();
  private Path myDir;
  private VirtualFile myRoot;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(@NotNull Notification notification) {
        myNotifications.add(notification);
      }
    });
    myDir = FileUtil.createTempDirectory("elp-process", null, true).toPath().toRealPath();
    Path project = Files.createDirectories(myDir.resolve("project/src"));
    Files.writeString(project.resolve("a.erl"), "-module(a).");
    myRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(myDir.resolve("project"));
    assertNotNull(myRoot);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  /** Measured in the IDE: a text file with the executable bit exits with 127 and nothing tells the user. */
  public void testServerThatExitsAtOnceDidNotStart() throws Exception {
    String elp = script("elp", "echo 'line 1: not: command not found' >&2\nexit 127");
    start(elp);
    PlatformTestUtil.waitWithEventsDispatching("no notification", () -> notification("ELP did not start") != null, 60);
    String content = notification("ELP did not start").getContent();
    assertTrue(content, content.contains(elp));
    assertTrue(content, content.contains("code 127"));
    assertTrue(content, content.contains("not: command not found"));
    assertNull("a server that never started is not restarted", notification("ELP stopped unexpectedly"));
  }

  /**
   * ELP ignores {@code shutdown}, {@code exit} and a closed standard input while a load hangs. The platform's client sends
   * {@code shutdown}, waits 10 s, sends {@code exit} and then destroys the process tree; no process of the server is left.
   */
  public void testStopKillsAnInitializedServerThatIgnoresShutdownWithItsChildren() throws Exception {
    Path pids = myDir.resolve("pids");
    Path standIn = Files.writeString(myDir.resolve("stand_in.py"), STAND_IN.replace("PIDS", pids.toString()));
    ElpLspClientDescriptor descriptor = start(script("elp", "exec python3 -I '" + standIn + "'"));
    PlatformTestUtil.waitWithEventsDispatching("the stand-in was not initialized", () -> descriptor.getLoadState().wasInitialized(), 60);
    assertStopKillsTheTree(descriptor, pids);
  }

  /**
   * The same before the client has initialized the server: the platform's client then stops without destroying the process, so the
   * plugin kills it with its children once the client has let it go.
   */
  public void testStopKillsAServerThatNeverAnswersWithItsChildren() throws Exception {
    Path pids = myDir.resolve("pids");
    String elp = script("elp", "trap '' TERM INT HUP\nsleep 600 &\necho $! > '" + pids + ".child'\necho $$ > '" + pids + ".self'\n" +
                               "while true; do sleep 1; done");
    ElpLspClientDescriptor descriptor = start(elp);
    assertStopKillsTheTree(descriptor, pids);
  }

  private void assertStopKillsTheTree(ElpLspClientDescriptor descriptor, Path pids) throws Exception {
    PlatformTestUtil.waitWithEventsDispatching("the stand-in did not start", () -> Files.exists(Path.of(pids + ".self")), 60);
    long self = Long.parseLong(Files.readString(Path.of(pids + ".self")).trim());
    long child = Long.parseLong(Files.readString(Path.of(pids + ".child")).trim());
    assertEquals(self, descriptor.getProcessPid());
    assertTrue(alive(self) && alive(child));
    long stop = System.currentTimeMillis();
    LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    try {
      PlatformTestUtil.waitWithEventsDispatching("the server or its child is still alive", () -> !alive(self) && !alive(child), 60);
    }
    finally {
      // Never leave a process of the test behind, even when the assertion fails.
      ProcessHandle.of(child).ifPresent(ProcessHandle::destroyForcibly);
      ProcessHandle.of(self).ifPresent(ProcessHandle::destroyForcibly);
    }
    System.out.println(getName() + ": process tree gone " + (System.currentTimeMillis() - stop) + " ms after the stop");
    assertNull("a stop that was asked for is not a crash", notification("ELP stopped unexpectedly"));
    assertNull(notification("ELP did not start"));
  }

  /** A language server that answers initialize and then ignores everything, including shutdown, exit and the end of its input. */
  private static final String STAND_IN = """
    import json, os, signal, subprocess, sys, time
    for s in (signal.SIGTERM, signal.SIGINT, signal.SIGHUP):
        signal.signal(s, signal.SIG_IGN)
    child = subprocess.Popen(["sleep", "600"])
    open("PIDS.child", "w").write(str(child.pid))
    open("PIDS.self", "w").write(str(os.getpid()))
    inp, out = sys.stdin.buffer, sys.stdout.buffer
    while True:
        headers = {}
        line = inp.readline()
        while line and line.strip():
            k, v = line.decode().split(":", 1)
            headers[k.strip().lower()] = v.strip()
            line = inp.readline()
        if not line:
            break
        message = json.loads(inp.read(int(headers["content-length"])))
        if message.get("method") == "initialize":
            body = json.dumps({"jsonrpc": "2.0", "id": message["id"], "result": {"capabilities": {}}}).encode()
            out.write(b"Content-Length: %d\\r\\n\\r\\n" % len(body) + body)
            out.flush()
    while True:
        time.sleep(1)
    """;

  /** At most one automatic restart in five minutes; then the plugin stays and says so. */
  public void testAutomaticRestartsAreLimited() {
    ElpRestarts restarts = new ElpRestarts();
    long now = 1_000_000;
    assertTrue(restarts.claimAutomaticRestart(now));
    assertFalse(restarts.claimAutomaticRestart(now + 60_000));
    assertFalse(restarts.claimAutomaticRestart(now + ElpRestarts.WINDOW_MS - 1));
    assertTrue(restarts.claimAutomaticRestart(now + ElpRestarts.WINDOW_MS));
  }

  private ElpLspClientDescriptor start(String elp) {
    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(elp, ElpMode.ELP));
    LspServerManager.getInstance(getProject()).ensureServerStarted(ElpLspIntegrationProvider.class, descriptor);
    return descriptor;
  }

  private Notification notification(String title) {
    return ContainerUtil.find(myNotifications, n -> title.equals(n.getTitle()));
  }

  private static boolean alive(long pid) {
    return ProcessHandle.of(pid).map(ProcessHandle::isAlive).orElse(false);
  }

  private String script(String name, String body) throws Exception {
    Path script = Files.writeString(myDir.resolve(name), "#!/bin/sh\n" + body + "\n");
    assertTrue(script.toFile().setExecutable(true));
    return script.toString();
  }
}
