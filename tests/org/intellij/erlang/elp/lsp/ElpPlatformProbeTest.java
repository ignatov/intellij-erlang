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

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.execution.configurations.PathEnvironmentVariableUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.platform.lsp.api.LspServerManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.PsiTestUtil;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BooleanSupplier;

/**
 * A probe for the ELP probe workflow, not a regression test: it prints what the real ELP and the plugin's path mapping do on this
 * OS as lines that start with {@code PROBE}, and fails only at the end with the list of the steps that did not work.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ElpPlatformProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int TIMEOUT_SECONDS = 120;

  private ElpSettings.ElpSettingsState myOldState;
  private VirtualFile myRoot;
  private Sdk myOldSdk;
  private boolean mySdkChanged;
  private String myOldRebarPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.source = old.source;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
      ElpSettings.getInstance().loadState(myOldState);
      if (mySdkChanged) WriteAction.run(() -> ProjectRootManager.getInstance(getProject()).setProjectSdk(myOldSdk));
      if (myOldRebarPath != null) RebarSettings.getInstance(getProject()).setRebarPath(myOldRebarPath);
      if (myRoot != null) PsiTestUtil.removeContentEntry(getModule(), myRoot);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  /** The path mapping of {@link ElpLspClientDescriptor} for a root under a symlink, without a server. */
  public void testProbeSymlinkMapping() throws Exception {
    Path created = FileUtil.createTempDirectory("elp-real-path", null, true).toPath();
    Path temp = created.toRealPath();
    probe("temp " + created + " real " + temp + " same " + created.equals(temp));
    Path real = Files.createDirectories(temp.resolve("real dir/project/src"));
    Files.writeString(real.resolve("a.erl"), "-module(a).");
    Path link;
    try {
      link = Files.createSymbolicLink(temp.resolve("link"), temp.resolve("real dir"));
    }
    catch (Exception e) {
      probe("symlink cannot be created: " + e);
      return;
    }
    VirtualFile root = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(link.resolve("project"));
    assertNotNull(root);
    VirtualFile module = root.findFileByRelativePath("src/a.erl");
    assertNotNull(module);
    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration("elp", ElpMode.ELP));
    probe("root.getPath " + root.getPath());
    String linkUri = descriptor.getFileUri(module);
    probe("linkUri " + linkUri + " -> getPath " + java.net.URI.create(linkUri).getPath());
    descriptor.resolveRealRoot();
    String realUri = descriptor.getFileUri(module);
    probe("realUri " + realUri + " mapped " + !realUri.equals(linkUri));
    List<String> failed = new ArrayList<>();
    Map<String, String> uris = new LinkedHashMap<>();
    uris.put("realUri", realUri);
    uris.put("linkUri", linkUri);
    String javaUri = real.resolve("a.erl").toUri().toString();
    uris.put("java toUri", javaUri);
    if (javaUri.matches("file:///[A-Za-z]:/.*")) {
      String drive = javaUri.substring(8, 9);
      String rest = javaUri.substring(10);
      uris.put("rust upper drive", "file:///" + drive.toUpperCase() + ":" + rest);
      uris.put("lower drive", "file:///" + drive.toLowerCase() + ":" + rest);
      uris.put("vscode lower %3A", "file:///" + drive.toLowerCase() + "%3A" + rest);
    }
    for (Map.Entry<String, String> entry : uris.entrySet()) {
      VirtualFile found = descriptor.findFileByUri(entry.getValue());
      boolean ok = module.equals(found);
      probe("findFileByUri " + entry.getKey() + " " + entry.getValue() + " -> " + (found == null ? "null" : found.getPath()) + " ok " + ok);
      if (!ok) failed.add("findFileByUri " + entry.getKey());
    }
    if (!failed.isEmpty()) fail("PROBE failed: " + failed);
  }

  /** The real server on a copy of testData/elp/project: load, definition, diagnostics, two renames, TMPDIR, the processes. */
  public void testProbeLiveServer() throws Exception {
    probeLive("default", null, null, null);
  }

  /**
   * The same with {@code ELP_EQWALIZER_PATH} set to a file without an extension that does not exist: the release build for Windows
   * panics in {@code EqwalizerExe::ensure_exe} on its embedded eqWAlizer, and this runtime override takes another branch there.
   */
  public void testProbeLiveServerWithoutEqwalizerBinary() throws Exception {
    Path none = FileUtil.createTempDirectory("elp-no-eqwalizer", null, true).toPath().resolve("no-eqwalizer");
    probeLive("no-eqwalizer", Map.of("ELP_EQWALIZER_PATH", none.toString()), null, null);
  }

  /** Started as the IDE starts it, after the preflight, with the rebar3.cmd wrapper next to the escript as the rebar path. */
  public void testProbeIdeStartWithRebar3Wrapper() throws Exception {
    String escript = System.getenv("PROBE_REBAR3");
    Assume.assumeTrue("Set PROBE_REBAR3 to the rebar3 escript", escript != null && new File(escript + ".cmd").isFile());
    probeLive("ide-start-rebar3.cmd", null, escript + ".cmd", null);
  }

  /**
   * The same in a directory whose path is all lower case: ELP on Windows answers with lower-cased file URIs, and in the IDE all of
   * its diagnostics for the project under the mixed-case temporary directory were empty.
   */
  public void testProbeLiveServerInLowerCasePath() throws Exception {
    String parent = System.getenv("PROBE_LOWER_CASE_DIR");
    Assume.assumeTrue("Set PROBE_LOWER_CASE_DIR to a directory whose path is all lower case", parent != null && !parent.isBlank());
    probeLive("lower-case-path", null, null, Path.of(parent).resolve("run" + System.currentTimeMillis()));
  }

  private void probeLive(@NotNull String label, @Nullable Map<String, String> environment, @Nullable String rebarPath,
                         @Nullable Path parent) throws Exception {
    String elp = System.getenv("ERLANG_ELP_PATH");
    Assume.assumeTrue("Set ERLANG_ELP_PATH to the elp binary to run this probe", elp != null && !elp.isBlank());
    label = "[" + label + "] ";
    List<String> failed = new ArrayList<>();
    Path created = parent != null ? Files.createDirectories(parent) : FileUtil.createTempDirectory("elp-probe", null, true).toPath();
    Path project = created.toRealPath().resolve("project");
    probe(label + "project " + project + " (created as " + created + ")");
    FileUtil.copyDir(new File("testData/elp/project"), project.toFile());
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), project.toString());
    myRoot = LocalFileSystem.getInstance().refreshAndFindFileByNioFile(project);
    assertNotNull(myRoot);
    PsiTestUtil.addSourceContentToRoots(getModule(), myRoot);
    IndexingTestUtil.waitUntilIndexesAreReady(getProject());
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = elp;
    state.mode = ElpMode.ELP.name();
    ElpSettings.getInstance().loadState(state);
    VirtualFile shop = myRoot.findFileByRelativePath("src/shop.erl");
    assertNotNull(shop);
    myFixture.openFileInEditor(shop);

    long start = System.currentTimeMillis();
    ElpLspClientDescriptor descriptor;
    if (rebarPath != null) {
      File erl = PathEnvironmentVariableUtil.findInPath(SystemInfo.isWindows ? "erl.exe" : "erl");
      assertNotNull("erl is not on the PATH", erl);
      String otp = new File(erl.getParentFile(), "..").getCanonicalPath();
      Sdk sdk = ErlangSdkType.createMockSdk(otp, ErlangSdkRelease.V_27_0);
      myOldSdk = ProjectRootManager.getInstance(getProject()).getProjectSdk();
      mySdkChanged = true;
      WriteAction.run(() -> {
        ProjectJdkTable.getInstance().addJdk(sdk, getTestRootDisposable());
        ProjectRootManager.getInstance(getProject()).setProjectSdk(sdk);
      });
      myOldRebarPath = RebarSettings.getInstance(getProject()).getRebarPath();
      RebarSettings.getInstance(getProject()).setRebarPath(rebarPath);
      ElpPreflight.getInstance(getProject()).invalidate();
      ElpPreflight.Result preflight = PlatformTestUtil.waitForFuture(
        ApplicationManager.getApplication().executeOnPooledThread(() -> ElpPreflight.check(getProject())), 120_000);
      probe(label + "otp " + otp + "; preflight canStart " + preflight.canStart() + " problems " + preflight.problems());
      ElpLspIntegrationProvider.startServersIfNeeded(getProject());
      ElpLspClientDescriptor[] found = new ElpLspClientDescriptor[1];
      boolean started = waitFor("a server", () -> {
        for (var server : LspServerManager.getInstance(getProject()).getServersForProvider(ElpLspIntegrationProvider.class)) {
          if (server.getDescriptor() instanceof ElpLspClientDescriptor d) found[0] = d;
        }
        return found[0] != null;
      }, 60);
      probe(label + "server created " + started);
      if (!started) fail("PROBE failed: " + label + "no server");
      descriptor = found[0];
    }
    else {
      descriptor = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(elp, ElpMode.ELP));
      descriptor.setEnvironmentForTests(environment);
      LspServerManager.getInstance(getProject()).ensureServerStarted(ElpLspIntegrationProvider.class, descriptor);
    }
    ElpLspClientDescriptor server = descriptor;
    boolean serving = waitFor("serving", () -> ElpEngine.ownsFeature(getProject(), shop, ElpEngine.Feature.NAVIGATION) ||
                                               server.getLoadState().state() == ElpLoadState.State.FAILED);
    serving = serving && server.getLoadState().state() != ElpLoadState.State.FAILED;
    probe(label + "serving " + serving + " after " + (System.currentTimeMillis() - start) + " ms; state " + server.getLoadState().state() +
          "; history " + server.getLoadState().history() + "; otpNotFound " + server.getLoadState().isOtpNotFound() +
          "; firstError " + server.getLoadState().firstError());
    probe(label + "uri of shop.erl " + server.getFileUri(shop) + "; tmpDir " + server.getTmpDir());
    List<Long> pids = new ArrayList<>();
    long pid = server.getProcessPid();
    pids.add(pid);
    ProcessHandle.of(pid).ifPresent(p -> p.descendants().forEach(child -> pids.add(child.pid())));
    probe(label + "processes " + describe(pids));

    if (!serving) {
      failed.add("serving");
    }
    else {
      Editor editor = myFixture.getEditor();
      PsiFile shopPsi = myFixture.getFile();
      int call = editor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
      PsiElement[][] targets = new PsiElement[1][];
      long definitionStart = System.currentTimeMillis();
      boolean answered = waitFor("definition", () -> (targets[0] = definition(shopPsi, editor, call)) != null);
      String target = answered && targets[0].length > 0 ? targets[0][0].getContainingFile().getName() : "none";
      probe(label + "definition answered " + answered + " in " + (System.currentTimeMillis() - definitionStart) + " ms, target " + target);
      if (!"shop_backend.erl".equals(target)) failed.add("definition");

      VirtualFile defects = myRoot.findFileByRelativePath("src/shop_defects.erl");
      assertNotNull(defects);
      myFixture.openFileInEditor(defects);
      List<HighlightInfo> infos = new ArrayList<>();
      boolean diagnostics = waitFor("diagnostics", () -> {
        infos.clear();
        infos.addAll(myFixture.doHighlighting());
        return infos.stream().anyMatch(info -> "function unused_function/0 is unused".equals(info.getDescription()));
      }, 90);
      probe(label + "diagnostics " + diagnostics + "; " + infos.size() + " infos, first " +
            infos.stream().map(HighlightInfo::getDescription).filter(d -> d != null).limit(6).toList());
      if (!diagnostics) failed.add("diagnostics");

      myFixture.openFileInEditor(shop);
      Editor shopEditor = myFixture.getEditor();
      shopEditor.getCaretModel().moveToOffset(shopEditor.getDocument().getText().indexOf("\nsum([], Acc)") + 2);
      String renameError = ElpRenameHandler.renameAtCaret(getProject(), shopEditor, myFixture.getFile(), "add_up");
      boolean renamed = shopEditor.getDocument().getText().contains("add_up([Price | Rest], Acc) -> add_up(Rest, Acc + Price).");
      probe(label + "rename error " + renameError + "; renamed " + renamed);
      if (!renamed) failed.add("rename");
      shopEditor.getCaretModel().moveToOffset(shopEditor.getDocument().getText().indexOf("\nadd_up([], Acc)") + 2);
      String secondError = ElpRenameHandler.renameAtCaret(getProject(), shopEditor, myFixture.getFile(), "total");
      String text = shopEditor.getDocument().getText();
      boolean second = text.contains("total([Price | Rest], Acc) -> total(Rest, Acc + Price).") && !text.contains("add_up");
      probe(label + "second rename in the same open file error " + secondError + "; ok " + second +
            (second ? "" : "; lines " + text.lines().filter(l -> l.contains("total") || l.contains("add_up")).limit(4).toList()));
      if (!second) failed.add("second rename");
    }

    Path tmpDir = server.getTmpDir();
    boolean inTmpDir = waitFor("eqwalizer in TMPDIR", () -> tmpDir != null && eqwalizerIn(tmpDir.toFile(), 0) != null, 20);
    for (String variable : List.of("TEMP", "TMP")) {
      String dir = System.getenv(variable);
      probe(label + "env " + variable + "=" + dir + (dir == null ? "" : " eqwalizer since start: " + eqwalizerIn(new File(dir), start)));
    }
    probe(label + "eqwalizer in the plugin's TMPDIR " + inTmpDir);
    probe(label + "last log lines " + server.getLoadState().lastLogLines());

    LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    long stopStart = System.currentTimeMillis();
    boolean allGone = waitFor("processes gone", () -> pids.stream().noneMatch(p -> ProcessHandle.of(p).map(ProcessHandle::isAlive).orElse(false)), 60);
    probe(label + "after stop: all gone " + allGone + " in " + (System.currentTimeMillis() - stopStart) + " ms; alive " +
          describe(pids.stream().filter(p -> ProcessHandle.of(p).map(ProcessHandle::isAlive).orElse(false)).toList()));
    if (!allGone) failed.add("processes left");
    boolean tmpGone = waitFor("tmpDir gone", () -> tmpDir == null || !Files.exists(tmpDir), 60);
    probe(label + "tmpDir deleted after stop " + tmpGone);
    if (!tmpGone) failed.add("tmpDir left");
    if (!failed.isEmpty()) fail("PROBE failed: " + label + failed);
  }

  private static void probe(@NotNull String line) {
    System.out.println("PROBE " + line);
  }

  private static boolean waitFor(@NotNull String what, @NotNull BooleanSupplier condition) {
    return waitFor(what, condition, TIMEOUT_SECONDS);
  }

  private static boolean waitFor(@NotNull String what, @NotNull BooleanSupplier condition, int seconds) {
    try {
      PlatformTestUtil.waitWithEventsDispatching(what, condition, seconds);
      return true;
    }
    catch (Throwable e) {
      probe(what + ": not reached in " + seconds + " s (" + e.getClass().getSimpleName() + ")");
      return false;
    }
  }

  @NotNull
  private static String describe(@NotNull List<Long> pids) {
    List<String> result = new ArrayList<>();
    for (Long pid : pids) {
      result.add(pid + "=" + ProcessHandle.of(pid).flatMap(p -> p.info().command()).map(c -> new File(c).getName()).orElse("?"));
    }
    return result.toString();
  }

  @Nullable
  private static String eqwalizerIn(@NotNull File dir, long since) {
    File[] files = dir.listFiles();
    if (files == null) return null;
    for (File file : files) {
      if (file.getName().startsWith("eqwalizer") && file.lastModified() >= since) return file.getName() + " (" + file.length() + " bytes)";
    }
    return null;
  }

  @Nullable
  private static PsiElement[] definition(@NotNull PsiFile file, @NotNull Editor editor, int offset) {
    return PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(() -> ReadAction.compute(
      () -> new ElpGotoDeclarationHandler().getGotoDeclarationTargets(file.findElementAt(offset), offset, editor))), 30_000);
  }
}
