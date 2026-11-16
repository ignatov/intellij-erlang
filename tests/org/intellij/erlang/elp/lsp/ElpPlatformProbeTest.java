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
import com.intellij.openapi.application.ApplicationManager;
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
import org.intellij.erlang.elp.ElpSettings;
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

  /** The real server on a copy of testData/elp/project: load, definition, diagnostics, rename, TMPDIR, the processes after stop. */
  public void testProbeLiveServer() throws Exception {
    String elp = System.getenv("ERLANG_ELP_PATH");
    Assume.assumeTrue("Set ERLANG_ELP_PATH to the elp binary to run this probe", elp != null && !elp.isBlank());
    List<String> failed = new ArrayList<>();
    Path created = FileUtil.createTempDirectory("elp-probe", null, true).toPath();
    Path project = created.toRealPath().resolve("project");
    probe("project " + project + " (created as " + created + ")");
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

    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(elp, ElpMode.ELP));
    long start = System.currentTimeMillis();
    LspServerManager.getInstance(getProject()).ensureServerStarted(ElpLspIntegrationProvider.class, descriptor);
    boolean serving = waitFor("serving", () -> ElpEngine.ownsFeature(getProject(), shop, ElpEngine.Feature.NAVIGATION));
    probe("serving " + serving + " after " + (System.currentTimeMillis() - start) + " ms; state " + descriptor.getLoadState().state() +
          "; history " + descriptor.getLoadState().history() + "; otpNotFound " + descriptor.getLoadState().isOtpNotFound() +
          "; firstError " + descriptor.getLoadState().firstError());
    probe("last log lines " + descriptor.getLoadState().lastLogLines());
    probe("uri of shop.erl " + descriptor.getFileUri(shop) + "; tmpDir " + descriptor.getTmpDir());
    List<Long> pids = new ArrayList<>();
    long pid = descriptor.getProcessPid();
    pids.add(pid);
    ProcessHandle.of(pid).ifPresent(p -> p.descendants().forEach(child -> pids.add(child.pid())));
    probe("processes " + describe(pids));

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
      probe("definition answered " + answered + " in " + (System.currentTimeMillis() - definitionStart) + " ms, target " + target);
      if (!"shop_backend.erl".equals(target)) failed.add("definition");

      VirtualFile defects = myRoot.findFileByRelativePath("src/shop_defects.erl");
      assertNotNull(defects);
      myFixture.openFileInEditor(defects);
      List<HighlightInfo> infos = new ArrayList<>();
      boolean diagnostics = waitFor("diagnostics", () -> {
        infos.clear();
        infos.addAll(myFixture.doHighlighting());
        return infos.stream().anyMatch(info -> "function unused_function/0 is unused".equals(info.getDescription()));
      });
      probe("diagnostics " + diagnostics + "; " + infos.size() + " infos, first " +
            infos.stream().map(HighlightInfo::getDescription).filter(d -> d != null).limit(6).toList());
      if (!diagnostics) failed.add("diagnostics");

      myFixture.openFileInEditor(shop);
      Editor shopEditor = myFixture.getEditor();
      shopEditor.getCaretModel().moveToOffset(shopEditor.getDocument().getText().indexOf("\nsum([], Acc)") + 2);
      String renameError = ElpRenameHandler.renameAtCaret(getProject(), shopEditor, myFixture.getFile(), "add_up");
      boolean renamed = shopEditor.getDocument().getText().contains("add_up([Price | Rest], Acc) -> add_up(Rest, Acc + Price).");
      probe("rename error " + renameError + "; renamed " + renamed);
      if (!renamed) failed.add("rename");
    }

    Path tmpDir = descriptor.getTmpDir();
    boolean inTmpDir = waitFor("eqwalizer in TMPDIR", () -> tmpDir != null && eqwalizerIn(tmpDir.toFile(), 0) != null, 60);
    if (!inTmpDir) failed.add("eqwalizer in TMPDIR");
    for (String variable : List.of("TEMP", "TMP", "TMPDIR")) {
      String dir = System.getenv(variable);
      probe("env " + variable + "=" + dir + (dir == null ? "" : " eqwalizer since start: " + eqwalizerIn(new File(dir), start)));
    }
    probe("tmpDir " + tmpDir + " eqwalizer " + (tmpDir == null ? null : eqwalizerIn(tmpDir.toFile(), 0)));

    LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    long stopStart = System.currentTimeMillis();
    boolean allGone = waitFor("processes gone", () -> pids.stream().noneMatch(p -> ProcessHandle.of(p).map(ProcessHandle::isAlive).orElse(false)), 60);
    probe("after stop: all gone " + allGone + " in " + (System.currentTimeMillis() - stopStart) + " ms; alive " +
          describe(pids.stream().filter(p -> ProcessHandle.of(p).map(ProcessHandle::isAlive).orElse(false)).toList()));
    if (!allGone) failed.add("processes left");
    boolean tmpGone = waitFor("tmpDir gone", () -> tmpDir == null || !Files.exists(tmpDir), 60);
    probe("tmpDir deleted after stop " + tmpGone);
    if (!tmpGone) failed.add("tmpDir left");
    if (!failed.isEmpty()) fail("PROBE failed: " + failed);
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
