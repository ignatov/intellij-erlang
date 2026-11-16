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
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEngine;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

/**
 * A smoke test with the real ELP through the platform's LSP client on {@code testData/elp/project}: the server starts, publishes
 * a diagnostic, answers a definition and a rename. Skipped unless the environment variable {@code ERLANG_ELP_PATH} names the
 * {@code elp} binary; ELP needs {@code erl} on the PATH. Run it with
 * {@code ERLANG_ELP_PATH=/path/to/elp ./gradlew test --rerun --tests 'org.intellij.erlang.elp.lsp.ElpLiveServerTest'}; without
 * {@code --rerun} Gradle may take the results of a run without the variable, which is not an input of the task.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ElpLiveServerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final int TIMEOUT_SECONDS = 120;

  private ElpSettings.ElpSettingsState myOldState;
  private VirtualFile myRoot;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
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

  public void testDiagnosticDefinitionAndRename() throws Exception {
    String elp = System.getenv("ERLANG_ELP_PATH");
    Assume.assumeTrue("Set ERLANG_ELP_PATH to the elp binary to run this test", elp != null && !elp.isBlank());
    assertTrue("ERLANG_ELP_PATH is " + elp + ", which is not a file", new File(elp).isFile());

    // ELP breaks under a symlinked path, such as the temporary directory of macOS, so it gets the real path.
    Path project = FileUtil.createTempDirectory("elp-live", null, true).toPath().toRealPath().resolve("project");
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

    VirtualFile shop = file("src/shop.erl");
    myFixture.openFileInEditor(shop);
    ElpLspClientDescriptor descriptor = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(elp, ElpMode.ELP));
    LspServerManager.getInstance(getProject()).ensureServerStarted(ElpLspIntegrationProvider.class, descriptor);
    PlatformTestUtil.waitWithEventsDispatching("ELP did not start", () -> ElpEngine.ownsFeature(getProject(), shop, ElpEngine.Feature.NAVIGATION),
                                               TIMEOUT_SECONDS);

    // Definition: the remote call in price/1 goes to lookup/2 in shop_backend.erl; ELP answers once it has loaded the project.
    Editor editor = myFixture.getEditor();
    int call = editor.getDocument().getText().indexOf("lookup(shop_memory") + 2;
    PsiFile shopPsi = myFixture.getFile();
    AtomicReference<PsiElement[]> targets = new AtomicReference<>();
    PlatformTestUtil.waitWithEventsDispatching("ELP did not answer the definition", () -> {
      targets.set(definition(shopPsi, editor, call));
      return targets.get() != null;
    }, TIMEOUT_SECONDS);
    assertSize(1, targets.get());
    ErlangFunction lookup = assertInstanceOf(targets.get()[0], ErlangFunction.class);
    assertEquals("lookup", lookup.getName());
    assertEquals("shop_backend.erl", lookup.getContainingFile().getName());

    // A diagnostic of ELP in the module with the planted defects.
    myFixture.openFileInEditor(file("src/shop_defects.erl"));
    PlatformTestUtil.waitWithEventsDispatching("ELP published no diagnostics", () -> {
      List<HighlightInfo> infos = myFixture.doHighlighting();
      return ContainerUtil.exists(infos, info -> "function unused_function/0 is unused".equals(info.getDescription()));
    }, TIMEOUT_SECONDS);

    // Rename of the local function sum/2.
    myFixture.openFileInEditor(shop);
    Editor shopEditor = myFixture.getEditor();
    shopEditor.getCaretModel().moveToOffset(shopEditor.getDocument().getText().indexOf("\nsum([], Acc)") + 2);
    assertNull(ElpRenameHandler.renameAtCaret(getProject(), shopEditor, myFixture.getFile(), "add_up"));
    String text = shopEditor.getDocument().getText();
    assertTrue(text, text.contains("add_up([price(Name) || Name <- Names], 0)."));
    assertTrue(text, text.contains("add_up([Price | Rest], Acc) -> add_up(Rest, Acc + Price)."));

    // ELP extracts its eqWAlizer binary into the plugin's temporary directory of the session, which goes when the server stops.
    Path tmpDir = descriptor.getTmpDir();
    assertNotNull(tmpDir);
    PlatformTestUtil.waitWithEventsDispatching("ELP wrote nothing into " + tmpDir, () -> eqwalizerIn(tmpDir) != null, TIMEOUT_SECONDS);
    LspServerManager.getInstance(getProject()).stopServers(ElpLspIntegrationProvider.class);
    PlatformTestUtil.waitWithEventsDispatching(tmpDir + " is still there", () -> !Files.exists(tmpDir), TIMEOUT_SECONDS);
  }

  @Nullable
  private static String eqwalizerIn(@NotNull Path dir) {
    String[] names = dir.toFile().list();
    if (names == null) return null;
    for (String name : names) {
      if (name.startsWith("eqwalizer")) return name;
    }
    return null;
  }

  @Nullable
  private static PsiElement[] definition(@NotNull PsiFile file, @NotNull Editor editor, int offset) {
    return PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(() -> ReadAction.compute(
      () -> new ElpGotoDeclarationHandler().getGotoDeclarationTargets(file.findElementAt(offset), offset, editor))), 30_000);
  }

  @NotNull
  private VirtualFile file(@NotNull String relativePath) {
    VirtualFile file = myRoot.findFileByRelativePath(relativePath);
    assertNotNull(relativePath, file);
    return file;
  }
}
