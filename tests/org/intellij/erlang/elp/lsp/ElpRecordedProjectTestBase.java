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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.impl.VfsRootAccess;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.TempDirTestFixture;
import com.intellij.testFramework.fixtures.impl.TempDirTestFixtureImpl;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpPreflight;
import org.intellij.erlang.elp.ElpServedRoots;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * The fixture project {@code testData/elp/project} on the local file system, as a content root of the test module, with a
 * {@link FakeElpClient} that answers with the recorded answers of the real ELP. Our code finds that client where it finds a running
 * ELP, so the tests run the same code paths as the IDE, from the request to what the user sees.
 * Starts in the {@code ELP} position with the server running.
 */
public abstract class ElpRecordedProjectTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  protected static final String ELP_PATH = "/opt/elp/bin/elp";

  protected VirtualFile myRoot;
  protected ElpRecordedAnswers myAnswers;
  protected FakeElpServer myServer;
  protected FakeElpClient myClient;
  /** How many times our code looked for a running ELP client. */
  protected final AtomicInteger myClientLookups = new AtomicInteger();
  protected ElpLspClientDescriptor myDescriptor;

  private ElpSettings.ElpSettingsState myOldState;
  private String myOldPathProperty;
  private String myOldModeProperty;
  private String myOldEqwalizerProperty;

  @Override
  protected TempDirTestFixture createTempDirTestFixture() {
    // The LSP client finds files by file:// URIs, so the project must be on the local file system.
    return new TempDirTestFixtureImpl();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/elp";
  }

  /** An Erlang SDK, as a user's project has: the plugin resolves {@code io:format} and the other OTP calls into it. */
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    // The fake client stands in for ELP; the real provider would run the preflight on the opened files and refuse the project.
    ElpLspIntegrationProvider.ignoreOpenedFilesInTests(getTestRootDisposable());
    setUpProjectSdk();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
    myOldPathProperty = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    myOldModeProperty = System.getProperty(ElpConfiguration.MODE_PROPERTY);
    myOldEqwalizerProperty = System.getProperty(ElpConfiguration.EQWALIZER_PROPERTY);
    System.clearProperty(ElpConfiguration.PATH_PROPERTY);
    System.clearProperty(ElpConfiguration.MODE_PROPERTY);
    System.clearProperty(ElpConfiguration.EQWALIZER_PROPERTY);

    myRoot = myFixture.copyDirectoryToProject("project", "");
    VfsRootAccess.allowRootAccess(getTestRootDisposable(), myRoot.getPath());
    PsiTestUtil.addSourceContentToRoots(getModule(), myRoot);
    IndexingTestUtil.waitUntilIndexesAreReady(getProject());

    myDescriptor = new ElpLspClientDescriptor(getProject(), myRoot, new ElpConfiguration(ELP_PATH, ElpMode.ELP));
    String shopUri = myDescriptor.getFileUri(file("src/shop.erl"));
    assertTrue(shopUri, shopUri.endsWith("/src/shop.erl"));
    myAnswers = new ElpRecordedAnswers(shopUri.substring(0, shopUri.length() - "/src/shop.erl".length()));
    myServer = new FakeElpServer(myAnswers);
    myClient = new FakeElpClient(myDescriptor, myServer, myAnswers.initializeResult());
    ElpLspServerStatus.setClientsForTests(project -> {
      myClientLookups.incrementAndGet();
      return project == getProject() ? List.of(myClient) : List.of();
    }, getTestRootDisposable());
    position(ElpMode.ELP);
    // ELP has served the root: it reached Running once.
    ElpPreflight.trustInTests(true, getTestRootDisposable());
    ElpServedRoots.getInstance().setRootsForTests(List.of(myRoot.getPath()), getTestRootDisposable());
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldState);
      restore(ElpConfiguration.PATH_PROPERTY, myOldPathProperty);
      restore(ElpConfiguration.MODE_PROPERTY, myOldModeProperty);
      restore(ElpConfiguration.EQWALIZER_PROPERTY, myOldEqwalizerProperty);
      if (myRoot != null) PsiTestUtil.removeContentEntry(getModule(), myRoot);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  /** Chooses the engine position with a path to {@code elp}, as the settings page stores it. */
  protected static void position(@NotNull ElpMode mode) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = ELP_PATH;
    state.mode = mode.name();
    ElpSettings.getInstance().loadState(state);
  }

  @NotNull
  protected VirtualFile file(@NotNull String relativePath) {
    VirtualFile file = myRoot.findFileByRelativePath(relativePath);
    assertNotNull(relativePath, file);
    return file;
  }

  @NotNull
  protected PsiFile psiFile(@NotNull String relativePath) {
    PsiFile file = PsiManager.getInstance(getProject()).findFile(file(relativePath));
    assertNotNull(relativePath, file);
    return file;
  }

  @NotNull
  protected static Document document(@NotNull VirtualFile file) {
    Document document = FileDocumentManager.getInstance().getDocument(file);
    assertNotNull(file.getPath(), document);
    return document;
  }

  protected int offset(@NotNull String relativePath, @NotNull Position position) {
    Document document = document(file(relativePath));
    return document.getLineStartOffset(position.getLine()) + position.getCharacter();
  }

  /** Opens the file of a recorded request in the editor with the caret where the recorder asked. */
  @NotNull
  protected Editor openAt(@NotNull ElpRecordedAnswers.Case recorded) {
    myFixture.configureFromTempProjectFile(recorded.file());
    myFixture.getEditor().getCaretModel().moveToOffset(offset(recorded.file(), recorded.position()));
    return myFixture.getEditor();
  }

  /** What the user sees of a place: the path in the project, the line and column, and the text there. */
  @NotNull
  protected String describe(@NotNull VirtualFile file, @NotNull TextRange range) {
    Document document = document(file);
    int line = document.getLineNumber(range.getStartOffset());
    String path = VfsUtilCore.getRelativePath(file, myRoot);
    return (path != null ? path : file.getPath()) + ":" + (line + 1) + ":" + (range.getStartOffset() - document.getLineStartOffset(line) + 1) +
           " " + document.getText(range);
  }

  @NotNull
  protected String describe(@NotNull String uri, @NotNull Range range) {
    VirtualFile file = myDescriptor.findFileByUri(uri);
    assertNotNull(uri, file);
    Document document = document(file);
    int start = document.getLineStartOffset(range.getStart().getLine()) + range.getStart().getCharacter();
    int end = document.getLineStartOffset(range.getEnd().getLine()) + range.getEnd().getCharacter();
    return describe(file, new TextRange(start, end));
  }

  /** Runs {@code action} on a pooled thread, as the platform runs the code that may block on the server, and waits for it. */
  protected static <T> T onPooledThread(@NotNull Callable<T> action) {
    return PlatformTestUtil.waitForFuture(ApplicationManager.getApplication().executeOnPooledThread(action), 30_000);
  }

  private static void restore(@NotNull String key, @Nullable String value) {
    if (value == null) System.clearProperty(key);
    else System.setProperty(key, value);
  }
}
