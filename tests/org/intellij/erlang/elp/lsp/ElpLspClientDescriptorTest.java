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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.lsp.api.LspClientDescriptor;
import com.intellij.platform.lsp.api.LspIntegrationProvider;
import com.intellij.platform.lsp.api.customization.*;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.EnvironmentUtil;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpEnvironment;
import org.intellij.erlang.elp.ElpMode;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

public class ElpLspClientDescriptorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String SDK_HOME = new File("testData/mockSdk-R15B02").getAbsolutePath();
  private static final String REBAR = new File("/opt/rebar-tools/rebar3").getPath();

  private ElpSettings.ElpSettingsState myOldElpState;
  private String myOldRebarPath;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldElpState = new ElpSettings.ElpSettingsState();
    myOldElpState.path = old.path;
    myOldElpState.mode = old.mode;
    myOldRebarPath = RebarSettings.getInstance(getProject()).getRebarPath();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldElpState);
      RebarSettings.getInstance(getProject()).setRebarPath(myOldRebarPath);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ApplicationManager.getApplication().runWriteAction(
          (Computable<Sdk>)() -> ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_R15B02));
      }
    };
  }

  public void testCommandLineAndPath() throws Exception {
    setUpProjectSdk();
    RebarSettings.getInstance(getProject()).setRebarPath(REBAR);
    VirtualFile root = file("src/a.erl").getParent().getParent();
    ElpLspClientDescriptor descriptor =
      new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration("/opt/elp/bin/elp", ElpMode.DIAGNOSTICS));

    GeneralCommandLine commandLine = onPooledThread(descriptor::createCommandLine);

    assertEquals("/opt/elp/bin/elp", commandLine.getExePath());
    assertEquals(List.of("server"), commandLine.getParametersList().getList());
    assertEquals(root.getPath(), commandLine.getWorkDirectory().getPath());
    String expectedHead = new File(SDK_HOME, "bin").getPath() + File.pathSeparator + new File(REBAR).getParent();
    String inherited = EnvironmentUtil.getValue("PATH");
    assertEquals(inherited == null ? expectedHead : expectedHead + File.pathSeparator + inherited,
                 commandLine.getEnvironment().get("PATH"));
  }

  public void testPathIsInheritedWithoutSdkAndRebar() throws Exception {
    RebarSettings.getInstance(getProject()).setRebarPath("");
    assertEquals(EnvironmentUtil.getValue("PATH"), onPooledThread(() -> ElpEnvironment.serverPath(getProject())));
  }

  public void testOnlyErlangModulesAndHeadersAreSupported() {
    ElpLspClientDescriptor descriptor = descriptor(ElpMode.DIAGNOSTICS);
    assertTrue(descriptor.isSupportedFile(file("src/a.erl")));
    assertTrue(descriptor.isSupportedFile(file("include/a.hrl")));
    assertFalse(descriptor.isSupportedFile(file("rebar.config")));
    assertFalse(descriptor.isSupportedFile(file("src/x.app.src")));
    assertFalse(descriptor.isSupportedFile(file("x.escript")));
    assertEquals("erlang", descriptor.getLanguageId(file("src/b.erl")));
  }

  public void testDiagnosticsModeKeepsOnlyDiagnosticsAndQuickFixes() throws Exception {
    LspCustomization customization = descriptor(ElpMode.DIAGNOSTICS).getLspCustomization();
    List<String> notDisabled = new ArrayList<>();
    for (Method method : LspCustomization.class.getMethods()) {
      if (method.getParameterCount() != 0 || !method.getName().endsWith("Customizer")) continue;
      Object customizer = method.invoke(customization);
      if (!customizer.getClass().getSimpleName().endsWith("Disabled")) notDisabled.add(method.getName());
    }
    notDisabled.sort(null);
    assertEquals(List.of("getCodeActionsCustomizer", "getCommandsCustomizer", "getDiagnosticsCustomizer", "getSymbolKindCustomizer"),
                 notDisabled);

    assertInstanceOf(customization.getDiagnosticsCustomizer(), LspDiagnosticsSupport.class);
    LspCodeActionsSupport codeActions = assertInstanceOf(customization.getCodeActionsCustomizer(), LspCodeActionsSupport.class);
    assertTrue(codeActions.getQuickFixesSupport());
    assertFalse(codeActions.getIntentionActionsSupport());
    assertFalse(codeActions.getInlineRefactoringSupport());
    assertFalse(codeActions.getExtractRefactoringSupport());
  }

  public void testDiagnosticsModeClientCapabilities() {
    // The platform derives only the semantic tokens capability from the customization; it still advertises the other
    // features, but sends no requests for the switched off ones.
    var textDocument = descriptor(ElpMode.DIAGNOSTICS).getClientCapabilities().getTextDocument();
    assertNotNull(textDocument.getPublishDiagnostics());
    assertNotNull(textDocument.getCodeAction());
    assertNull(textDocument.getSemanticTokens());
  }

  public void testAllModeKeepsThePlatformDefaults() {
    ElpLspClientDescriptor descriptor = descriptor(ElpMode.ALL);
    LspCustomization customization = descriptor.getLspCustomization();
    assertEquals(LspCustomization.class, customization.getClass());
    assertInstanceOf(customization.getHoverCustomizer(), LspHoverSupport.class);
    assertInstanceOf(customization.getCompletionCustomizer(), LspCompletionSupport.class);
    assertInstanceOf(customization.getGoToDefinitionCustomizer(), LspGoToDefinitionSupport.class);
    var textDocument = descriptor.getClientCapabilities().getTextDocument();
    assertNotNull(textDocument.getCompletion());
    assertNotNull(textDocument.getHover());
  }

  public void testProviderStartsElpOnlyForErlangFilesOnceAPathIsSet() {
    VirtualFile module = file("src/a.erl");
    configure("", ElpMode.DIAGNOSTICS);
    assertEmpty(fileOpened(module));

    configure("/opt/elp/bin/elp", ElpMode.ALL);
    List<LspClientDescriptor> started = fileOpened(module);
    assertSize(1, started);
    ElpLspClientDescriptor descriptor = assertInstanceOf(started.get(0), ElpLspClientDescriptor.class);
    assertEquals(new ElpConfiguration("/opt/elp/bin/elp", ElpMode.ALL), descriptor.getConfiguration());
    assertSize(1, descriptor.getRoots());

    assertSize(1, fileOpened(file("include/a.hrl")));
    assertEmpty(fileOpened(file("rebar.config")));
    assertEmpty(fileOpened(file("src/x.app.src")));
    assertEmpty(fileOpened(file("x.escript")));
  }

  private void configure(String path, ElpMode mode) {
    ElpSettings.ElpSettingsState state = new ElpSettings.ElpSettingsState();
    state.path = path;
    state.mode = mode.name();
    ElpSettings.getInstance().loadState(state);
  }

  private List<LspClientDescriptor> fileOpened(VirtualFile file) {
    List<LspClientDescriptor> started = new ArrayList<>();
    LspIntegrationProvider.LspClientStarter starter = started::add;
    ReadAction.run(() -> new ElpLspIntegrationProvider().fileOpened(getProject(), file, starter));
    return started;
  }

  private ElpLspClientDescriptor descriptor(ElpMode mode) {
    VirtualFile root = file("src/" + mode + ".erl").getParent();
    return new ElpLspClientDescriptor(getProject(), root, new ElpConfiguration("/opt/elp/bin/elp", mode));
  }

  private VirtualFile file(String path) {
    return myFixture.addFileToProject(path, "").getVirtualFile();
  }

  private static <T> T onPooledThread(Callable<T> callable) throws Exception {
    return ApplicationManager.getApplication().executeOnPooledThread(callable).get();
  }
}
