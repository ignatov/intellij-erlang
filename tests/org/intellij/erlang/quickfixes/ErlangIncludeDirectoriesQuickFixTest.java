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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangIncludeDirectoriesInspection;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangIncludeDirectoriesQuickFixTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MESSAGE = "Some 'include' folders are not marked as include directories.";

  private static final LightProjectDescriptor DEFAULT_DESCRIPTOR = new LightProjectDescriptor();

  private static final LightProjectDescriptor ERLANG_MODULE_DESCRIPTOR = new LightProjectDescriptor() {
    @Override
    public @NotNull String getModuleTypeId() {
      return ErlangModuleType.getInstance().getId();
    }
  };

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/include_directories/";
  }

  @Override
  protected @NotNull LightProjectDescriptor getProjectDescriptor() {
    return getTestName(true).startsWith("erlangModule") ? ERLANG_MODULE_DESCRIPTOR : DEFAULT_DESCRIPTOR;
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myFixture.enableInspections(ErlangIncludeDirectoriesInspection.class);
  }

  // #842: the warning is shown for a module of any type, so "Mark folders" must mark the folder for it too.
  public void testNonErlangModuleMarkFolders() {
    assertFalse(ModuleType.get(myFixture.getModule()) == ErlangModuleType.getInstance());
    doTest();
  }

  public void testErlangModuleMarkFolders() {
    assertSame(ErlangModuleType.getInstance(), ModuleType.get(myFixture.getModule()));
    doTest();
  }

  private void doTest() {
    myFixture.copyDirectoryToProject("", "");
    myFixture.configureFromTempProjectFile("test.erl");
    VirtualFile includeDir = myFixture.findFileInTempDir("include");
    assertNotNull(includeDir);

    assertTrue(ContainerUtil.exists(myFixture.doHighlighting(), info -> MESSAGE.equals(info.getDescription())));
    IntentionAction markFolders = ContainerUtil.find(myFixture.getAllQuickFixes(), fix -> "Mark folders".equals(fix.getText()));
    assertNotNull(markFolders);
    myFixture.launchAction(markFolders);

    List<VirtualFile> includeDirectories = ErlangIncludeDirectoryUtil.getIncludeDirectories(myFixture.getModule());
    assertEquals("'Mark folders' did not mark the include folder of module type " + ModuleType.get(myFixture.getModule()).getId(),
                 List.of(includeDir), includeDirectories);
    assertFalse(ContainerUtil.exists(myFixture.doHighlighting(), info -> MESSAGE.equals(info.getDescription())));
  }
}
