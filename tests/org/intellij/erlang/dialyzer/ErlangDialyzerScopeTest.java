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

package org.intellij.erlang.dialyzer;

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInspection.ex.InspectionProfileImpl;
import com.intellij.codeInspection.ex.ToolsImpl;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.profile.codeInspection.ProjectInspectionProfileManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.scope.packageSet.NamedScope;
import com.intellij.psi.search.scope.packageSet.NamedScopesHolder;
import com.intellij.psi.search.scope.packageSet.PackageSet;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangDialyzerScopeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String FILE_NAME = "foo.erl";

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
    setUpProjectSdk();
    myFixture.enableInspections(ErlangDialyzerInspection.class);
  }

  public void testEnabledByDefault() {
    assertNotNull(collectInformation());
  }

  public void testEnabledOnlyForScope() {
    ToolsImpl tools = dialyzerTools();
    tools.getDefaultState().setEnabled(false);
    tools.prependTool(scopeOfFile(), tools.getTool(), true, HighlightDisplayLevel.ERROR);
    assertNotNull(collectInformation());
  }

  public void testDisabledForScope() {
    ToolsImpl tools = dialyzerTools();
    tools.prependTool(scopeOfFile(), tools.getTool(), false, HighlightDisplayLevel.ERROR);
    assertNull(collectInformation());
  }

  private ErlangDialyzerExternalAnnotator.State collectInformation() {
    PsiFile file = myFixture.configureByText(FILE_NAME, "-module(foo).\n");
    return new ErlangDialyzerExternalAnnotator().collectInformation(file);
  }

  @NotNull
  private ToolsImpl dialyzerTools() {
    InspectionProfileImpl profile = ProjectInspectionProfileManager.getInstance(getProject()).getCurrentProfile();
    ToolsImpl tools = profile.getToolsOrNull(ErlangDialyzerInspection.INSPECTION_SHORT_NAME, getProject());
    assertNotNull(tools);
    return tools;
  }

  @NotNull
  private static NamedScope scopeOfFile() {
    return new NamedScope("Dialyzer test scope", new PackageSet() {
      @Override
      public boolean contains(@NotNull PsiFile file, NamedScopesHolder holder) {
        return FILE_NAME.equals(file.getName());
      }

      @NotNull
      @Override
      public PackageSet createCopy() {
        return this;
      }

      @NotNull
      @Override
      public String getText() {
        return "file:" + FILE_NAME;
      }

      @Override
      public int getNodePriority() {
        return 0;
      }
    });
  }
}
