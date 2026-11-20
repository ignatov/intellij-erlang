/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.ui.popup.ListPopup;
import com.intellij.openapi.ui.popup.ListPopupStep;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.ui.UiInterceptors;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.inspection.ErlangUnresolvedIncludeInspection;
import org.intellij.erlang.inspection.ErlangUnresolvedIncludeLibInspection;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangFindIncludeQuickFixTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/find_include/" + getTestName(true) + "/";
  }

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
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnresolvedIncludeInspection.class, ErlangUnresolvedIncludeLibInspection.class);
    ApplicationManager.getApplication().runWriteAction(() -> ErlangFacet.createFacet(myFixture.getModule()));
  }

  public void testSimple() {
    doIncludeTest("inc", "test.erl", "inc/inc.hrl");
  }

  public void testIncludeWithPath() {
    doIncludeTest("hdr", "test.erl", "hdr/inc.hrl");
    myFixture.checkResultByFile("test-after.erl");
  }

  public void testIncludeLibWithPath() {
    doNothingOfferedTest("test.erl", "hdr/inc.hrl");
  }

  public void testIncludeLibWithMatchingPath() {
    doIncludeTest("libs", "test.erl", "libs/app/include/inc.hrl");
    myFixture.checkResultByFile("test-after.erl");
  }

  public void testHeaderOutsideModule() {
    doNothingOfferedTest("test.erl");
  }

  private void doNothingOfferedTest(String... files) {
    List<String> offered = launchFindInclude(files);
    assertEquals("files offered by the popup: " + offered, 0, offered.size());
    assertEmpty(ErlangIncludeDirectoryUtil.getIncludeDirectories(myFixture.getModule()));
    myFixture.checkResultByFile(files[0]);
  }

  private void doIncludeTest(String expectedIncludePath, String ... files) {
    List<String> offered = launchFindInclude(files);
    assertEquals("files offered by the popup: " + offered, 1, offered.size());
    assertTrue(offered.getFirst(), offered.getFirst().endsWith(files[files.length - 1]));
    List<VirtualFile> includeDirectories = ErlangIncludeDirectoryUtil.getIncludeDirectories(myFixture.getModule());
    assertEquals(1, includeDirectories.size());
    assertTrue(includeDirectories.getFirst().getPath().endsWith(expectedIncludePath));
    List<String> unresolved = ContainerUtil.mapNotNull(myFixture.doHighlighting(), info ->
      info.getDescription() != null && info.getDescription().startsWith("Unresolved include") ? info.getDescription() : null);
    assertEmpty(unresolved);
  }

  private List<String> launchFindInclude(String... files) {
    myFixture.configureByFiles(files);
    List<String> offered = new ArrayList<>();
    UiInterceptors.registerPossible(getTestRootDisposable(), new UiInterceptors.UiInterceptor<>(ListPopup.class) {
      @Override
      protected void doIntercept(ListPopup popup) {
        //noinspection unchecked
        ListPopupStep<Object> step = (ListPopupStep<Object>) popup.getListStep();
        for (Object value : step.getValues()) offered.add(step.getTextFor(value));
        if (!step.getValues().isEmpty()) step.onChosen(step.getValues().getFirst(), true);
      }
    });
    launchIntention("Find include");
    PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    return offered;
  }
}
