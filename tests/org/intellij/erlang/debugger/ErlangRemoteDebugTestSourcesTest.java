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

package org.intellij.erlang.debugger;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.PsiTestUtil;
import org.intellij.erlang.debugger.remote.ErlangRemoteDebugRunConfiguration;
import org.intellij.erlang.debugger.xdebug.ErlangDebugLocationResolver;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.IOException;

public class ErlangRemoteDebugTestSourcesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private VirtualFile myTestRoot;
  private VirtualFile myProductionModule;
  private VirtualFile myTestModule;

  @Override
  protected String getTestDataPath() {
    return "testData/debugger/remoteTestSources/";
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myProductionModule = myFixture.copyFileToProject("m.erl", "m.erl");
    myTestRoot = createTestRoot();
    myTestModule = myFixture.copyFileToProject("m_helper.erl", "test/m_helper.erl");
    PsiTestUtil.addSourceRoot(getModule(), myTestRoot, true);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      if (myTestRoot != null) {
        PsiTestUtil.removeSourceRoot(getModule(), myTestRoot);
      }
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  // The resolver is built the way ErlangXDebugProcess builds it for the session of the configuration.
  public void testRemoteNodeSessionResolvesModuleUnderTestSources() {
    ErlangRemoteDebugRunConfiguration configuration = createRemoteConfiguration();
    ErlangDebugLocationResolver resolver = new ErlangDebugLocationResolver(getProject(),
                                                                           configuration.getConfigurationModule().getModule(),
                                                                           configuration.isUseTestCodePath());

    assertEquals(myProductionModule, resolver.resolveModuleFile("m"));
    assertEquals(myTestModule, resolver.resolveModuleFile("m_helper"));
  }

  private ErlangRemoteDebugRunConfiguration createRemoteConfiguration() {
    ErlangRemoteDebugRunConfiguration configuration = new ErlangRemoteDebugRunConfiguration(getProject(), "remote");
    configuration.setModule(getModule());
    configuration.setRemoteErlangNodeName("target@localhost");
    configuration.setCookie("c");
    return configuration;
  }

  private VirtualFile createTestRoot() throws IOException {
    return myFixture.getTempDirFixture().findOrCreateDir("test");
  }
}
