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

package org.intellij.erlang.resolve;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;
import java.util.List;

// -include in a source file of an OTP application of the SDK (issue #459): OTP compiles
// lib/<app>/src with -I ../include, so the header is found in lib/<app>/include
public class ErlangSdkIncludeResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String SDK_HOME = "testData/resolve/include/otpSdkSource/";

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk(SDK_HOME, ErlangSdkRelease.V_27_0);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testIncludeFromOtpAppIncludeDirectory() {
    VirtualFile sdkSource = LocalFileSystem.getInstance().refreshAndFindFileByIoFile(new File(SDK_HOME, "lib/stdlib-6.2.2.3/src/zip.erl").getAbsoluteFile());
    assertNotNull(sdkSource);
    myFixture.configureFromExistingVirtualFile(sdkSource);
    List<ErlangFile> includedFiles = ErlangPsiImplUtil.getDirectlyIncludedFiles((ErlangFile) myFixture.getFile());
    assertEquals("Include attribute resolved to unexpected number of files.", 1, includedFiles.size());
    assertEquals("zip.hrl", includedFiles.get(0).getName());
  }
}
