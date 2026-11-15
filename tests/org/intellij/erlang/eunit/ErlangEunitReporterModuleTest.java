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

package org.intellij.erlang.eunit;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.UsefulTestCase;
import org.intellij.erlang.sdk.ErlangSdkRelease;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.Arrays;

public class ErlangEunitReporterModuleTest extends UsefulTestCase {
  public void testOtp27AndLaterGetOtp27Module() throws IOException {
    assertReporterModule(ErlangSdkRelease.V_27_0, "otp_27_plus");
    assertReporterModule(ErlangSdkRelease.V_29_0, "otp_27_plus");
  }

  public void testOlderReleasesGetPreOtp27Module() throws IOException {
    assertReporterModule(ErlangSdkRelease.V_R16B, "otp_pre_27");
    assertReporterModule(ErlangSdkRelease.V_25_0, "otp_pre_27");
  }

  private void assertReporterModule(ErlangSdkRelease release, String beamDirectory) throws IOException {
    File directory = FileUtil.createTempDirectory(getTestName(true), null);
    ErlangEunitReporterModule.putReporterModuleTo(directory, release);
    byte[] actual = Files.readAllBytes(new File(directory, "eunit_teamcity.beam").toPath());
    try (InputStream expected = getClass().getResourceAsStream("/eunit/beams/" + beamDirectory + "/eunit_teamcity.beam")) {
      assertNotNull(expected);
      assertTrue(release.toString(), Arrays.equals(expected.readAllBytes(), actual));
    }
  }
}
