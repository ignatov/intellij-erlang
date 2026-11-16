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

package org.intellij.erlang.elp.distribution;

import com.intellij.openapi.progress.EmptyProgressIndicator;
import com.intellij.openapi.util.io.NioFiles;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.fixtures.BasePlatformTestCase;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Downloads the pinned build for this machine from GitHub through the plugin's own code path, checks it and runs
 * {@code elp version}. Skipped unless {@code ERLANG_ELP_DOWNLOAD_TEST=1}, because it goes to the network and fetches 18 to 42 MB.
 * {@code ERLANG_ELP_DOWNLOAD_DIR} keeps the installation there for a look at it (extended attributes, signature); otherwise a
 * temporary directory is used and removed. {@code ERLANG_ELP_DOWNLOAD_OTP} is the OTP release to choose the build for, 28 by
 * default:
 * {@code ERLANG_ELP_DOWNLOAD_TEST=1 ./gradlew test --rerun --tests 'org.intellij.erlang.elp.distribution.ElpRealDownloadTest'}
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ElpRealDownloadTest extends BasePlatformTestCase {
  public void testDownloadPinnedBuildForThisMachine() throws Exception {
    Assume.assumeTrue("Set ERLANG_ELP_DOWNLOAD_TEST=1 to download ELP from GitHub", "1".equals(System.getenv("ERLANG_ELP_DOWNLOAD_TEST")));
    String keepDir = System.getenv("ERLANG_ELP_DOWNLOAD_DIR");
    String otp = System.getenv("ERLANG_ELP_DOWNLOAD_OTP");
    Path baseDir = keepDir != null && !keepDir.isBlank() ? Path.of(keepDir) : Files.createTempDirectory("erlang-elp-download");
    try {
      ElpRelease release = ElpRelease.pinned();
      ElpAssetChoice choice = ElpAssetChoice.choose(release, ElpPlatform.current(), true, otp != null ? Integer.parseInt(otp) : 28);
      assertNotNull(choice.message(), choice.asset());
      ElpInstaller installer = new ElpInstaller(baseDir, release, release.downloadUrl(), ElpInstaller::runVersion);
      Path elp = ElpInstallerTest.install(installer, choice.asset(), new EmptyProgressIndicator());
      assertEquals(installer.executable(choice.asset()), elp);
      assertTrue(Files.isExecutable(elp));
      System.out.println("ELP installed: " + elp + " (" + Files.size(elp) + " bytes) from " + release.url(choice.asset()));
    }
    finally {
      if (keepDir == null || keepDir.isBlank()) NioFiles.deleteRecursively(baseDir);
    }
  }
}
