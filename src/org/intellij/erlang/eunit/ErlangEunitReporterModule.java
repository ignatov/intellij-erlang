/*
 * Copyright 2012-2014 Sergey Ignatov
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
import com.intellij.util.ResourceUtil;
import com.intellij.util.io.URLUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.net.URL;

public class ErlangEunitReporterModule {
  private ErlangEunitReporterModule() {
  }

  public static final String MODULE_NAME = "eunit_teamcity";
  private static final String MODULE_BEAM = MODULE_NAME + ".beam";

  // Without a known release the module for OTP 27 and later is used: it also loads on OTP 26, while the one for
  // older releases is refused from OTP 27 on.
  public static void putReporterModuleTo(@NotNull File directory, @Nullable ErlangSdkRelease release) throws IOException {
    String beamDirectory = release != null ? release.getBundledBeamDirectory() : "otp_27_plus";
    URL moduleUrl = ResourceUtil.getResource(ErlangEunitReporterModule.class.getClassLoader(),
                                             "/eunit/beams/" + beamDirectory, MODULE_BEAM);
    if (moduleUrl == null) {
      throw new IOException("Failed to locate eunit reporter module.");
    }

    try (BufferedInputStream inputStream = new BufferedInputStream(URLUtil.openStream(moduleUrl))) {
      try (BufferedOutputStream outputStream = new BufferedOutputStream(new FileOutputStream(new File(directory, MODULE_BEAM)))) {
        FileUtil.copy(inputStream, outputStream);
      }
    }
  }
}
