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
import org.jetbrains.annotations.NotNull;

import java.io.*;
import java.net.URL;

public class ErlangEunitReporterModule {
  private ErlangEunitReporterModule() {
  }

  public static final String MODULE_NAME = "eunit_teamcity";
  private static final String MODULE_BEAM = MODULE_NAME + ".beam";

  public static void putReporterModuleTo(@NotNull File directory) throws IOException {
    URL moduleUrl = ResourceUtil.getResource(ErlangEunitReporterModule.class, "/eunit", MODULE_BEAM);
    if (moduleUrl == null) {
      throw new IOException("Failed to locate eunit reporter module.");
    }

    BufferedInputStream inputStream = new BufferedInputStream(URLUtil.openStream(moduleUrl));
    try {
      BufferedOutputStream outputStream = new BufferedOutputStream(new FileOutputStream(new File(directory, MODULE_BEAM)));
      try {
        FileUtil.copy(inputStream, outputStream);
      } finally {
        outputStream.close();
      }
    } finally {
      inputStream.close();
    }
  }
}
