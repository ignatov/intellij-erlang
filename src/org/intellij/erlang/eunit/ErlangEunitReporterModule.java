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
  private static final String MODULE_SOURCE = MODULE_NAME + ".erl";
  private static final String[] SOURCE_FILES = {MODULE_SOURCE, MODULE_NAME + ".hrl"};

  // The reporter is bundled as source and compiled by the Erlang that runs the tests: a .beam compiled by one
  // Erlang/OTP release is refused by releases some years older or newer (OTP 27 and later refuse one compiled by
  // OTP 22). The expression finds the source on the code path, where the directory of putReporterModuleTo is put,
  // writes the .beam next to it and loads it.
  public static final String LOAD_EXPRESSION =
    "ReporterSource = code:where_is_file(\"" + MODULE_SOURCE + "\"), " +
    "{ok, " + MODULE_NAME + "} = compile:file(ReporterSource, [{outdir, filename:dirname(ReporterSource)}, report_errors]), " +
    "{module, " + MODULE_NAME + "} = code:load_abs(filename:rootname(ReporterSource))";

  public static void putReporterModuleTo(@NotNull File directory) throws IOException {
    for (String fileName : SOURCE_FILES) {
      URL url = ResourceUtil.getResource(ErlangEunitReporterModule.class.getClassLoader(), "/eunit/src", fileName);
      if (url == null) {
        throw new IOException("Failed to locate eunit reporter module.");
      }

      try (BufferedInputStream inputStream = new BufferedInputStream(URLUtil.openStream(url))) {
        try (BufferedOutputStream outputStream = new BufferedOutputStream(new FileOutputStream(new File(directory, fileName)))) {
          FileUtil.copy(inputStream, outputStream);
        }
      }
    }
  }
}
