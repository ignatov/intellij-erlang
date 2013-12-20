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
  public static final String MODULE_BEAM = MODULE_NAME + ".beam";

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
