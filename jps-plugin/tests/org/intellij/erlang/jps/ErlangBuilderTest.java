/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.jps;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.PathUtilRt;
import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsElement;
import org.jetbrains.jps.model.library.JpsOrderRootType;
import org.jetbrains.jps.model.library.JpsTypedLibrary;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.util.JpsPathUtil;

/**
 * @author ignatov
 */
public class ErlangBuilderTest extends JpsBuildTestCase {
  public void testSimple() throws Exception {
    String depFile = createFile("src/simple.erl", "-module(simple). foo() -> ok.");
    String moduleName = "m";
    addModule(moduleName, PathUtilRt.getParentPath(depFile));
    rebuildAll();
    assertCompiled(moduleName, "simple.beam");
  }

  private void assertCompiled(@NotNull String moduleName, @NotNull String fileName) {
    String absolutePath = getAbsolutePath("out/production/" + moduleName);
    assertNotNull(FileUtil.findFileInProvidedPath(absolutePath, fileName));
  }

  @Override
  protected JpsSdk<JpsDummyElement> addJdk(String name, String path) {
    String homePath = "/usr/lib/erlang";
    String versionString = "R16B";
    JpsTypedLibrary<JpsSdk<JpsDummyElement>> jdk = myModel.getGlobal().addSdk(versionString, homePath, versionString, JpsErlangSdkType.INSTANCE);
    jdk.addRoot(JpsPathUtil.pathToUrl(homePath), JpsOrderRootType.COMPILED);
    return jdk.getProperties();
  }

  @Override
  protected <T extends JpsElement> JpsModule addModule(@NotNull String moduleName,
                                                       @NotNull String[] srcPaths,
                                                       @Nullable String outputPath,
                                                       @Nullable String testOutputPath,
                                                       @NotNull JpsSdk<T> sdk) {
    return addModule(moduleName, srcPaths, outputPath, testOutputPath, sdk, JpsErlangModuleType.INSTANCE);
  }
}
