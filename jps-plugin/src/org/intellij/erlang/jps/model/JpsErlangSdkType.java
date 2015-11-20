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

package org.intellij.erlang.jps.model;

import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.TestOnly;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsElementFactory;
import org.jetbrains.jps.model.JpsElementTypeWithDefaultProperties;
import org.jetbrains.jps.model.library.sdk.JpsSdkType;

import java.io.File;

public class JpsErlangSdkType extends JpsSdkType<JpsDummyElement> implements JpsElementTypeWithDefaultProperties<JpsDummyElement> {
  public static final JpsErlangSdkType INSTANCE = new JpsErlangSdkType();

  public static final String BYTECODE_INTERPRETER = "erl";
  public static final String BYTECODE_COMPILER = "erlc";
  public static final String SCRIPT_INTERPRETER = "escript";

  private static final String TESTS_SDK_PATH_PROPERTY = "erlang.sdk.path";
  private static final String DEFAULT_TESTS_SDK_PATH = "/usr/lib/erlang/";

  @NotNull
  public static File getByteCodeInterpreterExecutable(@NotNull String sdkHome) {
    return getSdkExecutable(sdkHome, BYTECODE_INTERPRETER);
  }

  @NotNull
  public static File getByteCodeCompilerExecutable(@NotNull String sdkHome) {
    return getSdkExecutable(sdkHome, BYTECODE_COMPILER);
  }

  @NotNull
  public static File getScriptInterpreterExecutable(@NotNull String sdkHome) {
    return getSdkExecutable(sdkHome, SCRIPT_INTERPRETER);
  }

  @NotNull
  @Override
  public JpsDummyElement createDefaultProperties() {
    return JpsElementFactory.getInstance().createDummyElement();
  }

  @NotNull
  public static String getExecutableFileName(@NotNull String executableName) {
    return SystemInfo.isWindows ? executableName + ".exe" : executableName;
  }

  @TestOnly
  @NotNull
  public static String getTestsSdkPath() {
    String sdkPathFromProperty = StringUtil.nullize(System.getProperty(TESTS_SDK_PATH_PROPERTY));
    return StringUtil.notNullize(sdkPathFromProperty, DEFAULT_TESTS_SDK_PATH);
  }

  @TestOnly
  @NotNull
  public static String getSdkConfigurationFailureMessage() {
    return "Failed to setup an Erlang SDK at " + getTestsSdkPath() +
           "\nUse " + TESTS_SDK_PATH_PROPERTY + " system property to set sdk path";
  }

  @NotNull
  private static File getSdkExecutable(@NotNull String sdkHome, @NotNull String command) {
    return new File(new File(sdkHome, "bin"), getExecutableFileName(command));
  }
}
