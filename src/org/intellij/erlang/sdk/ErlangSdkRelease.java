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

package org.intellij.erlang.sdk;

import com.intellij.util.text.VersionComparatorUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class ErlangSdkRelease {
  public static final ErlangSdkRelease V_R14A = new ErlangSdkRelease("R14A", "5.8");
  public static final ErlangSdkRelease V_R15B02 = new ErlangSdkRelease("R15B02", "5.9.2");
  public static final ErlangSdkRelease V_R16A = new ErlangSdkRelease("R16A", "5.10");
  public static final ErlangSdkRelease V_R16B = new ErlangSdkRelease("R16B", "5.10.1");
  public static final ErlangSdkRelease V_17_0 = new ErlangSdkRelease("17", "6.0");
  public static final ErlangSdkRelease V_18_0 = new ErlangSdkRelease("18", "7.0");

  private static final Pattern VERSION_PATTERN = Pattern.compile("Erlang/OTP (\\S+) \\[erts-(\\S+)]");

  private final String myOtpRelease;
  private final String myErtsVersion;

  public ErlangSdkRelease(@NotNull String otpRelease, @NotNull String ertsVersion) {
    myOtpRelease = otpRelease;
    myErtsVersion = ertsVersion;
  }

  @NotNull
  public String getOtpRelease() {
    return myOtpRelease;
  }

  @NotNull
  public String getErtsVersion() {
    return myErtsVersion;
  }

  public boolean isNewerThan(@NotNull ErlangSdkRelease other) {
    return VersionComparatorUtil.compare(myErtsVersion, other.myErtsVersion) > 0;
  }

  public boolean needBifCompletion(@NotNull String moduleName) {
    return V_R16A.isNewerThan(this) || "lager".equals(moduleName) || moduleName.isEmpty();
  }

  @Override
  public String toString() {
    return "Erlang/OTP " + getOtpRelease() + " [erts-" + getErtsVersion() + "]";
  }

  @Nullable
  static ErlangSdkRelease fromString(@Nullable String versionString) {
    Matcher m = versionString != null ? VERSION_PATTERN.matcher(versionString) : null;
    return m != null && m.matches() ? new ErlangSdkRelease(m.group(1), m.group(2)) : null;
  }
}
