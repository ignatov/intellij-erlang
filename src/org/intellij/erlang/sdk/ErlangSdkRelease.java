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

package org.intellij.erlang.sdk;

import com.intellij.util.text.VersionComparatorUtil;
import org.jetbrains.annotations.NotNull;

public enum ErlangSdkRelease {
  R16B03("5.10.4"),
  R16B02("5.10.3"),
  R16B01("5.10.2"),
  R16B  ("5.10.1"),
  R16A  ("5.10"  ),
  R15B03("5.9.3" ),
  R15B02("5.9.2" ),
  R15B01("5.9.1" ),
  R15B  ("5.9"   ),
  R14B04("5.8.5" ),
  R14B03("5.8.4" ),
  R14B02("5.8.3" ),
  R14B01("5.8.2" ),
  R14B  ("5.8.1" ),
  R14A  ("5.8"   ),
  R13B04("5.7.5" ),
  R13B03("5.7.4" ),
  R13B02("5.7.3" ),
  R13B01("5.7.2" ),
  R13B  ("5.7.1" ),
  R13A  ("5.7"   );

  @NotNull
  private final String myVersion;

  private ErlangSdkRelease(@NotNull String version) {
    myVersion = version;
  }

  @NotNull
  public String getVersion() {
    return myVersion;
  }

  public boolean needBifCompletion(@NotNull String moduleName) {
    return VersionComparatorUtil.compare(myVersion, "5.10") < 0  || "lager".equals(moduleName) || moduleName.isEmpty();
  }
}
