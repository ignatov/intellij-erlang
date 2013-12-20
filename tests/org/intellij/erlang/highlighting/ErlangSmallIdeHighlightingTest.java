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

package org.intellij.erlang.highlighting;

import com.intellij.util.PlatformUtilsCore;

public class ErlangSmallIdeHighlightingTest extends ErlangHighlightingTestBase {
  public ErlangSmallIdeHighlightingTest() {
    super(PlatformUtilsCore.PYCHARM_PREFIX);
  }

  public void testIncludeFromOtpIncludeDirResolve() throws Exception {
    enableUnresolvedMacroInspection();
    doTest("smallIdeOtpInclude/src/IncludeResolve.erl",
      "smallIdeOtpInclude/include/testapp.hrl",
      "smallIdeOtpInclude/src/test.app.src");
  }

  public void testIncludeFromRebarConfigIncludeDirResolve() throws Exception {
    enableUnresolvedMacroInspection();
    doTest("smallIdeRebarConfigInclude/src/IncludeResolve.erl",
      "smallIdeRebarConfigInclude/include/custom/testapp.hrl",
      "smallIdeRebarConfigInclude/src/test.app.src",
      "smallIdeRebarConfigInclude/rebar.config");
  }
}
