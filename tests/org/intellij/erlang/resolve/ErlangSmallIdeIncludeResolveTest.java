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

package org.intellij.erlang.resolve;

import com.intellij.util.PlatformUtilsCore;

public class ErlangSmallIdeIncludeResolveTest extends ErlangIncludeResolveTestBase {
  public ErlangSmallIdeIncludeResolveTest() {
    super(PlatformUtilsCore.PHP_PREFIX);
  }

  public void testRelativeToIncludeDirectory() throws Exception {
    doTest("src/testmodule.erl", "include/testinclude.hrl", "src/test.app.src");
  }

  public void testRelativeToIncludeDirectoryNested() throws Exception {
    doTest("src/directory/testmodule.erl", "include/testinclude.hrl", "src/test.app.src");
  }

  public void testRelativeToIncludeDirectoryFromTest() throws Exception {
    doTest("test/testmodule.erl", "src/dummy.erl", "include/testinclude.hrl", "src/test.app.src");
  }

  // 'include' folders, which are not in otp app root are not considered as include paths by rebar, we should do the same
  public void testRelativeToIncludeDirectoryNotInOtpAppRoot() throws Exception {
    doTestWithExpectedResolveFailure("src/directory/testmodule.erl", "src/include/testinclude.hrl", "src/test.app.src");
  }

  public void testIncludePathsInRebarConfig() throws Exception {
    doTest("src/testmodule.erl", "include/forTestModule/testinclude.hrl", "rebar.config", "src/test.app.src");
  }
}
