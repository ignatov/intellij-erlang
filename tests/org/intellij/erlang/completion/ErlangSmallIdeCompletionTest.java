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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.util.PlatformUtilsCore;

public class ErlangSmallIdeCompletionTest extends ErlangCompletionTestBase {
  public ErlangSmallIdeCompletionTest() {
    super(PlatformUtilsCore.PYCHARM_PREFIX);
  }

  public void testIncludeOtpIncludeDirectoryCompletion() throws Throwable {
    myFixture.configureByFiles("otp-include-directory/src/includeuser.erl",
      "otp-include-directory/include/include.hrl",
      "otp-include-directory/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }

  public void testIncludeRebarConfigIncludePath() throws Throwable {
    myFixture.configureByFiles("rebar-config-include-path/src/includeuser.erl",
      "rebar-config-include-path/include/for-include-user/include.hrl",
      "rebar-config-include-path/rebar.config",
      "rebar-config-include-path/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }

  public void testIncludeOtpIncludeDirectoryNested() throws Throwable {
    myFixture.configureByFiles("otp-include-directory-nested/src/directory/includeuser.erl",
      "otp-include-directory-nested/include/include.hrl",
      "otp-include-directory-nested/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }
  
  public void testIncludeOtpIncludeDirectoryNotInRoot() throws Throwable {
    myFixture.configureByFiles("otp-include-not-in-root/src/directory/src/includeuser.erl",
      "otp-include-not-in-root/src/directory/include/include.hrl",
      "otp-include-not-in-root/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EXCLUDES, "include.hrl");
  }
}
