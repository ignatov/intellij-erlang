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
import com.intellij.util.PlatformUtils;

/**
 * @author savenko
 */
public class ErlangSmallIdeCompletionTest extends ErlangCompletionTestBase {
  public ErlangSmallIdeCompletionTest() {
    super(PlatformUtils.PYCHARM_PREFIX);
  }

  public void testIncludeOtpIncludeDirectoryCompletion() throws Throwable {
    myFixture.configureByFiles("otp-include-directory/src/includeuser.erl", "otp-include-directory/include/include.hrl");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }

  public void testIncludeRebarConfigIncludePath() throws Throwable {
    myFixture.configureByFiles("rebar-config-include-path/src/includeuser.erl",
      "rebar-config-include-path/include/for-include-user/include.hrl",
      "rebar-config-include-path/rebar.config");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }
}
