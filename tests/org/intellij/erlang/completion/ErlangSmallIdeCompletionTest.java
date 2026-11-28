/*
 * Copyright 2012-2015 Sergey Ignatov
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

import java.util.List;

public class ErlangSmallIdeCompletionTest extends ErlangCompletionTestBase {
  public ErlangSmallIdeCompletionTest() {
    super(true);
  }

  public void testIncludeOtpIncludeDirectoryCompletion() {
    myFixture.configureByFiles("otp-include-directory/src/includeuser.erl",
      "otp-include-directory/include/include.hrl",
      "otp-include-directory/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }

  public void testIncludeRebarConfigIncludePath() {
    myFixture.configureByFiles("rebar-config-include-path/src/includeuser.erl",
      "rebar-config-include-path/include/for-include-user/include.hrl",
      "rebar-config-include-path/rebar.config",
      "rebar-config-include-path/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }

  public void testIncludeOtpIncludeDirectoryNested() {
    myFixture.configureByFiles("otp-include-directory-nested/src/directory/includeuser.erl",
      "otp-include-directory-nested/include/include.hrl",
      "otp-include-directory-nested/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }

  public void testIncludeOtpIncludeDirectoryWithBuildCopy() {
    myFixture.configureByFiles("otp-include-directory-with-build-copy/testapp/src/includeuser.erl",
      "otp-include-directory-with-build-copy/testapp/include/include.hrl",
      "otp-include-directory-with-build-copy/testapp/_build/default/lib/testapp/ebin/testapp.app",
      "otp-include-directory-with-build-copy/testapp/src/testapp.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.INCLUDES, "include.hrl");
  }
  
  public void testIncludeOtpIncludeDirectoryNotInRoot() {
    myFixture.configureByFiles("otp-include-not-in-root/src/directory/src/includeuser.erl",
      "otp-include-not-in-root/src/directory/include/include.hrl",
      "otp-include-not-in-root/src/test.app.src");
    doTestVariantsInner(CompletionType.BASIC, 1, CheckType.EXCLUDES, "include.hrl");
  }

  // issue 1731: completion inside 3000 nested parentheses, which erlc compiles, must not overflow the stack
  public void testDeeplyNestedParentheses() {
    String prefix = "-module(parens).\n-export([f/0]).\nf() -> " + "(".repeat(1690);
    myFixture.configureByText("parens.erl", prefix + "<caret>" + "(".repeat(1310) + "1" + ")".repeat(3000) + ".\n");
    myFixture.completeBasic();
  }

  public void testFunctionsInsideDeeplyNestedParentheses() {
    String prefix = "-module(parens).\n-export([f/0]).\nfoo_bar() -> 1.\nfoo_baz() -> 2.\nf() -> " + "(".repeat(3000);
    myFixture.configureByText("parens.erl", prefix + "foo_<caret>" + ")".repeat(3000) + ".\n");
    myFixture.completeBasic();
    List<String> variants = myFixture.getLookupElementStrings();
    assertNotNull(variants);
    assertContainsElements(variants, "foo_bar", "foo_baz");
  }
}
