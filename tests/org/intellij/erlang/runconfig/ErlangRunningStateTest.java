/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.runconfig;

import junit.framework.TestCase;

import java.util.List;

public class ErlangRunningStateTest extends TestCase {
  public void testSimpleArguments() {
    assertArguments("one two", "one", "two");
  }

  public void testQuotedArguments() {
    assertArguments("\"hello world\" \"\" 'quoted atom'", "\"hello world\"", "\"\"", "'quoted atom'");
  }

  public void testStructuredArguments() {
    assertArguments("{ok, 1} [one, two] #{key => value} <<1, 2>>",
                    "{ok, 1}", "[one, two]", "#{key => value}", "<<1, 2>>");
  }

  public void testNestedQuotedArgument() {
    assertArguments("{ok, \"hello world\"} ~s/foo bar/", "{ok, \"hello world\"}", "~s/foo bar/");
  }

  private static void assertArguments(String params, String... expected) {
    ErlangRunningState.ErlangEntryPoint entryPoint =
      ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("sample start", params);

    assertNotNull(entryPoint);
    assertEquals(List.of(expected), entryPoint.getArgsList());
  }
}
