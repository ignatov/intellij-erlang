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

package org.intellij.erlang.parser;

import org.intellij.erlang.ErlangParserDefinition;

public class ErlangAppParserTest extends ErlangParserTestBase {
  public ErlangAppParserTest() {
    super("parser", "app", new ErlangParserDefinition());
  }

  public void testRtbCoordinator()  throws Exception { doTest(true, false); }
  public void testRebar()           throws Exception { doTest(true, false); }
  public void test254()             throws Exception { doTest(true, true);  }
}
