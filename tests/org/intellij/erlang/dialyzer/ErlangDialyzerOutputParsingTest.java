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

package org.intellij.erlang.dialyzer;

import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.util.text.StringUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangDialyzerOutputParsingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // Output of `dialyzer --plt <plt> dz_smoke.erl` (OTP 27 and OTP 29): file:line:column, long messages wrapped. Issue #1350.
  public void testWrappedWarningWithColumn() {
    String stdout = "  Checking whether the PLT /tmp/dz/e.plt is up-to-date... yes\n" +
                    "  Proceeding with analysis...\n" +
                    "dz_smoke.erl:4:1: Function f/0 has no local return\n" +
                    "dz_smoke.erl:4:10: The call dz_smoke:g\n" +
                    "         (1) will never return since it differs in the 1st argument from the success typing arguments: \n" +
                    "         (atom())\n" +
                    "dz_smoke.erl:6:1: Function g/1 has no local return\n" +
                    "dz_smoke.erl:6:11: Guard test is_atom\n" +
                    "         (X :: 1) can never succeed\n" +
                    " done in 0m0.09s\n" +
                    "done (warnings were emitted)\n";
    ProcessOutput output = new ProcessOutput();
    output.appendStdout(stdout);
    ErlangDialyzerExternalAnnotator.State state = new ErlangDialyzerExternalAnnotator.State(
      getProject(), "/sdk/bin/dialyzer", null, "/project/src/dz_smoke.erl", "/project");
    ErlangDialyzerExternalAnnotator.State result = new ErlangDialyzerExternalAnnotator().doAnnotate(state, s -> output);
    assertNotNull(result);
    assertEquals("Problem{myLine=4, myDescription='Function f/0 has no local return'}\n" +
                 "Problem{myLine=4, myDescription='The call dz_smoke:g(1) will never return since it differs in the 1st argument from the success typing arguments: (atom())'}\n" +
                 "Problem{myLine=6, myDescription='Function g/1 has no local return'}\n" +
                 "Problem{myLine=6, myDescription='Guard test is_atom(X :: 1) can never succeed'}",
                 StringUtil.join(result.problems, Object::toString, "\n"));
  }

  // Before OTP 24 there is no column; the "Unknown functions" list after the warnings is not part of them.
  public void testLineOnlyWarningsAndUnknownFunctions() {
    String stdout = "  Proceeding with analysis...\n" +
                    "dz_smoke.erl:4: Function f/0 has no local return\n" +
                    "dz_smoke.erl:6: Call to missing or unexported function foo:bar/1\n" +
                    " done in 0m0.09s\n" +
                    "Unknown functions:\n" +
                    "  foo:bar/1 (dz_smoke.erl:6:3)\n" +
                    "done (warnings were emitted)\n";
    ProcessOutput output = new ProcessOutput();
    output.appendStdout(stdout);
    ErlangDialyzerExternalAnnotator.State state = new ErlangDialyzerExternalAnnotator.State(
      getProject(), "/sdk/bin/dialyzer", null, "/project/src/dz_smoke.erl", "/project");
    ErlangDialyzerExternalAnnotator.State result = new ErlangDialyzerExternalAnnotator().doAnnotate(state, s -> output);
    assertNotNull(result);
    assertEquals("Problem{myLine=4, myDescription='Function f/0 has no local return'}\n" +
                 "Problem{myLine=6, myDescription='Call to missing or unexported function foo:bar/1'}",
                 StringUtil.join(result.problems, Object::toString, "\n"));
  }
}
