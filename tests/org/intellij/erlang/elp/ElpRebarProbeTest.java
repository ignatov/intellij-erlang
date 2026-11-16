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

package org.intellij.erlang.elp;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.PlatformTestUtil;
import com.intellij.util.EnvironmentUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.util.Arrays;

/**
 * A probe for the ELP probe workflow, not a regression test: how the rebar3 lookup and {@code rebar3 version} of
 * {@link ElpPreflight} treat the real rebar3 escript, and the rebar3.cmd wrapper next to it, on this OS. Prints lines that start
 * with {@code PROBE}; skipped unless {@code PROBE_REBAR3} names the escript.
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ElpRebarProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testProbeRebar3() throws Exception {
    String escript = System.getenv("PROBE_REBAR3");
    Assume.assumeTrue("Set PROBE_REBAR3 to the rebar3 escript", escript != null && !escript.isBlank());
    String path = EnvironmentUtil.getValue("PATH");
    File wrapper = new File(escript + ".cmd");
    probe("PATH has the rebar3 directory " + (path != null && Arrays.asList(path.split(File.pathSeparator)).contains(new File(escript).getParent())));
    probe("findRebar3(none, PATH) " + ElpPreflight.findRebar3(null, path));
    probe("findRebar3(escript) " + ElpPreflight.findRebar3(escript, null));
    probe("findRebar3(rebar3.cmd, exists " + wrapper.isFile() + ") " + ElpPreflight.findRebar3(wrapper.getPath(), null));
    probe("ELP_REBAR3 for rebar3.cmd " + ElpServerEnvironment.build(null, null, wrapper.getPath(), null).get(ElpServerEnvironment.ELP_REBAR3));
    for (String rebar3 : new String[]{escript, wrapper.getPath()}) {
      String[] result = PlatformTestUtil.waitForFuture(
        ApplicationManager.getApplication().executeOnPooledThread(() -> ElpPreflight.rebar3Version(rebar3, path)), 60_000);
      probe("rebar3Version(" + new File(rebar3).getName() + ") version " + result[0] + " error " + result[1]);
    }
  }

  private static void probe(String line) {
    System.out.println("PROBE " + line);
  }
}
