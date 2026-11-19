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
import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangDialyzerTimeoutTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final List<Notification> myNotifications = new ArrayList<>();

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(Notification notification) {
        myNotifications.add(notification);
      }
    });
  }

  public void testDialyzerErrorIsNotified() {
    ErlangDialyzerExternalAnnotator.State state = annotate(output("dialyzer: Could not find the PLT\n", false));
    assertEmpty(state.problems);
    assertSize(1, myNotifications);
  }

  public void testTimeoutIsReportedNotSwallowed() {
    // Dialyzer printed its progress, then was killed at the 10 s limit of ErlangSystemUtil.
    ErlangDialyzerExternalAnnotator.State state = annotate(output("  Checking whether the PLT /tmp/big.plt is up-to-date... yes\n" +
                                                                  "  Proceeding with analysis...\n", true));
    assertTrue("A dialyzer run cut off by the timeout gives no problem and no notification",
               !state.problems.isEmpty() || !myNotifications.isEmpty());
  }

  public void testTimeoutIsNotifiedOncePerFile() {
    assertEmpty(annotate(output("", true)).problems);
    assertEmpty(annotate(output("", true)).problems);
    assertSize(1, myNotifications);
  }

  private static ProcessOutput output(String stdout, boolean timeout) {
    ProcessOutput output = new ProcessOutput();
    output.appendStdout(stdout);
    if (timeout) output.setTimeout();
    return output;
  }

  private ErlangDialyzerExternalAnnotator.State annotate(ProcessOutput output) {
    ErlangDialyzerExternalAnnotator.State state = new ErlangDialyzerExternalAnnotator.State(
      getProject(), "/sdk/bin/dialyzer", null, "/project/src/foo.erl", "/project");
    ErlangDialyzerExternalAnnotator.State result = new ErlangDialyzerExternalAnnotator().doAnnotate(state, s -> output);
    assertSame(state, result);
    return result;
  }
}
