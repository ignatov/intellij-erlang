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

import com.intellij.notification.Notification;
import com.intellij.notification.Notifications;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.junit.Assume;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

public class ErlangDialyzerTimeoutTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final List<Notification> myNotifications = new ArrayList<>();
  private File myDirectory;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    Assume.assumeFalse("The fake dialyzer is a shell script", SystemInfo.isWindows);
    myDirectory = FileUtil.createTempDirectory("dialyzer", null);
    getProject().getMessageBus().connect(getTestRootDisposable()).subscribe(Notifications.TOPIC, new Notifications() {
      @Override
      public void notify(Notification notification) {
        myNotifications.add(notification);
      }
    });
  }

  public void testDialyzerErrorIsNotified() throws IOException {
    ErlangDialyzerExternalAnnotator.State state = annotate("echo 'dialyzer: Could not find the PLT'\n");
    assertEmpty(state.problems);
    assertSize(1, myNotifications);
  }

  public void testTimeoutIsReportedNotSwallowed() throws IOException {
    // Dialyzer prints its progress, then analyses for longer than the 10 s limit of ErlangSystemUtil.
    ErlangDialyzerExternalAnnotator.State state = annotate("echo '  Checking whether the PLT /tmp/big.plt is up-to-date... yes'\n" +
                                                           "echo '  Proceeding with analysis...'\n" +
                                                           "exec sleep 30\n");
    assertTrue("A dialyzer run cut off by the timeout gives no problem and no notification",
               !state.problems.isEmpty() || !myNotifications.isEmpty());
  }

  public void testTimeoutIsNotifiedOncePerFile() throws IOException {
    String script = "exec sleep 30\n";
    assertEmpty(annotate(script).problems);
    assertEmpty(annotate(script).problems);
    assertSize(1, myNotifications);
  }

  private ErlangDialyzerExternalAnnotator.State annotate(String script) throws IOException {
    File dialyzer = new File(myDirectory, "dialyzer");
    Files.writeString(dialyzer.toPath(), "#!/bin/sh\n" + script, StandardCharsets.UTF_8);
    assertTrue(dialyzer.setExecutable(true));
    File module = new File(myDirectory, "foo.erl");
    Files.writeString(module.toPath(), "-module(foo).\n", StandardCharsets.UTF_8);
    ErlangDialyzerExternalAnnotator.State state = new ErlangDialyzerExternalAnnotator.State(
      getProject(), dialyzer.getPath(), null, module.getPath(), myDirectory.getPath());
    ErlangDialyzerExternalAnnotator.State result = new ErlangDialyzerExternalAnnotator().doAnnotate(state);
    assertSame(state, result);
    return result;
  }
}
