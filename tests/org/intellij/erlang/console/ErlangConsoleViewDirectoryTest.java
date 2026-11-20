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

package org.intellij.erlang.console;

import com.intellij.execution.process.ProcessHandler;
import com.intellij.openapi.util.Disposer;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.ByteArrayOutputStream;
import java.io.OutputStream;

public class ErlangConsoleViewDirectoryTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testStoppedConsoleDoesNotHideRunningOne() {
    FakeProcessHandler first = startConsole();
    FakeProcessHandler second = startConsole();

    ErlangConsoleView found = ErlangConsoleViewDirectory.getInstance().getConsole(getProject());
    assertNotNull(found);
    assertTrue(found.isRunning());
    FakeProcessHandler foundHandler = found == first.myView ? first : second;
    foundHandler.destroyProcess();
    assertTrue(foundHandler.isProcessTerminated());
    assertFalse(found.isRunning());

    ErlangConsoleView afterStop = ErlangConsoleViewDirectory.getInstance().getConsole(getProject());
    assertNotNull(afterStop);
    assertTrue("a stopped console hides the running one", afterStop.isRunning());
  }

  public void testMostRecentlyStartedOrUsedRunningConsoleIsChosen() {
    FakeProcessHandler first = startConsole();
    FakeProcessHandler second = startConsole();
    assertSame(second.myView, ErlangConsoleViewDirectory.getInstance().getConsole(getProject()));

    first.myView.append("ok.");
    first.myView.execute();
    assertSame(first.myView, ErlangConsoleViewDirectory.getInstance().getConsole(getProject()));

    first.destroyProcess();
    assertSame(second.myView, ErlangConsoleViewDirectory.getInstance().getConsole(getProject()));
  }

  public void testNoConsoleWhenAllStopped() {
    FakeProcessHandler first = startConsole();
    FakeProcessHandler second = startConsole();
    first.destroyProcess();
    second.destroyProcess();
    assertNull(ErlangConsoleViewDirectory.getInstance().getConsole(getProject()));
  }

  @NotNull
  private FakeProcessHandler startConsole() {
    ErlangConsoleView view = new ErlangConsoleView(getProject());
    Disposer.register(getTestRootDisposable(), view);
    FakeProcessHandler handler = new FakeProcessHandler(view);
    view.attachToProcess(handler);
    handler.startNotify();
    return handler;
  }

  private static final class FakeProcessHandler extends ProcessHandler {
    private final ErlangConsoleView myView;
    private final ByteArrayOutputStream myInput = new ByteArrayOutputStream();

    private FakeProcessHandler(@NotNull ErlangConsoleView view) {
      myView = view;
    }

    @Override
    protected void destroyProcessImpl() {
      notifyProcessTerminated(0);
    }

    @Override
    protected void detachProcessImpl() {
      notifyProcessDetached();
    }

    @Override
    public boolean detachIsDefault() {
      return false;
    }

    @Override
    public OutputStream getProcessInput() {
      return myInput;
    }
  }
}
