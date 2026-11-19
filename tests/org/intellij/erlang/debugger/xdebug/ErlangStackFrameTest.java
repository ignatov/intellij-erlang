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

package org.intellij.erlang.debugger.xdebug;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.ui.ColoredTextContainer;
import com.intellij.ui.SimpleTextAttributes;
import com.intellij.xdebugger.XDebugSession;
import org.intellij.erlang.debugger.node.ErlangDebuggerEventListener;
import org.intellij.erlang.debugger.node.ErlangDebuggerNode;
import org.intellij.erlang.debugger.node.ErlangTraceElement;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.lang.reflect.Proxy;
import java.util.Collections;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

public class ErlangStackFrameTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/debugger/";
  }

  // #1347: the 2026.x debugger computes the frame title on a background coroutine thread, outside a read action.
  public void testPresentationComputedOutsideReadAction() throws Exception {
    myFixture.configureByFile("dbg_smoke.erl");
    ErlangDebuggerNode node = new ErlangDebuggerNode(proxy(ErlangDebuggerEventListener.class));
    try {
      ErlangXDebugProcess process = new ErlangXDebugProcess(proxy(XDebugSession.class), node,
                                                            new ErlangDebugLocationResolver(getProject(), null, false));
      ErlangTraceElement trace = new ErlangTraceElement(new OtpErlangLong(1), "dbg_smoke", "inner",
                                                        new OtpErlangList(new OtpErlangLong(20)), Collections.emptyList());
      ErlangStackFrame frame = new ErlangStackFrame(process, trace);
      assertNotNull(frame.getSourcePosition());

      StringBuilder title = new StringBuilder();
      Future<?> future = ApplicationManager.getApplication().executeOnPooledThread(() -> frame.customizePresentation(new TextContainer(title)));
      future.get(10, TimeUnit.SECONDS);
      assertEquals("dbg_smoke:inner", title.toString());
    }
    finally {
      node.stop();
    }
  }

  @SuppressWarnings("unchecked")
  private static <T> T proxy(@NotNull Class<T> type) {
    return (T)Proxy.newProxyInstance(ErlangStackFrameTest.class.getClassLoader(), new Class[]{type}, (p, m, a) -> null);
  }

  private static class TextContainer implements ColoredTextContainer {
    private final StringBuilder myText;

    TextContainer(StringBuilder text) {
      myText = text;
    }

    @Override
    public void append(@NotNull String fragment, @NotNull SimpleTextAttributes attributes) {
      myText.append(fragment);
    }

    @Override
    public void setIcon(Icon icon) {
    }
  }
}
