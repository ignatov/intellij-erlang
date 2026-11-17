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

package org.intellij.erlang.elp.lsp;

import com.intellij.platform.lsp.api.LspServerNotificationsHandler;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.MessageType;
import org.eclipse.lsp4j.ProgressParams;
import org.eclipse.lsp4j.WorkDoneProgressBegin;
import org.eclipse.lsp4j.WorkDoneProgressEnd;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.jsonrpc.messages.NotificationMessage;
import org.eclipse.lsp4j.jsonrpc.services.GenericEndpoint;
import org.eclipse.lsp4j.jsonrpc.services.ServiceEndpoints;
import org.intellij.erlang.elp.lsp.ElpLoadState.State;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;

import static org.intellij.erlang.elp.lsp.ElpLoadState.ERROR_GRACE_MS;
import static org.intellij.erlang.elp.lsp.ElpLoadState.HARD_TIMEOUT_MS;
import static org.intellij.erlang.elp.lsp.ElpLoadState.IDLE_TIMEOUT_MS;

/**
 * The load state of ELP on the sequences measured with ELP 1.1.0 (headless probe and IDE): a good load, a load without rebar3
 * that shows an error and still runs, a load without {@code erl} that never runs, and a long first open that reports progress.
 */
public class ElpLoadStateTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private final List<String> myChanges = new ArrayList<>();
  private ElpLoadState myState;
  private long myStart;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myState = new ElpLoadState((state, from, to) -> myChanges.add(from + "->" + to));
    myStart = System.currentTimeMillis();
    myState.started(myStart);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      myState.stopped();
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testGoodLoad() {
    assertFalse(myState.isRunning());
    myState.initialized();
    myState.progress("ELP/0", "begin", myStart + 13);
    myState.status("Loading", myStart + 593);
    assertFalse("the client runs, ELP still loads", myState.isRunning());
    myState.status("Running", myStart + 652);
    assertTrue(myState.isRunning());
    myState.tick(myStart + HARD_TIMEOUT_MS + 1);
    assertTrue(myState.isRunning());
    assertEquals(List.of("STARTING->LOADING", "LOADING->RUNNING"), myChanges);
    assertEquals(List.of(State.STARTING, State.LOADING, State.RUNNING), myState.history());
  }

  /** Without rebar3: an Error message at 0.143 s, then Loading and Running 30 ms later, on a fallback project. */
  public void testErrorBeforeRunningIsNotAFailureWhenRunningComes() {
    myState.initialized();
    myState.message(1, "No such file or directory (os error 2)", true);
    myState.status("Loading", myStart + 144);
    myState.status("Running", myStart + 174);
    myState.tick(myStart + ERROR_GRACE_MS + 1000);
    assertTrue(myState.isRunning());
    assertEquals("the error is told once ELP runs", "No such file or directory (os error 2)", myState.takeErrorToReport());
    assertNull(myState.takeErrorToReport());
  }

  /** Without erl: the OTP Info message, two Error messages and the progress ending within 17 ms; no elp/status at all. */
  public void testLoadWithoutErlFails() {
    myState.initialized();
    myState.message(3, "ELP version: 1.1.0+build-2026-10-14, OTP version: Could not find OTP: No such file or directory (os error 2)", true);
    myState.progress("ELP/0", "begin", myStart + 13);
    myState.message(1, "No such file or directory (os error 2)", true);
    myState.progress("ELP/0", "end", myStart + 17);
    myState.tick(myStart + ERROR_GRACE_MS - 1000);
    assertEquals(State.LOADING, myState.state());
    myState.tick(myStart + 17 + ERROR_GRACE_MS);
    assertEquals(State.FAILED, myState.state());
    assertTrue(myState.isOtpNotFound());
    assertEquals("No such file or directory (os error 2)", myState.firstError());
    assertEquals(2, myState.lastLogLines().size());
    assertNull("nothing runs, nothing to tell after a load", myState.takeErrorToReport());
  }

  /** A first open that fetches and builds dependencies reports progress all the time; it is not cut short. */
  public void testSlowLoadWithProgressIsNotAFailure() {
    myState.initialized();
    myState.progress("ELP/0", "begin", myStart + 500);
    for (long t = 1_000; t < 200_000; t += 5_000) {
      myState.progress("ELP/0", "report", myStart + t);
      myState.tick(myStart + t + 1);
    }
    myState.message(1, "watchdog: stalled 30489ms in update_configuration", false);
    myState.tick(myStart + 200_000);
    assertEquals(State.LOADING, myState.state());
    myState.status("Running", myStart + 201_000);
    assertTrue(myState.isRunning());
  }

  public void testQuietLoadFailsAfterTheIdleTimeout() {
    myState.initialized();
    myState.progress("ELP/0", "begin", myStart + 10);
    myState.progress("ELP/0", "end", myStart + 20);
    myState.tick(myStart + 20 + IDLE_TIMEOUT_MS - 1);
    assertEquals(State.LOADING, myState.state());
    myState.tick(myStart + 20 + IDLE_TIMEOUT_MS);
    assertEquals(State.FAILED, myState.state());
    assertNull(myState.firstError());
  }

  public void testLoadThatNeverEndsFailsAfterTheHardTimeout() {
    myState.initialized();
    myState.progress("ELP/0", "begin", myStart + 10);
    myState.progress("ELP/0", "report", myStart + HARD_TIMEOUT_MS - 1);
    myState.tick(myStart + HARD_TIMEOUT_MS);
    assertEquals(State.FAILED, myState.state());
  }

  /** A load that was called failed and then finishes after all: ELP takes over. */
  public void testRunningAfterAFailure() {
    myState.initialized();
    myState.tick(myStart + IDLE_TIMEOUT_MS);
    assertEquals(State.FAILED, myState.state());
    myState.status("Running", myStart + IDLE_TIMEOUT_MS + 5_000);
    assertTrue(myState.isRunning());
    assertEquals(List.of("STARTING->LOADING", "LOADING->FAILED", "FAILED->RUNNING"), myChanges);
  }

  /** A reload after a change: not serving while ELP loads again. Then the process ends. */
  public void testReloadAndStop() {
    myState.initialized();
    myState.status("Running", myStart + 600);
    myState.status("Loading", myStart + 9_000);
    assertFalse(myState.isRunning());
    myState.status("Running", myStart + 10_000);
    myState.status("ShuttingDown", myStart + 20_000);
    assertEquals(State.STOPPED, myState.state());
    myState.started(myStart + 30_000);
    assertEquals(State.STARTING, myState.state());
    assertNull("a new session forgets the old messages", myState.firstError());
  }

  /** ELP's own notification reaches the load state through the client's JSON-RPC method, as the platform registers it. */
  public void testElpStatusNotification() throws Exception {
    ElpLsp4jClient client = new ElpLsp4jClient(handler(new ArrayList<>()), myState);
    var methods = ServiceEndpoints.getSupportedMethods(ElpLsp4jClient.class);
    assertTrue(methods.keySet().toString(), methods.containsKey("elp/status"));
    MessageJsonHandler json = new MessageJsonHandler(methods);
    GenericEndpoint endpoint = new GenericEndpoint(client);
    myState.initialized();
    for (String status : List.of("Loading", "Running")) {
      NotificationMessage message = (NotificationMessage)json.parseMessage(
        "{\"jsonrpc\":\"2.0\",\"method\":\"elp/status\",\"params\":{\"status\":\"" + status + "\"}}");
      endpoint.notify(message.getMethod(), message.getParams());
    }
    assertTrue(myState.isRunning());
  }

  /**
   * Info messages, such as "ELP version: ..." at every start, go to the log instead of a balloon; an Error is kept for the plugin's
   * own notification; a Warning is left to the platform. Progress is seen on its way to the platform.
   */
  public void testMessagesAndProgressOnTheirWayToThePlatform() {
    List<String> passed = new ArrayList<>();
    LspServerNotificationsHandler handler = ElpLsp4jClient.intercept(handler(passed), myState);
    handler.showMessage(new MessageParams(MessageType.Info, "ELP version: 1.1.0+build-2026-10-14, OTP version: Erlang/OTP 29"));
    handler.showMessage(new MessageParams(MessageType.Error, "No such file or directory (os error 2)"));
    handler.showMessage(new MessageParams(MessageType.Warning, "a warning"));
    handler.logMessage(new MessageParams(MessageType.Error, "Failed to set up project shop for parsing"));
    handler.notifyProgress(new ProgressParams(Either.forLeft("ELP/0"), Either.forLeft(new WorkDoneProgressBegin())));
    assertEquals(List.of("showMessage a warning", "logMessage Failed to set up project shop for parsing", "notifyProgress"), passed);
    assertEquals("No such file or directory (os error 2)", myState.firstError());
    myState.tick(myStart + ERROR_GRACE_MS + 1000);
    assertEquals("the progress is still on", State.STARTING, myState.state());
    handler.notifyProgress(new ProgressParams(Either.forLeft("ELP/0"), Either.forLeft(new WorkDoneProgressEnd())));
    myState.tick(System.currentTimeMillis() + ERROR_GRACE_MS);
    assertEquals(State.FAILED, myState.state());
  }

  private static LspServerNotificationsHandler handler(List<String> calls) {
    return (LspServerNotificationsHandler)Proxy.newProxyInstance(
      LspServerNotificationsHandler.class.getClassLoader(), new Class<?>[]{LspServerNotificationsHandler.class}, (proxy, method, args) -> {
        if (method.getDeclaringClass() == Object.class) return method.invoke(calls, args);
        Object argument = args != null && args.length == 1 ? args[0] : null;
        calls.add(method.getName() + (argument instanceof MessageParams params ? " " + params.getMessage() : ""));
        return null;
      });
  }
}
