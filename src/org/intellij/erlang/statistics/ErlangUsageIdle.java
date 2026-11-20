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

package org.intellij.erlang.statistics;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.util.Disposer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.TestOnly;

import java.awt.*;
import java.awt.event.AWTEventListener;

/**
 * The time of the last key or mouse input, so that the consent dialog does not open while the user types or clicks.
 * It listens only while a question is due and only writes a time stamp per event.
 */
final class ErlangUsageIdle {
  private static final long MASK = AWTEvent.KEY_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_WHEEL_EVENT_MASK;
  private static volatile long ourLastInput;
  private static final AWTEventListener LISTENER = event -> ourLastInput = System.currentTimeMillis();
  private static boolean ourInstalled;

  private ErlangUsageIdle() {
  }

  static synchronized void install(@NotNull Disposable parent) {
    if (ourInstalled || GraphicsEnvironment.isHeadless()) return;
    ourInstalled = true;
    ourLastInput = System.currentTimeMillis();
    Toolkit.getDefaultToolkit().addAWTEventListener(LISTENER, MASK);
    Disposer.register(parent, ErlangUsageIdle::uninstall);
  }

  static synchronized void uninstall() {
    if (!ourInstalled) return;
    ourInstalled = false;
    Toolkit.getDefaultToolkit().removeAWTEventListener(LISTENER);
  }

  static long idleMillis() {
    return System.currentTimeMillis() - ourLastInput;
  }

  @TestOnly
  static void setLastInputForTests(long time) {
    ourLastInput = time;
  }
}
