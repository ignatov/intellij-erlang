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
import java.awt.event.MouseEvent;

/**
 * The time of the last key or mouse input, so that the consent dialog does not open while the user types or clicks.
 * It listens only while a question is due and only writes a time stamp per event.
 */
final class ErlangUsageIdle {
  // the motion events are there for drags; a pointer that only moves is not input
  static final long MASK = AWTEvent.KEY_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_WHEEL_EVENT_MASK |
                           AWTEvent.MOUSE_MOTION_EVENT_MASK;
  private static volatile long ourLastInput;
  private static final AWTEventListener LISTENER = event -> {
    if (event.getID() != MouseEvent.MOUSE_MOVED) ourLastInput = System.currentTimeMillis();
  };
  private static boolean ourInstalled;
  private static Disposable ourDisposable;

  private ErlangUsageIdle() {
  }

  // the idle time counts from here; without a display there is no input, and only the time stamp is set
  static synchronized void install(@NotNull Disposable parent) {
    if (ourInstalled) return;
    ourInstalled = true;
    ourLastInput = System.currentTimeMillis();
    if (!GraphicsEnvironment.isHeadless()) Toolkit.getDefaultToolkit().addAWTEventListener(LISTENER, MASK);
    if (ourDisposable == null) {
      ourDisposable = ErlangUsageIdle::uninstall;
      Disposer.register(parent, ourDisposable);
    }
  }

  static synchronized void uninstall() {
    if (!ourInstalled) return;
    ourInstalled = false;
    if (!GraphicsEnvironment.isHeadless()) Toolkit.getDefaultToolkit().removeAWTEventListener(LISTENER);
  }

  static synchronized boolean isInstalled() {
    return ourInstalled;
  }

  static long idleMillis() {
    return System.currentTimeMillis() - ourLastInput;
  }

  @TestOnly
  static synchronized void setInstalledForTests(boolean installed, long lastInput) {
    ourInstalled = installed;
    ourLastInput = lastInput;
  }

  @TestOnly
  static void dispatchForTests(@NotNull AWTEvent event) {
    LISTENER.eventDispatched(event);
  }

  @TestOnly
  static void setLastInputForTests(long time) {
    ourLastInput = time;
  }
}
