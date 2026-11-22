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

package org.intellij.erlang.update;

/**
 * The stored state of the notification about a new version of the plugin. It is application-level and is kept in a
 * file that does not roam.
 */
public class ErlangUpdateNoticeState {
  // the time of the last question to the plugin repository, in milliseconds since the epoch; 0: never
  public long lastCheck;
  // the time when the notification was last shown, in milliseconds since the epoch; 0: never
  public long lastShown;
  // the user chose "Don't show again"
  public boolean dontShowAgain;
}
