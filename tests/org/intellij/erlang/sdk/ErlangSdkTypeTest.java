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

package org.intellij.erlang.sdk;

import junit.framework.TestCase;
import org.intellij.erlang.jps.model.JpsErlangSdkType;

import java.lang.reflect.Method;

public class ErlangSdkTypeTest extends TestCase {
  // #1216: the Windows suggestion must be an Erlang/OTP home (bin/erl and bin/erlc are executable) or null.
  public void testWindowsSuggestionIsValidSdkHomeOrNull() throws Exception {
    Method method = ErlangSdkType.class.getDeclaredMethod("suggestWindowsErlangPath");
    method.setAccessible(true);
    String home = (String)method.invoke(null);
    if (home == null) return;
    assertTrue("Not an Erlang SDK home: " + home,
               JpsErlangSdkType.getByteCodeInterpreterExecutable(home).canExecute() &&
               JpsErlangSdkType.getByteCodeCompilerExecutable(home).canExecute());
  }
}
