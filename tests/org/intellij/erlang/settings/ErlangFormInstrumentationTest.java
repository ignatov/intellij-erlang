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

package org.intellij.erlang.settings;

import junit.framework.TestCase;

import java.lang.reflect.Method;

// The GUI Designer instrumenter adds one $$$getFont$$$ helper to a class bound to a form with a font property (#1402).
public class ErlangFormInstrumentationTest extends TestCase {
  public void testExternalToolsConfigurableHasOneGetFontHelper() throws Exception {
    doTest("org.intellij.erlang.settings.ErlangExternalToolsConfigurable");
  }

  public void testRebarConfigurationFormHasOneGetFontHelper() throws Exception {
    doTest("org.intellij.erlang.rebar.settings.RebarConfigurationForm");
  }

  public void testRebarProjectRootStepHasOneGetFontHelper() throws Exception {
    doTest("org.intellij.erlang.rebar.importWizard.RebarProjectRootStep");
  }

  private static void doTest(String className) throws Exception {
    Class<?> aClass = Class.forName(className, false, ErlangFormInstrumentationTest.class.getClassLoader());
    int count = 0;
    for (Method method : aClass.getDeclaredMethods()) {
      if (method.getName().equals("$$$getFont$$$")) count++;
    }
    assertEquals(className, 1, count);
  }
}
