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

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.roots.libraries.LibraryTablesRegistrar;
import org.intellij.erlang.rebar.runner.RebarRunningStateUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangSdkForSmallIdesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangSdkForSmallIdesTest() {
    super(true);
  }

  public void testNoSdkPathWithoutLibrary() {
    assertTrue(ErlangSystemUtil.isSmallIde());
    assertNull(LibraryTablesRegistrar.getInstance().getLibraryTable(getProject()).getLibraryByName("Erlang SDK"));
    assertNull(ErlangSdkType.getSdkPath(getProject()));
  }

  public void testEscriptFromPathWithoutLibrary() throws Exception {
    assertNull(LibraryTablesRegistrar.getInstance().getLibraryTable(getProject()).getLibraryByName("Erlang SDK"));
    String expected = ApplicationManager.getApplication().executeOnPooledThread(
      () -> RebarRunningStateUtil.findEscriptExecutable()).get();
    String actual = ApplicationManager.getApplication().executeOnPooledThread(
      () -> RebarRunningStateUtil.findEscriptExecutable(getProject())).get();
    assertEquals(expected, actual);
  }
}
