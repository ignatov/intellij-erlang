/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.quickfixes;

import org.intellij.erlang.inspection.ErlangDefiningImportedFunctionInspection;
import org.intellij.erlang.inspection.ErlangFunctionAlreadyImportedInspection;
import org.intellij.erlang.inspection.ErlangImportDirectiveOverridesAutoimportedBifInspection;

public class ErlangImportFixTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(
      ErlangDefiningImportedFunctionInspection.class,
      ErlangImportDirectiveOverridesAutoimportedBifInspection.class,
      ErlangFunctionAlreadyImportedInspection.class
      );
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/import/";
  }

  private void doTest() {
    loadModule();
    doTest("Remove from import");
  }

  private void loadModule() {
    myFixture.configureByText("incl.erl",
      "-module(incl).\n" +
        "-export([crc32/1, crc32/2, abs/1, dt_get_tag/0, bar/0, abs/0, foo/0]).\n" +
        "foo() -> ok.\n" +
        "crc32(Data) -> Data.\n" +
        "crc32(D, T) -> ok.\n" +
        "abs(D) -> D.\n" +
        "abs() -> zero.\n" +
        "dt_get_tag() -> ok.\n" +
        "bar() -> ok.");
  }

  public void testCommon()              { doTest(); }
  public void testMultipleImportLines() { doTest(); }
  public void testOneImport()           { doTest(); }
  public void testDuplicateImport()     { doTest(); }
  public void testNoImport()            { doTest(); }
  public void testNoImport2()           { doTest(); }
  public void testImportAutoimported()  { doTest(); }
  public void testAlreadyImported1()    { doTest(); }
  public void testAlreadyImported2()    { doTest(); }

}