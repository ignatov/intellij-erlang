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

import com.intellij.psi.PsiFile;
import org.intellij.erlang.inspection.ErlangAmbiguousCallOfAutoImportedFunctionInspection;
import org.intellij.erlang.inspection.ErlangDuplicateFunctionExportInspection;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.psi.ErlangExport;
import org.intellij.erlang.psi.ErlangFile;

import java.util.List;

public class ErlangFunctionFixesTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(
      ErlangUnusedFunctionInspection.class,
      ErlangDuplicateFunctionExportInspection.class,
      ErlangAmbiguousCallOfAutoImportedFunctionInspection.class
    );
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/export/";
  }

  public void testEmpty()      { doTest("Export function"); }
  public void testWithout()    { doTest("Export function"); }
  public void testCommon()     { doTest("Export function"); }
  public void testMacroWithExport()      { doTest("Export function"); }
  public void testMacroWithout()         { doTest("Export function"); }
  public void testMacroDefinedBelowExport() { doTest("Export function"); }
  public void testMacroDefinedTwice()      { doTest("Export function"); }
  public void testMacroSeveralExports()    { doTest("Export function"); }
  public void testMacroNotDefined()        { doTest("Export function"); }
  public void testQuotedAtomWithExport() { doTest("Export function"); }
  public void testQuotedAtomWithout()    { doTest("Export function"); }

  public void testDelete()     { doTest("Remove function"); }
  public void testDeleteSpec() { doTest("Remove function"); }
  public void testDeleteSpec2() { doTest("Remove function"); }
  public void testDeleteSpec3() { doTest("Remove function"); }

  public void testOneDuplicateExport1() { doTest("Remove duplicate export"); }
  public void testOneDuplicateExport2() { doTest("Remove duplicate export"); }
  public void testFewDuplicateExport()  { doTest("Remove duplicate export"); }

  public void testFewEmpties()          { doTest("Export function"); }
  public void testFewNonEmpties1()      { doTest("Export function"); }
  public void testFewNonEmpties2()      { doTest("Export function"); }

  public void testExportsToShowInPopupAllEmpty() {
    myFixture.configureByFile("without.erl");
    assertEquals(0, getExportsToShow(myFixture.getFile()).size());
    myFixture.configureByFile("fewEmpties.erl");
    assertEquals(0, getExportsToShow(myFixture.getFile()).size());
    myFixture.configureByFile("fewNonEmpties1.erl");
    assertEquals(2, getExportsToShow(myFixture.getFile()).size());
    myFixture.configureByFile("fewNonEmpties2.erl");
    assertEquals(2, getExportsToShow(myFixture.getFile()).size());
  }

  public void testExportsToShowInPopupForMacroFunction() {
    myFixture.configureByFile("macroSeveralExports.erl");
    var file = (ErlangFile) myFixture.getFile();
    var function = file.getFunctions().getLast();
    var definition = ErlangExportFunctionFix.getLastMacroDefinition(file, function);
    assertNotNull(definition);
    var exports = ErlangExportFunctionFix.getExportsBelow(ErlangExportFunctionFix.getExportPsiElements(file), definition);
    assertEquals(1, exports.size());
    assertEquals("[another/0]", exports.getFirst().getExportFunctions().getText());
  }

  private static List<ErlangExport> getExportsToShow(PsiFile file) {
    return ErlangExportFunctionFix.getNotEmptyExports(ErlangExportFunctionFix.getExportPsiElements((ErlangFile) file));
  }
}
