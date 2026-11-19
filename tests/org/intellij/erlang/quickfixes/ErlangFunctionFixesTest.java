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
  public void testMacroRedefinedBelowFunction()   { doTest("Export function"); }

  // Known limitation until the plugin evaluates conditional compilation: when the function or a -define of its macro
  // is inside -ifdef/-ifndef/-if, the export goes where it goes for any other function. The expected files below are not
  // correct in every configuration: with -DTEST erlc fails with "undefined macro 'name'" (the -export is above the
  // -define), and without it they compile but export nothing (the -export is inside the -ifdef(TEST) branch).
  public void testMacroAndFunctionInSeparateIfdefs() { doTest("Export function"); }
  public void testMacroAndFunctionInSameIfdef()      { doTest("Export function"); }
  public void testMacroDefinedInIfdefAndElse()       { doTest("Export function"); }

  public void testMacroDefinedAfterFunction() {
    myFixture.configureByFile("macroDefinedAfterFunction.erl");
    assertNoIntentionsAvailable("Export function", "no -export can be placed below the -define and above the functions");
  }

  public void testExportOfferedOnce() {
    myFixture.configureByFile("exportOfferedOnce.erl");
    var actions = myFixture.filterAvailableIntentions("Export function");
    assertEquals("one \"Export function\" action on an unused function: " + actions, 1, actions.size());
  }

  public void testExportOfferedOnceWhenInspectionDisabled() {
    myFixture.disableInspections(new ErlangUnusedFunctionInspection());
    assertExportOfferedOnce("exportOfferedOnce.erl");
  }

  public void testExportOfferedOnceWhenSuppressed()   { assertExportOfferedOnce("exportOfferedOnceWhenSuppressed.erl"); }
  public void testExportOfferedOnceOnUsedFunction()   { assertExportOfferedOnce("exportOfferedOnceOnUsedFunction.erl"); }

  private void assertExportOfferedOnce(String fileName) {
    myFixture.configureByFile(fileName);
    var actions = myFixture.filterAvailableIntentions("Export function");
    assertEquals(String.valueOf(actions), 1, actions.size());
    myFixture.launchAction(actions.getFirst());
    assertEquals(myFixture.getFile().getText(), 2, ((ErlangFile) myFixture.getFile()).getExportedFunctions().size());
  }

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
    var anchor = ErlangExportFunctionFix.getMacroExportAnchor(file, function);
    assertNotNull(anchor);
    var exports = ErlangExportFunctionFix.getExportsBelow(file, ErlangExportFunctionFix.getExportPsiElements(file), anchor);
    assertEquals(1, exports.size());
    assertEquals("[another/0]", exports.getFirst().getExportFunctions().getText());
  }

  private static List<ErlangExport> getExportsToShow(PsiFile file) {
    return ErlangExportFunctionFix.getNotEmptyExports(ErlangExportFunctionFix.getExportPsiElements((ErlangFile) file));
  }
}
