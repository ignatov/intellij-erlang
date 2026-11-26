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

import com.intellij.codeInsight.daemon.impl.analysis.FileHighlightingSetting;
import com.intellij.codeInsight.daemon.impl.analysis.HighlightingSettingsPerFile;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.ide.PowerSaveMode;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.markup.HighlighterLayer;
import com.intellij.openapi.editor.markup.HighlighterTargetArea;
import com.intellij.openapi.editor.markup.RangeHighlighter;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.psi.PsiAnchor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangAmbiguousCallOfAutoImportedFunctionInspection;
import org.intellij.erlang.inspection.ErlangDuplicateFunctionExportInspection;
import org.intellij.erlang.inspection.ErlangUnusedFunctionInspection;
import org.intellij.erlang.intention.ErlangExportFunctionIntention;
import org.intellij.erlang.psi.ErlangExport;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangElementFactory;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
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

  public void testExportOfferedOnceWhenInspectionsSkipped()  { assertExportOfferedOnceWithHighlighting(FileHighlightingSetting.SKIP_INSPECTION); }
  public void testExportOfferedOnceWhenHighlightingSkipped() { assertExportOfferedOnceWithHighlighting(FileHighlightingSetting.SKIP_HIGHLIGHTING); }
  public void testExportOfferedOnceWhenEssentialOnly()       { assertExportOfferedOnceWithHighlighting(FileHighlightingSetting.ESSENTIAL); }

  // The light fixture runs the inspections even in Power Save Mode, so only the availability of the intention is checked
  public void testExportIntentionAvailableInPowerSaveMode() {
    myFixture.configureByFile("exportOfferedOnce.erl");
    PowerSaveMode.setEnabled(true);
    try {
      assertTrue(new ErlangExportFunctionIntention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
    }
    finally {
      PowerSaveMode.setEnabled(false);
    }
  }

  private void assertExportOfferedOnceWithHighlighting(FileHighlightingSetting setting) {
    myFixture.configureByFile("exportOfferedOnce.erl");
    HighlightingSettingsPerFile settings = HighlightingSettingsPerFile.getInstance(getProject());
    settings.setHighlightingSettingForRoot(myFixture.getFile(), setting);
    try {
      assertExportOfferedOnce(null);
    }
    finally {
      settings.setHighlightingSettingForRoot(myFixture.getFile(), FileHighlightingSetting.FORCE_HIGHLIGHTING);
    }
  }

  private void assertExportOfferedOnce(String fileName) {
    if (fileName != null) myFixture.configureByFile(fileName);
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
  public void testDuplicateExportWithDigitSeparators() { doTest("Remove duplicate export"); }
  public void testDuplicateExportKeepsOtherLists() { doTest("Remove duplicate export"); }
  public void testDuplicateExportKeepsComments() { doTest("Remove duplicate export"); }
  public void testDuplicateExportCommaFirst() { doTest("Remove duplicate export"); }
  public void testDuplicateExportCommentBeforeComma() { doTest("Remove duplicate export"); }
  public void testDuplicateExportPreviewOfRemovedEntry() throws IOException { doTestPreview("duplicateExportKeepsOtherLists"); }
  public void testDuplicateExportPreviewOfRemovedAttribute() throws IOException { doTestPreview("oneDuplicateExport1"); }

  private void doTestPreview(String fixtureName) throws IOException {
    myFixture.configureByFile(fixtureName + ".erl");
    IntentionAction action = ContainerUtil.getFirstItem(myFixture.filterAvailableIntentions("Remove duplicate export"));
    assertNotNull(action);
    String expected = FileUtil.loadFile(new File(getTestDataPath(), fixtureName + "-after.erl")).replace("<caret>", "");
    assertEquals(expected, myFixture.getIntentionPreviewText(action));
  }

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

  public void testInvokeWithoutEditorUpdatesFirstExport() {
    myFixture.configureByFile("invokeWithoutEditor.erl");
    invokeExportFix(getFunctionAtCaret(), null);
    myFixture.checkResultByFile("invokeWithoutEditor-after.erl");
  }

  public void testInvokeOnFunctionThatCannotBeExportedChangesNothing() {
    myFixture.configureByFile("macroDefinedAfterFunction.erl");
    String before = myFixture.getFile().getText();
    invokeExportFix(getFunctionAtCaret(), myFixture.getEditor());
    assertEquals(before, myFixture.getFile().getText());
  }

  public void testInvokeOnNonFunctionChangesNothing() {
    myFixture.configureByFile("common.erl");
    String before = myFixture.getFile().getText();
    ErlangFunction function = getFunctionAtCaret();
    PsiElement module = ((ErlangFile) myFixture.getFile()).getModule();
    assertNotNull(module);
    WriteCommandAction.runWriteCommandAction(getProject(), () ->
      new ErlangExportFunctionFix(function).invoke(getProject(), myFixture.getFile(), myFixture.getEditor(), module, null));
    assertEquals(before, myFixture.getFile().getText());
  }

  public void testFixIsAvailable() {
    myFixture.configureByFile("common.erl");
    ErlangFunction function = getFunctionAtCaret();
    ErlangExportFunctionFix fix = new ErlangExportFunctionFix(function);
    PsiFile file = myFixture.getFile();
    assertTrue(fix.isAvailable(getProject(), file, function, function));
    PsiElement module = ((ErlangFile) file).getModule();
    assertNotNull(module);
    assertTrue("not a function: the fix does not check", fix.isAvailable(getProject(), file, module, module));
    PsiFile textFile = myFixture.configureByText("a.txt", "text");
    assertTrue("not an Erlang file: the fix does not check", fix.isAvailable(getProject(), textFile, function, function));
  }

  public void testFixIsNotAvailableWhenFunctionCannotBeExported() {
    myFixture.configureByFile("macroDefinedAfterFunction.erl");
    ErlangFunction function = getFunctionAtCaret();
    assertFalse(new ErlangExportFunctionFix(function).isAvailable(getProject(), myFixture.getFile(), function, function));
  }

  public void testFixPreviewIsEmpty() {
    myFixture.configureByFile("common.erl");
    ErlangExportFunctionFix fix = new ErlangExportFunctionFix(getFunctionAtCaret());
    assertEquals("Export function", fix.getText());
    assertSame(IntentionPreviewInfo.EMPTY, fix.generatePreview(getProject(), myFixture.getEditor(), myFixture.getFile()));
  }

  public void testPopupRendererText() throws Exception {
    String longList = "aaaaaaaaaa/0, bbbbbbbbbb/0, cccccccccc/0, dddddddddd/0, eeeeeeeeee/0, ffffffffff/0, gggggggggg/0";
    myFixture.configureByText("popup.erl", "-module(popup).\n" +
                                           "-export([foo/0,   bar/0]).\n" +
                                           "-export([" + longList + "]).\n" +
                                           "foo() -> ok.\n");
    List<ErlangExport> exports = ErlangExportFunctionFix.getExportPsiElements((ErlangFile) myFixture.getFile());
    assertEquals(2, exports.size());

    ListCellRenderer<Object> renderer = getPopupRenderer();
    assertEquals("[foo/0, bar/0]", getRenderedText(renderer, exports.get(0)));
    String expectedLong = ("[" + longList + "]").substring(0, 78) + "...";
    assertEquals(expectedLong, getRenderedText(renderer, exports.get(1)));
    assertEquals("Add a new -export() attribute",
                 getRenderedText(renderer, ErlangElementFactory.createExportFromText(getProject(), "")));
  }

  public void testPopupRendererTextOfExportOfLengthLimit() throws Exception {
    String list = "[" + "a".repeat(76) + "/0]";
    assertEquals(80, list.length());
    myFixture.configureByText("popup.erl", "-module(popup).\n-export(" + list + ").\n");
    List<ErlangExport> exports = ErlangExportFunctionFix.getExportPsiElements((ErlangFile) myFixture.getFile());
    assertEquals(list, getRenderedText(getPopupRenderer(), exports.getFirst()));
  }

  public void testPopupHighlighterRemoved() throws Exception {
    myFixture.configureByFile("common.erl");
    Editor editor = myFixture.getEditor();
    Method removeHighlighter = ErlangExportFunctionFix.class.getDeclaredMethod("removeHighlighter", Editor.class, Ref.class);
    removeHighlighter.setAccessible(true);

    Ref<RangeHighlighter> empty = new Ref<>();
    removeHighlighter.invoke(null, editor, empty);
    assertNull(empty.get());

    int before = editor.getMarkupModel().getAllHighlighters().length;
    RangeHighlighter highlighter = editor.getMarkupModel().addRangeHighlighter(
      0, 1, HighlighterLayer.SELECTION, null, HighlighterTargetArea.EXACT_RANGE);
    Ref<RangeHighlighter> current = new Ref<>(highlighter);
    removeHighlighter.invoke(null, editor, current);
    assertNull(current.get());
    assertFalse(highlighter.isValid());
    assertEquals(before, editor.getMarkupModel().getAllHighlighters().length);
  }

  private ErlangFunction getFunctionAtCaret() {
    PsiElement element = myFixture.getFile().findElementAt(myFixture.getCaretOffset());
    ErlangFunction function = PsiTreeUtil.getParentOfType(element, ErlangFunction.class);
    assertNotNull(function);
    return function;
  }

  private void invokeExportFix(ErlangFunction function, Editor editor) {
    WriteCommandAction.runWriteCommandAction(getProject(), () ->
      new ErlangExportFunctionFix(function).invoke(getProject(), myFixture.getFile(), editor, function, null));
  }

  @SuppressWarnings("unchecked")
  private static ListCellRenderer<Object> getPopupRenderer() throws Exception {
    Method getRenderer = ErlangExportFunctionFix.class.getDeclaredMethod("getRenderer");
    getRenderer.setAccessible(true);
    return (ListCellRenderer<Object>) getRenderer.invoke(null);
  }

  private static String getRenderedText(ListCellRenderer<Object> renderer, PsiElement element) {
    var component = renderer.getListCellRendererComponent(new JList<>(), PsiAnchor.create(element), 0, false, false);
    return ((JLabel) component).getText();
  }
}
