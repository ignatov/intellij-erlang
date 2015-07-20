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

package org.intellij.erlang.resolve;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangFunctionResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/resolve/function/" + getTestName(true) + "/";
  }

  protected void doTest(String focusedFilePath, String expectedFilePath, String... otherFilePaths) {
    String[] paths = {focusedFilePath, expectedFilePath};
    PsiFile[] files = myFixture.configureByFiles(ArrayUtil.mergeArrays(paths, otherFilePaths));
    ErlangFile focusedFile = (ErlangFile) files[0];
    assertNotNull(focusedFile);
    ErlangFile expectedFile = (ErlangFile) files[1];
    assertNotNull(expectedFile);

    int offset = myFixture.getEditor().getCaretModel().getOffset();
    ErlangFunctionCallExpression functionCall = PsiTreeUtil.getParentOfType(focusedFile.findElementAt(offset),
                                                                            ErlangFunctionCallExpression.class);
    assertNotNull(functionCall);
    PsiReference reference = functionCall.getReference();
    assertNotNull(reference);
    PsiElement resolvedFunction = reference.resolve();
    assertNotNull(resolvedFunction);

    String name = functionCall.getName();
    int arity = functionCall.getArgumentList().getExpressionList().size();
    ErlangFunction expectedFunction = expectedFile.getFunction(name, arity);
    assertSame(expectedFunction, resolvedFunction);
  }

  public void testNoAutoImport()               { doTest("test.erl", "incl.erl"); }
  public void testNoAutoImportWithTuple()      { doTest("test.erl", "incl.erl"); }
  public void testPreferImportFromUserModule() { doTest("test.erl", "incl.erl"); }
  public void testPreferLocalFunction()        { doTest("test.erl", "test.erl", "incl.erl"); }
  public void testPreferFirstImport()          { doTest("test.erl", "incl.erl", "another_incl.erl"); }
}
