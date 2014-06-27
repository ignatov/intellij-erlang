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

package org.intellij.erlang;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.Collection;

public class ErlangFindUsagesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/find-usages/";
  }

  public void testFunctionUsagesInSingleFile()    { doTest(4); }
  public void testFunctionUsagesInMultipleFiles() { doTest(5, "functionUsagesInSingleFile.erl");}

  public void testEmptyAtomFunctionInSingleFile()    { doTest(2); }
  public void testEmptyAtomFunctionInMultipleFiles() { doTest(3, "emptyAtomFunctionInSingleFile.erl"); }

  private void doTest(int expectedResult, String ... extraFiles) {
    String[] files = new String[1 + extraFiles.length];
    files[0] = getTestName(true) + ".erl";
    System.arraycopy(extraFiles, 0, files, 1, extraFiles.length);
    myFixture.configureByFiles(files);

    PsiElement element = myFixture.getElementAtCaret();
    Collection<PsiReference> refs = ReferencesSearch.search(element).findAll();
    assertEquals(expectedResult, refs.size());
  }
}
