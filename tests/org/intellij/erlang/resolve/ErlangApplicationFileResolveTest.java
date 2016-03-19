/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModuleRef;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangApplicationFileResolveTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/resolve/app/";
  }

  private void doTest(String focusedFilePath, String expectedFilePath) {
    PsiFile[] files = myFixture.configureByFiles(focusedFilePath, expectedFilePath);
    ErlangFile focusedFile = (ErlangFile) files[0];
    assertNotNull(focusedFile);
    ErlangFile expectedFile = (ErlangFile) files[1];
    assertNotNull(expectedFile);

    ErlangModuleRef ref = getElementAtCaret(ErlangModuleRef.class);
    ErlangFile resolvedFile = ErlangPsiImplUtil.resolveToFile(ref);

    assertSame(resolvedFile, expectedFile);
  }

  public void testMod()         { doTest("appMod.app", "module.erl"); }
  public void testModules()     { doTest("appModules.app", "module.erl"); }
  public void testRegistered()  { doTest("appRegistered.app", "module.erl"); }
}
