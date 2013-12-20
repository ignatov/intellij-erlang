/*
 * Copyright 2012-2013 Sergey Ignatov
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

import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public abstract class ErlangIncludeResolveTestBase extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public ErlangIncludeResolveTestBase(String platformPrefix) {
    super(platformPrefix);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/resolve/include/" + getTestName(true) + "/";
  }

  protected void doTest(String... files) throws Exception {
    doTest(1, files);
  }

  protected void doTestWithExpectedResolveFailure(String... files) throws Exception {
    doTest(0, files);
  }

  private void doTest(int expectedIncludedFiles, String... files) throws Exception {
    myFixture.configureByFiles(files);
    ErlangFile file = (ErlangFile) myFixture.getFile();
    List<ErlangFile> directlyIncludedFiles = ErlangPsiImplUtil.getDirectlyIncludedFiles(file);
    assertEquals("Include attribute resolved to unexpected number of files.", expectedIncludedFiles, directlyIncludedFiles.size());
  }
}
