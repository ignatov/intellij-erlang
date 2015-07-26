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

package org.intellij.erlang.parser;

import com.intellij.core.CoreApplicationEnvironment;
import com.intellij.lang.LanguageExtensionPoint;
import com.intellij.lang.ParserDefinition;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.ParsingTestCase;

import java.io.File;
import java.io.IOException;

public abstract class ErlangParserTestBase extends ParsingTestCase {
  private final boolean myOverrideTestData;

  public ErlangParserTestBase(String dataPath, String fileExt, boolean overrideTestData, ParserDefinition... definitions) {
    super(dataPath, fileExt, definitions);
    myOverrideTestData = overrideTestData;
  }

  @Override
  protected String getTestDataPath() {
    return "testData";
  }

  @Override
  protected boolean skipSpaces() {
    return true;
  }

  protected void doTest(boolean checkResult, boolean suppressErrors) {
    if (myOverrideTestData) {
      doOverrideTestData();
    }
    doTest(checkResult);
    if (!suppressErrors) {
      assertFalse(
        "PsiFile contains error elements",
        toParseTreeText(myFile, skipSpaces(), includeRanges()).contains("PsiErrorElement")
      );
    }
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    CoreApplicationEnvironment.registerExtensionPoint(Extensions.getRootArea(), "com.intellij.lang.braceMatcher", LanguageExtensionPoint.class);
  }

  private void doOverrideTestData() {
    try {
      String testName = getTestName(false);
      String text = loadFile(testName + "." + myFileExt);
      myFile = createPsiFile(testName, text);
      ensureParsed(myFile);
      FileUtil.writeToFile(new File(myFullDataPath + "/" + testName + ".txt"), toParseTreeText(myFile, skipSpaces(), includeRanges()));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
