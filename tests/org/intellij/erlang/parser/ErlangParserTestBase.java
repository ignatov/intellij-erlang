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
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.ParsingTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public abstract class ErlangParserTestBase extends ParsingTestCase {
  protected ErlangParserTestBase(String dataPath, String fileExt, ParserDefinition... definitions) {
    super(dataPath, fileExt, definitions);
  }

  @Override
  protected String getTestDataPath() {
    return "testData";
  }

  @Override
  protected boolean skipSpaces() {
    return true;
  }

  protected void doTest(boolean suppressErrors) {
    super.doTest(true);
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

  @Override
  protected boolean isCheckNoPsiEventsOnReparse() {
    return false;
  }

  @Override
  protected void checkResult(@NotNull String targetDataName, @NotNull PsiFile file) throws IOException {
    // The platform's message of the error element for unparsed input starts with a capital letter from 2026.3 on.
    // The dumps have that form, so that they are the same on every platform this branch is tested against.
    String tree = toParseTreeText(file, skipSpaces(), includeRanges())
      .replace("PsiErrorElement:unmatched input", "PsiErrorElement:Unmatched input");
    doCheckResult(myFullDataPath, targetDataName + ".txt", tree.trim());
  }
}
