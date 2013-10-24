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

package org.intellij.erlang.lexer;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LexerTestCase;

import java.io.File;

/**
 * @author savenko
 */
public class ErlangFormsLexerTest extends LexerTestCase {
  @Override
  protected Lexer createLexer() {
    return new ErlangFormsLexer();
  }

  @Override
  protected String getDirPath() {
    return "testData/lexer/FormsLexer";
  }

  public void testEmptyFile()        throws Exception { doTest(); }
  public void testModule()           throws Exception { doTest(); }
  public void testTwoForms()         throws Exception { doTest(); }
  public void testTrailingNewLine()  throws Exception { doTest(); }
  public void testRecordExpression() throws Exception { doTest(); }
  public void testDotsInLiterals()   throws Exception { doTest(); }
  public void testIncompleteForm()   throws Exception { doTest(); }
  public void testCommentaries()     throws Exception { doTest(); }

  private void doTest() throws Exception {
    doTest(loadTestFile(), loadExpectedFile());
  }

  private String loadTestFile() throws Exception {
    return loadFile(false);
  }

  private String loadExpectedFile() throws Exception {
    return loadFile(true);
  }

  private String loadFile(boolean isExpectedResultFile) throws Exception {
    String fileName = getTestName(true) + (isExpectedResultFile ? "-expected.txt" : ".erl");
    return FileUtil.loadFile(new File(getDirPath(), fileName), true);
  }
}
