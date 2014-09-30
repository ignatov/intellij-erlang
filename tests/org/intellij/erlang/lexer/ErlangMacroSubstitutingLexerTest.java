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
import org.intellij.erlang.context.ErlangCompileContext;

/**
 * @author savenko
 */
public class ErlangMacroSubstitutingLexerTest extends ErlangLexerTestBase {
  @Override
  protected Lexer createLexer() {
    return new ErlangMacroSubstitutingLexer(new ErlangCompileContext("test"));
  }

  @Override
  protected String getDirPath() {
    return "testData/lexer/MacroSubstitutingLexer";
  }

  public void testSingleTokenMacro()              throws Exception { doTest(); }
  public void testClauseBodyMacro()               throws Exception { doTest(); }
  public void testBindBitSyntaxMacro()            throws Exception { doTest(); }
  public void testParameterlessMacroOverloading() throws Exception { doTest(); }
  public void testFunctionArguments()             throws Exception { doTest(); }
  public void testMacroInsideMacroBody()          throws Exception { doTest(); }
  public void testQuestionMarkInMacro()           throws Exception { doTest(); }
  public void testMacroWithRightParentheses()     throws Exception { doTest(); }
  public void testEmptyMacroBody()                throws Exception { doTest(); }
  public void testUnresolvedMacroCall()           throws Exception { doTest(); }
  public void testIncompleteMacroCall()           throws Exception { doTest(); }
  public void testParametersSubstitution()        throws Exception { doTest(); }
  public void testParameterTextSubstitution()     throws Exception { doTest(); }
  public void testBindBitSyntaxMacroArgument()    throws Exception { doTest(); }
  public void testWhitespaceMacroArgument()       throws Exception { doTest(); }
  public void testCommasInMacroCallArguments()    throws Exception { doTest(); }
  public void testUnresolvedMacroInsideMacroBody()throws Exception { doTest(); }
  public void testUndefinitionPreventsExpansion() throws Exception { doTest(); }
}
