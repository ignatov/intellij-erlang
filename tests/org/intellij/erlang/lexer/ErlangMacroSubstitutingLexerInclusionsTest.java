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

package org.intellij.erlang.lexer;

import com.intellij.lang.TokenWrapper;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.tree.IElementType;
import org.intellij.erlang.context.ErlangCompileContext;
import org.intellij.erlang.context.ErlangCompileContextManager;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangMacroSubstitutingLexerInclusionsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/lexer/MacroSubstitutingLexerInclusions/";
  }

  public void testMacroUndefinitionFromTransitiveInclusion() throws Exception { doTest(); }
  public void testMacroDefinitionFromTransitiveInclusion()   throws Exception { doTest(); }
  public void testMacroUndefinitionFromDirectInclusion()     throws Exception { doTest(); }
  public void testMacroDefinitionFromDirectInclusion()       throws Exception { doTest(); }
  public void testConditionallyDisabledInclusions()          throws Exception { doTest(); }
  public void testMacroDefinitionsSpanAcrossInclusions()     throws Exception { doTest(); }
  public void testExpansionOccursInPlace()                   throws Exception { doTest(); }

  private void doTest() throws Exception {
    VirtualFile source = LocalFileSystem.getInstance().findFileByPath(getTestDataPath() + getTestName(true) + ".erl");
    assertNotNull(source);

    ErlangCompileContext compileContext = ErlangCompileContextManager.getInstance(getProject()).getContext(getProject(), source);
    ErlangMacroSubstitutingLexer lexer = new ErlangMacroSubstitutingLexer(compileContext, source);
    String sourceText = VfsUtilCore.loadText(source);
    lexer.start(StringUtil.convertLineSeparators(sourceText));
    String actualTokens = printTokens(lexer);

    assertSameLinesWithFile(getTestDataPath() + getTestName(true) + ".txt", actualTokens);
  }

  private static String printTokens(Lexer lexer) {
    StringBuilder sb = new StringBuilder();
    while (lexer.getTokenType() != null) {
      IElementType tt = lexer.getTokenType();
      sb.append(tt.toString()).append(" ('").append(tokenText(lexer)).append("')\n");
      lexer.advance();
    }
    return sb.toString();
  }

  private static String tokenText(Lexer lexer) {
    IElementType tt = lexer.getTokenType();
    CharSequence rawText = tt instanceof TokenWrapper ? ((TokenWrapper) tt).getValue() :
      lexer.getBufferSequence().subSequence(lexer.getTokenStart(), lexer.getTokenEnd());
    return StringUtil.replace(rawText.toString(), "\n", "\\n");
  }
}
