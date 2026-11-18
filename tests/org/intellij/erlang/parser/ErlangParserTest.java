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

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangStringLiteral;

import java.io.IOException;
import java.util.Collection;

public class ErlangParserTest extends ErlangParserTestBase {
  public ErlangParserTest() {
    super("parser", "erl", new ErlangParserDefinition());
  }
                  
  public void testHelloWorld()                          { doTest(false); }
  public void testExport()                              { doTest(false); }
  public void testH()                                   { doTest(false); }
  public void testMnesia()                              { doTest(false); }
  public void testIsDigits()                            { doTest(false); }
  public void testDialyzerDataflow()                    { doTest(false); }
  public void testTest()                                { doTest(false); }
  public void testRecords()                             { doTest(false); }
  public void testDialyzerClParse()                     { doTest(false); }
  public void testMp4Mux()                              { doTest(false); }
  public void testComments()                            { doTest(false); }
  public void testMultiline()                           { doTest(false); }
  public void testRecordExpression()                    { doTest(false); }
  public void testPeriods()                             { doTest(true);  }
  public void test19()                                  { doTest(true);  }
  public void test25()                                  { doTest(false); }
  public void test26()                                  { doTest(false); }
  public void testFailedRecord()                        { doTest(true);  }
  public void testRecords2()                            { doTest(false); }
  public void testAtomWithArity()                       { doTest(false); }
  public void testNoAtomsWithArity()                    { doTest(false); }
  public void test77()                                  { doTest(false); }
  public void test74()                                  { doTest(false); }
  public void testErrors()                              { doTest(true);  }
  public void test51()                                  { doTest(false); } // performance
  public void test96()                                  { doTest(false); }
  public void test30()                                  { doTest(true);  }
  public void test127()                                 { doTest(true);  }
  public void testMeck()                                { doTest(false); }
  public void test145()                                 { doTest(false); }
  public void test145_2()                               { doTest(false); }
  public void test29()                                  { doTest(false); }
  public void testEmptyRecord()                         { doTest(false); }
  public void testTypeAgain()                           { doTest(false); }
  public void testTest2()                               { doTest(true);  }
  public void testDebug()                               { doTest(true);  }
  public void testDebug2()                              { doTest(true);  }
  public void test175()                                 { doTest(true);  }
  public void test182()                                 { doTest(true);  }
  public void test183()                                 { doTest(true);  }
  public void test210()                                 { doTest(false); }
  public void test213()                                 { doTest(false); }
  public void test223()                                 { doTest(false); }
  public void testQuotation()                           { doTest(true);  }
  public void testRiakHacks()                           { doTest(true);  }
  public void testRecover()                             { doTest(true);  }
  public void testRecover2()                            { doTest(true);  }
  public void testBeginEnd()                            { doTest(true);  }
  public void testBeginEnd2()                           { doTest(true);  }
  public void test296()                                 { doTest(true);  }
  public void test304()                                 { doTest(true);  }
  public void test305()                                 { doTest(true);  }
  public void test306()                                 { doTest(true);  }
  public void testEscriptShebang1()                     { doTest(false); }
  public void testEscriptShebang2()                     { doTest(true);  }
  public void test325()                                 { doTest(true);  }
  public void test390()                                 { doTest(true);  }
  public void testMapConstruct()                        { doTest(false); }
  public void testMapSingleValueAcc()                   { doTest(false); }
  public void testMapUpdate()                           { doTest(false); }
  public void testMapMatch()                            { doTest(false); }
  public void testMapTypes()                            { doTest(false); }
  public void testComprehensions()                      { doTest(false); }
  public void testMacrosHacks()                         { doTest(false); }
  public void testMacrosUnderscore()                    { doTest(false); }
  public void testOptionalCallbacks()                   { doTest(false); }
  public void testComprehensionsRecovery()              { doTest(true);  }
  public void testSendExpressionPriority()              { doTest(false); }
  public void testLongListExpression632()               { doTest(false); }
  public void testQuotedAtomFollowedByQuote()           { doTest(true);  }
  public void testOptionalCallbacksRecovery()           { doTest(true);  }
  public void testWhitespaceEscapeInCharLiterals()      { doTest(false); }
  public void testMacroCallsInCompoundStringLiterals()  { doTest(false); }
  public void testTypo()                                { doTest(true);  }
  public void testNewNumbers()                          { doTest(true);  }
  public void testTripleQuotes()                        { doTest(true);  }
  public void testSigils()                              { doTest(true);  }
  public void testTripleQuotesMoreThanThree()           { doTest(false); }
  public void testOtp28Generators()                     { doTest(false); }
  public void testBasedFloats()                         { doTest(false); }
  public void testNominalTypes()                        { doTest(false); }
  public void testOtp29Comprehensions()                 { doTest(false); }
  public void testNativeRecords()                       { doTest(false); }
  public void testNativeRecordAttributesRecovery()      { doTest(true);  }
  public void testIntTypeExpressions()                  { doTest(false); }
  public void testIntTypeExpressionsErrors()            { doTest(true);  }
  public void testBinaryTypes()                         { doTest(false); }
  public void testBinaryTypesErrors()                   { doTest(true);  }
  public void testBinaryTypeSizeExpressions()           { doTest(false); }
  public void testQuotedAttributeNames()                { doTest(false); }

  public void testSixQuoteMultilineStringAndSigil() {
    PsiFile file = parseText("-module(six_quotes).\n" +
                             "plain() -> \"\"\"\"\"\"\n  hello\n  \"\"\"\"\"\".\n" +
                             "sigil() -> ~\"\"\"\"\"\"\n  world\n  \"\"\"\"\"\".\n" +
                             "plain_with_space() -> \"\"\"\"\"\" \n  hello\n  \"\"\"\"\"\".\n" +
                             "sigil_with_space() -> ~\"\"\"\"\"\" \n  world\n  \"\"\"\"\"\".\n" +
                             "sigil_with_prefix_and_tab() -> ~s\"\"\"\"\"\"\t\n  text\n  \"\"\"\"\"\".\n");
    assertFalse(PsiTreeUtil.hasErrorElements(file));
    assertEquals(5, PsiTreeUtil.findChildrenOfType(file, ErlangStringLiteral.class).size());
  }

  public void testTripleQuotedDocStringDoesNotExposeFunctionAsStub() {
    PsiFile file = parseText("-module(triple_quoted_doc_string_stub).\n" +
                             "-moduledoc \"\"\"\"\n" +
                             "  -doc \"\"\".\n" +
                             "  example() -> ok.\n" +
                             "  \"\"\"\".\n" +
                             "-export([visible/0]).\n" +
                             "visible() -> ok.\n");
    assertFalse(PsiTreeUtil.hasErrorElements(file));
    Collection<ErlangFunction> functions = PsiTreeUtil.findChildrenOfType(file, ErlangFunction.class);
    assertEquals(1, functions.size());
    assertEquals("visible", functions.iterator().next().getName());
  }

  private PsiFile parseText(String text) {
    return createPsiFile("test.erl", text);
  }

  public void testUnterminatedLongQuoteRunScalesLinearly() throws IOException {
    tokenizeUnterminatedQuoteRun(2_000);
    long smallRunNanos = tokenizeUnterminatedQuoteRun(10_000);
    long largeRunNanos = tokenizeUnterminatedQuoteRun(40_000);
    assertTrue("Long quote run took " + largeRunNanos + " ns after " + smallRunNanos + " ns for one quarter the input",
               largeRunNanos < smallRunNanos * 8);
  }

  private static long tokenizeUnterminatedQuoteRun(int quoteCount) throws IOException {
    String input = "\"".repeat(quoteCount);
    _ErlangLexer lexer = new _ErlangLexer((Project) null);
    lexer.reset(input, 0, input.length(), _ErlangLexer.YYINITIAL);
    long start = System.nanoTime();
    int tokens = 0;
    while (lexer.advance() != null) tokens++;
    assertTrue("Expected tokens for unterminated quote run", tokens > 0);
    return System.nanoTime() - start;
  }
}
