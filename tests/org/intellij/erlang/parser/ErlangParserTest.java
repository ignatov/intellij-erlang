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
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.ErlangStringLiteral;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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
  public void testMapSingleValueAcc()                   { doTest(true);  }
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
  public void testBasedIntegerSeparators()              { doTest(false); }
  public void testBasedIntegerSeparatorsErrors()        { doTest(true);  }
  public void testControlEscapes()                      { doTest(false); }
  public void testControlEscapesErrors()                { doTest(true);  }
  public void testGenericEscapes()                      { doTest(false); }
  public void testHexEscapes()                          { doTest(false); }
  public void testHexEscapesErrors()                    { doTest(true);  }
  public void testControlEscapeAtStringEnd()            { doTest(false); }
  public void testBasedIntegerBaseSeparators()          { doTest(false); }
  public void testBasedIntegerBaseSeparatorsErrors()    { doTest(true);  }
  public void testVerbatimSigilControlEscape()          { doTest(false); }
  public void testHexEscapeInBraceSigil()               { doTest(false); }
  public void testHexEscapeInBraceSigilErrors()         { doTest(true);  }
  public void testTripleQuotes()                        { doTest(true);  }
  public void testSigils()                              { doTest(true);  }
  public void testTripleQuotesMoreThanThree()           { doTest(false); }
  public void testOtp28Generators()                     { doTest(false); }
  public void testBasedFloats()                         { doTest(false); }
  public void testNominalTypes()                        { doTest(false); }
  public void testOtp29Comprehensions()                 { doTest(false); }
  public void testNativeRecords()                       { doTest(false); }
  public void testNativeRecordAttributesRecovery()      { doTest(true);  }
  public void testOtp29RecordNames()                    { doTest(false); }
  public void testOtp29RecordNamesErrors()              { doTest(true);  }
  public void testIntTypeExpressions()                  { doTest(false); }
  public void testIntTypeExpressionsErrors()            { doTest(true);  }
  public void testBinaryTypes()                         { doTest(false); }
  public void testBinaryTypesErrors()                   { doTest(true);  }
  public void testBinaryTypeSizeExpressions()           { doTest(false); }
  public void testBinaryComprehensionTemplates()        { doTest(false); }

  public void testDeeplyNestedBinariesParseWithoutBacktracking() {
    int depth = 20;
    String parenthesized = "X";
    String caseTemplates = "<<>>";
    for (int i = 0; i < depth; i++) {
      parenthesized = "<<(" + parenthesized + ")/binary>>";
      caseTemplates = "<< case X of _ -> " + caseTemplates + " end >>";
    }
    String text = "f(X) -> " + parenthesized + ".\ng(X) -> " + caseTemplates + ".\n";
    long start = System.nanoTime();
    PsiFile file = createPsiFile("nested", text);
    boolean hasErrors = PsiTreeUtil.hasErrorElements(file);
    long millis = (System.nanoTime() - start) / 1_000_000;
    assertFalse("PsiFile contains error elements", hasErrors);
    assertTrue("Parsing took " + millis + " ms", millis < 5000);
  }

  // The parsing tests give the files a mock file type; the Erlang file type comes from the extension then.
  public void testFileTypeFromExtensionInTests() {
    PsiFile unknown = createFile("x.foo", "-module(x).\n");
    assertFalse(unknown.getViewProvider().getFileType() instanceof ErlangFileType);
    assertSame(ErlangFileType.MODULE, unknown.getFileType());
    assertSame(ErlangFileType.HEADER, createFile("y.HRL", "-define(A, 1).\n").getFileType());
  }

  public void testTypedMacroBody()                      { doTest(false); }
  public void testTypedMacroBodyIncomplete()            { doTest(false); }
  public void testIfElifConditionalCompilation()        { doTest(false); }
  public void testQuotedAttributeNames()                { doTest(false); }
  public void testModuleMacroQualifiedType()            { doTest(false); }

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

  // #1643: erlc rejects a map entry without '=>' or ':=' ("syntax error before: '}'").
  public void testMapEntryWithoutOperatorIsError() {
    String[] bodies = {"#{a = 1}", "X#{a = 1}", "case X of #{a = 1} -> ok end"};
    List<String> accepted = new ArrayList<>();
    for (String body : bodies) {
      PsiFile file = parseText("-module(t).\nf(X) -> " + body + ".\n");
      if (!PsiTreeUtil.hasErrorElements(file)) accepted.add(body);
    }
    assertEmpty("Parsed without a syntax error", accepted);
  }

  // #1643: a macro alone can expand to a whole entry, which erlc accepts.
  public void testMapEntryMacroWithoutOperator() {
    String[] bodies = {"#{?E}", "X#{?E, b => 2}", "#{?F(1)}", "case X of #{?M} -> ok end"};
    List<String> rejected = new ArrayList<>();
    for (String body : bodies) {
      PsiFile file = parseText("-module(t).\n-define(E, a => 1).\n-define(F(V), a => V).\n-define(M, a := 1).\nf(X) -> " + body + ".\n");
      if (PsiTreeUtil.hasErrorElements(file)) rejected.add(body);
    }
    assertEmpty("Parsed with a syntax error", rejected);
  }

  // #1646: erlc gives "t.erl:3:18: syntax error before: '.'"
  public void testLoneDotAfterAttributeIsError() {
    PsiFile file = parseText("-module(t).\n" +
                             "-export([f/0]).\n" +
                             "-record(r, {a}). .\n" +
                             "f() -> #r{}.\n");
    assertTrue("PsiFile contains no error elements", PsiTreeUtil.hasErrorElements(file));
  }

  // #1646: erlc gives "t2.erl:1:1: syntax error before: '.'"
  public void testLoneDotBeforeFirstFormIsError() {
    PsiFile file = parseText(".\n" +
                             "-module(t2).\n");
    assertTrue("PsiFile contains no error elements", PsiTreeUtil.hasErrorElements(file));
  }

  // #1646: erlc gives "t4.erl:3:12: syntax error before: '.'"; the dot after a function is marked only at the end of the file
  public void testLoneDotAfterFunctionBeforeNextFormIsError() {
    PsiFile file = parseText("-module(t4).\n" +
                             "-export([f/0, g/0]).\n" +
                             "f() -> ok. .\n" +
                             "g() -> ok.\n");
    assertTrue("PsiFile contains no error elements", PsiTreeUtil.hasErrorElements(file));
  }

  // #1646: one error per lone dot, and the forms after it still parse
  public void testLoneDotKeepsFollowingForms() {
    PsiFile file = parseText("-module(t).\n" +
                             "-record(r, {a}). . % comment\n" +
                             "f() -> #r{}.\n" +
                             ".\n" +
                             "g() -> ok.\n");
    assertSize(2, PsiTreeUtil.findChildrenOfType(file, PsiErrorElement.class));
    assertSize(2, PsiTreeUtil.findChildrenOfType(file, ErlangFunction.class));
    assertSize(1, PsiTreeUtil.findChildrenOfType(file, ErlangRecordDefinition.class));
  }

  // #1667: erlc accepts only `Var :: Type` and `Name(Type, ...)` as a spec constraint after `when`.
  public void testSpecConstraintNotVarTypeOrSubtypeIsError() {
    String[] constraints = {"(other_mod)", "x", "V", "1", "foo()", "(V :: atom())", "V :: atom(), (x)"};
    List<String> accepted = new ArrayList<>();
    for (String constraint : constraints) {
      PsiFile file = parseText("-module(t).\n-export([f/1]).\n-spec f(term()) -> V when " + constraint + ".\nf(X) -> X.\n");
      if (!PsiTreeUtil.hasErrorElements(file)) accepted.add(constraint);
    }
    assertEmpty("Parsed without a syntax error", accepted);
  }

  // #1667: the constraint forms that erlc accepts.
  public void testSpecConstraintVarTypeAndSubtype() {
    String[] constraints = {"V :: atom()", "is_subtype(V, atom())", "V :: atom(), is_subtype(V, term())"};
    List<String> rejected = new ArrayList<>();
    for (String constraint : constraints) {
      PsiFile file = parseText("-module(t).\n-export([f/1]).\n-spec f(term()) -> V when " + constraint + ".\nf(X) -> X.\n");
      if (PsiTreeUtil.hasErrorElements(file)) rejected.add(constraint);
    }
    assertEmpty("Parsed with a syntax error", rejected);
  }

  // #1667: a macro can expand to a whole constraint, erlc compiles -define(C, V :: atom()) with each of these.
  public void testSpecConstraintMacro() {
    String[] specs = {
      "-spec f(term()) -> V when ?C.",
      "-spec f(term()) -> V when ?C, is_subtype(V, term()).",
      "-spec f(term()) -> V when ?C; (atom()) -> atom().",
      "-spec(f(term()) -> V when ?C).",
      "-callback f(term()) -> V when ?C."
    };
    List<String> rejected = new ArrayList<>();
    for (String spec : specs) {
      PsiFile file = parseText("-module(t).\n-export([f/1]).\n-define(C, V :: atom()).\n" + spec + "\nf(X) -> X.\n");
      if (PsiTreeUtil.hasErrorElements(file)) rejected.add(spec);
    }
    assertEmpty("Parsed with a syntax error", rejected);
  }

  // #1667: an unfinished `when X` above a function must not take the function into the constraint.
  public void testUnfinishedSpecConstraintKeepsNextFunction() {
    PsiFile file = parseText("-module(t).\n-export([f/1]).\n-spec f(X) -> X when X\nf(X) -> X.\n");
    assertTrue(PsiTreeUtil.hasErrorElements(file));
    assertSize(1, PsiTreeUtil.findChildrenOfType(file, ErlangFunction.class));
  }

  public void testEmptyParenthesesAsFunctionBody() {
    assertHasSyntaxError("-module(t).\n-export([f/0]).\nf() ->\n    ().\n");
  }

  public void testEmptyParenthesesAsMatchRight() {
    assertHasSyntaxError("-module(t).\n-export([f/0]).\nf() -> X = (), X.\n");
  }

  public void testEmptyParenthesesAsComprehensionFilter() {
    assertHasSyntaxError("-module(t).\n-export([f/1]).\nf(X) -> [Z || Z <- X, ()].\n");
  }

  public void testEmptyParenthesesAsIfGuard() {
    assertHasSyntaxError("-module(t).\n-export([f/0]).\nf() -> if () -> ok end.\n");
  }

  public void testEmptyParenthesesOutsideExpressions() {
    PsiFile file = parseText("-module(t).\n-export([f/0]).\n-type t() :: ok.\n-spec f() -> t().\n-callback c() -> ok.\n-define(PAR, ()).\n" +
                             "f() -> F = fun() -> ok end, F(), g(), X = (1), <<(X), (X):8>>, ((X)).\n");
    assertFalse(toParseTreeText(file, skipSpaces(), includeRanges()), PsiTreeUtil.hasErrorElements(file));
  }

  public void testEmptyParenthesesErrorKeepsNextFunction() {
    PsiFile file = parseText("-module(t).\nf() -> ().\ng() -> ok.\n");
    assertTrue(PsiTreeUtil.hasErrorElements(file));
    Collection<ErlangFunction> functions = PsiTreeUtil.findChildrenOfType(file, ErlangFunction.class);
    assertEquals(2, functions.size());
  }

  // #1647: erlc parses a macro body and a macro argument only after the expansion, where () can be a fragment.
  public void testEmptyParenthesesInMacros() {
    String[] modules = {
      "-define(F, () -> ok).\n",
      "-define(P, (), ok).\n",
      "-define(E, (())).\n",
      "-define(P, ()) .\n",
      "-define(CALL(F, A), F A).\nf() -> ?CALL(g, ()).\n",
      "-define('CALL'(F, A), F A).\nf() -> ?'CALL'(g, ()).\n",
    };
    List<String> rejected = new ArrayList<>();
    for (String module : modules) {
      if (PsiTreeUtil.hasErrorElements(parseText("-module(t).\n" + module))) rejected.add(module);
    }
    assertEmpty("Parsed with a syntax error", rejected);
  }

  public void testEmptyParenthesesAsCallArgument() {
    assertHasSyntaxError("-module(t).\nf() -> g(()).\n");
  }

  private void assertHasSyntaxError(String text) {
    PsiFile file = parseText(text);
    assertTrue("erlc reports a syntax error before ')', the parser gives none:\n" +
               toParseTreeText(file, skipSpaces(), includeRanges()),
               PsiTreeUtil.hasErrorElements(file));
  }

  private PsiFile parseText(String text) {
    return createPsiFile("test.erl", text);
  }

  public void testUnterminatedLongQuoteRunScalesLinearly() throws IOException {
    int smallRunReads = charactersReadForUnterminatedQuoteRun(10_000);
    int largeRunReads = charactersReadForUnterminatedQuoteRun(40_000);
    assertTrue("Long quote run read " + largeRunReads + " characters after " + smallRunReads + " for one quarter the input",
               largeRunReads < smallRunReads * 8);
  }

  // #1318: the lexer reads each quote once, so the verdict of the check above must not change between runs.
  public void testUnterminatedLongQuoteRunTimingCheckIsStable() throws IOException {
    for (int round = 0; round < 20; round++) {
      assertEquals(10_000, charactersReadForUnterminatedQuoteRun(10_000));
      assertEquals(40_000, charactersReadForUnterminatedQuoteRun(40_000));
    }
  }

  private static int charactersReadForUnterminatedQuoteRun(int quoteCount) throws IOException {
    ReadCountingSequence input = new ReadCountingSequence("\"".repeat(quoteCount));
    _ErlangLexer lexer = new _ErlangLexer((Project) null);
    lexer.reset(input, 0, input.length(), _ErlangLexer.YYINITIAL);
    int tokens = 0;
    while (lexer.advance() != null) tokens++;
    assertEquals(1, tokens);
    return input.reads;
  }

  private static final class ReadCountingSequence implements CharSequence {
    private final String text;
    private int reads;

    ReadCountingSequence(String text) { this.text = text; }

    @Override public int length() { return text.length(); }

    @Override public char charAt(int index) { reads++; return text.charAt(index); }

    @Override public CharSequence subSequence(int start, int end) { return text.subSequence(start, end); }

    @Override public String toString() { return text; }
    }
}
