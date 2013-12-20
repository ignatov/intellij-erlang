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

package org.intellij.erlang.parser;

import org.intellij.erlang.ErlangParserDefinition;

public class ErlangParserTest extends ErlangParserTestBase {
  public ErlangParserTest() {
    super("parser", "erl", new ErlangParserDefinition());
  }

  public void testHelloWorld()        { doTest(true, false); }
  public void testExport()            { doTest(true, false); }
  public void testH()                 { doTest(true, false); }
  public void testMnesia()            { doTest(true, false); }
  public void testIsDigits()          { doTest(true, false); }
  public void testDialyzerDataflow()  { doTest(true, false); }
  public void testTest()              { doTest(true, false); }
  public void testRecords()           { doTest(true, false); }
  public void testDialyzerClParse()   { doTest(true, false); }
  public void testMp4Mux()            { doTest(true, false); }
  public void testComments()          { doTest(true, false); }
  public void testMultiline()         { doTest(true, false); }
  public void testRecordExpression()  { doTest(true, false); }
  public void testPeriods()           { doTest(true, true);  }
  public void test19()                { doTest(true, true);  }
  public void test25()                { doTest(true, false); }
  public void test26()                { doTest(true, false); }
  public void testFailedRecord()      { doTest(true, true);  }
  public void testRecords2()          { doTest(true, false); }
  public void testAtomWithArity()     { doTest(true, false); }
  public void test77()                { doTest(true, false); }
  public void test74()                { doTest(true, false); }
  public void testErrors()            { doTest(true, true);  }
  public void test51()                { doTest(true, false); } // performance
  public void test96()                { doTest(true, false); }
  public void test30()                { doTest(true, true);  }
  public void test127()               { doTest(true, true);  }
  public void testEunit()             { doTest(true, false); }
  public void testMeck()              { doTest(true, false); }
  public void test145()               { doTest(true, false); }
  public void test145_2()             { doTest(true, false); }
  public void test29()                { doTest(true, false); }
  public void testEmptyRecord()       { doTest(true, false); }
  public void testTypeAgain()         { doTest(true, false); }
  public void testTest2()             { doTest(true, true);  }
  public void testDebug()             { doTest(true, true);  }
  public void testDebug2()            { doTest(true, true);  }
  public void test175()               { doTest(true, true);  }
  public void test182()               { doTest(true, true);  }
  public void test183()               { doTest(true, true);  }
  public void test210()               { doTest(true, false); }
  public void test213()               { doTest(true, false); }
  public void test223()               { doTest(true, false); }
  public void testQuotation()         { doTest(true, true);  }
  public void testRiakHacks()         { doTest(true, true);  }
  public void testRecover()           { doTest(true, true);  }
  public void testRecover2()          { doTest(true, true);  }
  public void testBeginEnd()          { doTest(true, true);  }
  public void testBeginEnd2()         { doTest(true, true);  }
  public void test296()               { doTest(true, true);  }
  public void test304()               { doTest(true, true);  }
  public void test305()               { doTest(true, true);  }
  public void test306()               { doTest(true, true);  }
  public void testEscriptShebang1()   { doTest(true, false); }
  public void testEscriptShebang2()   { doTest(true, true);  }
  public void test325()               { doTest(true, true);  }
  public void test390()               { doTest(true, true);  }
}