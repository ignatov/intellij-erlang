/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.runconfig;

import junit.framework.TestCase;

import java.util.List;

public class ErlangRunningStateTest extends TestCase {
  public void testSimpleArguments() {
    assertArguments("one two", "one", "two");
  }

  public void testQuotedArguments() {
    assertArguments("\"hello world\" \"\" 'quoted atom'", "\"hello world\"", "\"\"", "'quoted atom'");
  }

  public void testStructuredArguments() {
    assertArguments("{ok, 1} [one, two] #{key => value} <<1, 2>>",
                    "{ok, 1}", "[one, two]", "#{key => value}", "<<1, 2>>");
  }

  public void testNestedQuotedArgument() {
    assertArguments("{ok, \"hello world\"} ~s/foo bar/", "{ok, \"hello world\"}", "~s/foo bar/");
  }

  public void testQuotedModuleAndFunctionNames() {
    ErlangRunningState.ErlangEntryPoint entryPoint =
      ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("'my mod'  'hello world'", "");

    assertNotNull(entryPoint);
    assertEquals("my mod", entryPoint.getModuleName());
    assertEquals("hello world", entryPoint.getFunctionName());
  }

  public void testControlQuestionMarkEscape() {
    ErlangRunningState.ErlangEntryPoint entryPoint =
      ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("m '\\^?'", "");

    assertNotNull(entryPoint);
    assertEquals("\u007f", entryPoint.getFunctionName());
  }

  public void testUnterminatedQuotedFunctionName() {
    assertNull(ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("m 'hello world", ""));
  }

  // erlc reads '\\' as \, '\r\t' as [13, 9], '\x{1}' as [1], '\x{7f}' as [127] and 'a\x{1f}b' as [97, 31, 98]
  public void testQuotedAtomEscapes() {
    assertEquals("'\\\\'", ErlangRunningState.quotedAtom("\\"));
    assertEquals("'it\\'s'", ErlangRunningState.quotedAtom("it's"));
    assertEquals("'\\n\\r\\t'", ErlangRunningState.quotedAtom("\n\r\t"));
    assertEquals("'\\x{1}'", ErlangRunningState.quotedAtom("\u0001"));
    assertEquals("'\\x{7f}'", ErlangRunningState.quotedAtom("\u007f"));
    assertEquals("'a\\x{1f}b'", ErlangRunningState.quotedAtom("a\u001fb"));
  }

  public void testQuotedAtomPlainAndQuoted() {
    assertEquals("a_b@1", ErlangRunningState.quotedAtom("a_b@1"));
    assertEquals("'Start'", ErlangRunningState.quotedAtom("Start"));
    assertEquals("'case'", ErlangRunningState.quotedAtom("case"));
    assertEquals("'my mod'", ErlangRunningState.quotedAtom("my mod"));
    assertEquals("''", ErlangRunningState.quotedAtom(""));
  }

  public void testQuotedAtomRoundTrip() {
    for (String name : List.of("\\", "it's", "\n\r\t", "\u0001\u007f", "Start", "case", "a b")) {
      String quoted = ErlangRunningState.quotedAtom(name);
      assertEquals(quoted, name, ErlangRunningState.atomName(quoted.substring(1, quoted.length() - 1)));
    }
  }

  public void testAtomNameOfEmptyAtom() {
    assertEquals("", ErlangRunningState.atomName("''"));
  }

  // erlc reads '\b\d\e\f\n\r\s\t\v' as [8, 127, 27, 12, 10, 13, 32, 9, 11]
  public void testAtomNameSingleCharacterEscapes() {
    assertEquals("\b\u007f\u001b\f\n\r \t\u000b", ErlangRunningState.atomName("\\b\\d\\e\\f\\n\\r\\s\\t\\v"));
  }

  // erlc reads '\^a\^Z\^?' as [1, 26, 127]
  public void testAtomNameControlEscapes() {
    assertEquals("\u0001\u001a\u007f", ErlangRunningState.atomName("\\^a\\^Z\\^?"));
  }

  // erlc reads '\x41\x{263A}' as [65, 9786]
  public void testAtomNameHexEscapes() {
    assertEquals("A\u263a", ErlangRunningState.atomName("\\x41\\x{263A}"));
  }

  // erlc reads '\101\7\18' as [65, 7, 1, 56]
  public void testAtomNameOctalEscapes() {
    assertEquals("A\u0007\u00018", ErlangRunningState.atomName("\\101\\7\\18"));
  }

  // erlc reads '\1 a' as [1, 32, 97] and 'a\1' as [97, 1]
  public void testAtomNameShortOctalEscapes() {
    assertEquals("\u0001 a", ErlangRunningState.atomName("\\1 a"));
    assertEquals("a\u0001", ErlangRunningState.atomName("a\\1"));
  }

  // erlc reads '\z\'\\' as [122, 39, 92]
  public void testAtomNameOtherEscapes() {
    assertEquals("z'\\", ErlangRunningState.atomName("\\z\\'\\\\"));
  }

  // erlc rejects these; nothing is thrown: a trailing \ is kept, a trailing \^ or \x is dropped,
  // an unclosed \x{41 is decoded and an invalid \x{...} keeps only the text between the braces
  public void testAtomNameIncompleteEscapes() {
    assertEquals("a\\", ErlangRunningState.atomName("a\\"));
    assertEquals("a", ErlangRunningState.atomName("a\\^"));
    assertEquals("A", ErlangRunningState.atomName("\\x{41"));
    assertEquals("a", ErlangRunningState.atomName("a\\x"));
    assertEquals("zz", ErlangRunningState.atomName("\\x{zz}"));
    assertEquals("110000", ErlangRunningState.atomName("\\x{110000}"));
  }

  public void testEscapedQuotedFunctionName() {
    ErlangRunningState.ErlangEntryPoint entryPoint =
      ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("'my\\smod' 'it\\'s\\x{41}'", "");

    assertNotNull(entryPoint);
    assertEquals("my mod", entryPoint.getModuleName());
    assertEquals("it'sA", entryPoint.getFunctionName());
  }

  public void testWrongNumberOfNames() {
    assertNull(ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("m", ""));
    assertNull(ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("m f g", ""));
    assertNull(ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("m 'f'g", ""));
  }

  private static void assertArguments(String params, String... expected) {
    ErlangRunningState.ErlangEntryPoint entryPoint =
      ErlangRunningState.ErlangEntryPoint.fromModuleAndFunction("sample start", params);

    assertNotNull(entryPoint);
    assertEquals(List.of(expected), entryPoint.getArgsList());
  }
}
