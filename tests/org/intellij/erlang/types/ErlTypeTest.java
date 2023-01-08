/*
 * Copyright 2012-2023 Sergey Ignatov
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

package org.intellij.erlang.types;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ErlTypeTest {
  @Test
  void testSubclassNumber() {
    assertTrue(ErlSimpleType.FLOAT.isSubtypeOf(ErlSimpleType.NUMBER));
    assertTrue(ErlSimpleType.INTEGER.isSubtypeOf(ErlSimpleType.NUMBER));

    assertFalse(ErlSimpleType.NUMBER.isSubtypeOf(ErlSimpleType.ATOM));
    assertFalse(ErlSimpleType.NUMBER.isSubtypeOf(ErlSimpleType.INTEGER));
    assertFalse(ErlSimpleType.NUMBER.isSubtypeOf(ErlSimpleType.FLOAT));
  }

  @Test
  void testSubclassAtom() {
    assertTrue(ErlSimpleType.BOOLEAN.isSubtypeOf(ErlSimpleType.ATOM));

    assertFalse(ErlSimpleType.ATOM.isSubtypeOf(ErlSimpleType.BOOLEAN));
  }

  @Test
  void testSubclassRecord() {
    var recordType = new ErlRecordType("record", new ErlRecordType.RecordMember[] {
      new ErlRecordType.RecordMember("int", ErlSimpleType.INTEGER),
      new ErlRecordType.RecordMember("float", ErlSimpleType.FLOAT),
      new ErlRecordType.RecordMember("atom", ErlSimpleType.ATOM),
    });
    assertTrue(recordType.isSubtypeOf(ErlSimpleType.TUPLE));
    assertEquals("#record{int :: integer(), float :: float(), atom :: atom()}",
                 recordType.toDefinitionString());
    assertEquals("#record{}",
                 recordType.toReferenceString());
  }
}