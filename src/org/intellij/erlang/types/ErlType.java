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

public interface ErlType {
  enum Kind {
    ANY,
    NONE, // empty type
    ATOM,
    BINARY,
    BIT_STRING, // also BIT_STRING is a BINARY
    BOOLEAN, // true|false, also BOOLEAN is an ATOM
    FLOAT, // also FLOAT is a NUMBER
    INTEGER, // also INTEGER is a NUMBER
    CHAR, // also CHAR is an INTEGER
    NUMBER, // equals INTEGER|FLOAT
    LIST,
    STRING, // also STRING is LIST
    NIL, // also NIL is LIST
    MAP,
    PID,
    PORT,
    REF,
    TUPLE,
    /**
     * A record with tag and fields
     * @see ErlRecordType
     */
    RECORD,
    FUN, // a generic function type, any arity, any return
  }

  Kind getKind();
  boolean isSubtypeOf(ErlType other);
  /**
   * @return String which is usable in the right side of a type definition
   */
  String toDefinitionString();

  /**
   * @return String which refers to the type already defined in the type-scope.
   */
  String toReferenceString();
}
