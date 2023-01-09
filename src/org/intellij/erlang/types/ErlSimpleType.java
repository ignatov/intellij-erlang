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

/**
 * Describes an Erlang type with optional type parameters.
 * Implements basic types without parameters, and more complex types like structs, maps, and user types
 * are implemented in subclasses.
 *
 * @see ErlType
 */
public final class ErlSimpleType implements ErlType {
  private final Kind kind;

  public static ErlType ANY = new ErlSimpleType(Kind.ANY);
  public static ErlType NONE = new ErlSimpleType(Kind.NONE);
  public static ErlType FLOAT = new ErlSimpleType(Kind.FLOAT);
  public static ErlType INTEGER = new ErlSimpleType(Kind.INTEGER);
  public static ErlType NUMBER = new ErlSimpleType(Kind.NUMBER);
  public static ErlType BINARY = new ErlSimpleType(Kind.BINARY);
  public static ErlType LIST = new ErlSimpleType(Kind.LIST);
  public static ErlType NIL = new ErlSimpleType(Kind.NIL);
  public static ErlType STRING = new ErlSimpleType(Kind.STRING);
  public static ErlType TUPLE = new ErlSimpleType(Kind.TUPLE);
  public static ErlType MAP = new ErlSimpleType(Kind.MAP);
  public static ErlType ATOM = new ErlSimpleType(Kind.ATOM);
  public static ErlType BOOLEAN = new ErlSimpleType(Kind.BOOLEAN);
  public static ErlType CHAR = new ErlSimpleType(Kind.CHAR);
  public static ErlType FUN = new ErlSimpleType(Kind.FUN);

  public ErlSimpleType(Kind kind) {
    this.kind = kind;
  }

  @Override
  public String toString() {
    return this.toReferenceString();
  }

  /**
   * Handles cases implemented in this class only
   */
  @Override
  public boolean isSubtypeOf(ErlType other) {
    if (this.equals(other)) return true;

    Kind otherKind = other.getKind();

    if (otherKind == Kind.ANY) return true;

    switch (this.kind) {
      case BINARY -> {
        return otherKind == Kind.BIT_STRING;
      }
      case BOOLEAN -> {
        return otherKind == Kind.ATOM;
      }
      case FLOAT, INTEGER -> {
        // Integer is a NUMBER, but not a FLOAT
        return otherKind == Kind.NUMBER;
      }
      case CHAR -> {
        return otherKind == Kind.INTEGER;
      }
      case STRING, NIL -> {
        return otherKind == Kind.LIST;
      }
      default -> {
        return false;
      }
    }
  }

  @Override
  public String toDefinitionString() {
    // Simple types are not definable, they always exist
    return this.toReferenceString();
  }

  @Override
  public String toReferenceString() {
    switch (kind) {
      case ANY -> {
        return "any()";
      }
      case NONE -> {
        return "none()";
      }
      case ATOM -> {
        return "atom()";
      }
      case BINARY -> {
        return "binary()";
      }
      case BIT_STRING -> {
        return "bit_string()";
      }
      case BOOLEAN -> {
        return "boolean()";
      }
      case FLOAT -> {
        return "float()";
      }
      case INTEGER -> {
        return "integer()";
      }
      case CHAR -> {
        return "char()";
      }
      case NUMBER -> {
        return "number()";
      }
      case LIST -> {
        return "list()"; // TODO: add type parameters
      }
      case STRING -> {
        return "string()";
      }
      case NIL -> {
        return "nil()";
      }
      case MAP -> {
        return "map()";
      }
      case PID -> {
        return "pid()";
      }
      case PORT -> {
        return "port()";
      }
      case REF -> {
        return "reference()";
      }
      case TUPLE -> {
        return "tuple()";
      }
      case FUN -> {
        return "fun()";
      }
      default -> {
        assert false : "Type kind=%s toString: Not implemented".formatted(kind);
        return "none()";
      }
    }
  }

  @Override
  public Kind getKind() {
    return this.kind;
  }

  public Kind kind() {
    return kind;
  }
}
