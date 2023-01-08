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

import org.jetbrains.annotations.Nullable;

import java.util.HashMap;

public final class ErlRecordType implements ErlType {
  private final String tag;
  /**
   * Ordered storage for record members (fields)
   */
  private final RecordMember[] members;
  /**
   * Map storage for fast lookup by name
   */
  private final HashMap<String, RecordMember> membersLookup = new HashMap<>();

  public ErlRecordType(String tag, RecordMember[] members) {
    this.tag = tag;
    this.members = members;
    for (var member : members) {
      membersLookup.put(member.getName(), member);
    }
  }

  @Override
  public Kind getKind() {
    return Kind.RECORD;
  }

  @Override
  public boolean isSubtypeOf(ErlType other) {
    Kind otherKind = other.getKind();
    if (other instanceof ErlRecordType otherRecord) {
      // Check fields compatibility
      for (RecordMember field : members) {
        // Incompatible if:
        // Other doesn't have field with the same name
        // Other has field with same name but it isn't a supertype of our field
        if (!otherRecord.hasMember(field.name)) return false;

        RecordMember otherMember = otherRecord.getMember(field.name);
        assert otherMember != null;

        if (!field.type.isSubtypeOf(otherMember.getType())) {
          return false;
        }
      }
    }
    return otherKind == Kind.TUPLE;
  }

  @Override
  public String toDefinitionString() {
    var sb = new StringBuilder();
    var firstField = true;

    sb.append("#").append(tag).append("{");

    for (var member : members) {
      if (firstField) {
        firstField = false;
      }
      else {
        sb.append(", ");
      }
      sb.append(member.name)
        .append(" :: ")
        .append(member.type.toReferenceString());
    }
    sb.append("}");
    return sb.toString();
  }

  @Override
  public String toReferenceString() {
    return "#%s{}".formatted(this.tag);
  }

  @Override
  public String toString() {
    return this.toReferenceString();
  }

  private @Nullable ErlRecordType.RecordMember getMember(String name) {
    return this.membersLookup.get(name);
  }

  private boolean hasMember(String name) {
    return this.membersLookup.containsKey(name);
  }

  public String getTag() {
    return tag;
  }

  public RecordMember[] getMembers() {
    return members;
  }

  public static class RecordMember {
    private final String name;
    private final ErlType type;

    public RecordMember(String name, ErlType type) {
      this.name = name;
      this.type = type;
    }

    public String getName() {
      return name;
    }

    public ErlType getType() {
      return type;
    }
  }

}
