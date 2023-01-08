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

import java.util.LinkedHashSet;
import java.util.List;

/**
 * A simple union type implementation, later to be upgraded to full union impl when type inference is implemented.
 */
public class ErlTypeUnion {
  private final LinkedHashSet<ErlType> members;

  public ErlTypeUnion(@Nullable List<ErlType> members) {
    this.members = members == null ? new LinkedHashSet<>() : new LinkedHashSet<>(members);
  }

  public void add(ErlType newMember) {
    members.removeIf(t -> t.isSubtypeOf(newMember)); // Adding a supertype drops subtypes
    members.add(newMember);
  }

  public String toString() {
    if (members.isEmpty()) return ErlSimpleType.NONE.toString();
    if (members.stream().anyMatch(t -> t.getKind() == ErlType.Kind.ANY)) return ErlSimpleType.ANY.toString();

    String[] membersAsStrings = members.stream().map(ErlType::toString).toArray(String[]::new);
    return String.join(" | ", membersAsStrings);
  }

  public boolean isEmpty() {
    return members.isEmpty();
  }
}
