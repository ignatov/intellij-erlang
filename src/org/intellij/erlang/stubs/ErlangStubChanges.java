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

package org.intellij.erlang.stubs;

/**
 * The changes of the stubs of Erlang files, one entry each. The stub version of {@code .erl} files
 * ({@code ErlangFileElementType.getStubVersion()}) is the number of entries.
 * <p>
 * Rules:
 * <ul>
 *   <li>Append one entry at the end, in the same change, for every change that needs a new stub version
 *   (AGENTS.md, "Stubs and indices"). An entry is the issue or pull request number and a short phrase, and ends with
 *   a comma, so that appending never edits the previous line.</li>
 *   <li>Never edit, reorder or remove an entry: that would lower the version or reuse an old one.</li>
 *   <li>Two pull requests that append here conflict on purpose: each of them needs its own version. Resolve the
 *   conflict by keeping both entries, master's first. CI checks that the entries of master stay a prefix.</li>
 * </ul>
 */
public final class ErlangStubChanges {
  public static final String[] ENTRIES = {
    "cf6536ac no reference search in stub generation",
    "#640 behaviour flag in the file stub for completion of behaviours",
    "#644 callback function stubs for -optional_callbacks",
    "881ba415 stubs for specifications",
    "b857bcb7 OTP 28 and 29 grammar changes",
    "#1123 integer constant expressions in integer types and size-and-unit binary types",
    "#1145 macros and integer expressions as binary type sizes",
    "#1114 EEP 64 triple-quoted string lexing changes file stubs",
    "#1119 a macro followed by a string literal parses as one string",
  };

  private ErlangStubChanges() {
  }
}
