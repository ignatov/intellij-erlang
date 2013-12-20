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

package org.intellij.erlang.stubs.types;

import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.NamedStubBase;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.intellij.erlang.stubs.index.ErlangAllNameIndex;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public abstract class ErlangNamedStubElementType<S extends NamedStubBase<T>, T extends ErlangNamedElement> extends ErlangStubElementType<S, T> {
  public ErlangNamedStubElementType(@NonNls @NotNull String debugName) {
    super(debugName);
  }

  public void indexStub(@NotNull final S stub, @NotNull final IndexSink sink) {
    sink.occurrence(ErlangAllNameIndex.KEY, stub.getName());
  }

  @NotNull
  public String getExternalId() {
    return "erlang." + super.toString();
  }
}
