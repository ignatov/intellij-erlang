/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;
import org.intellij.erlang.psi.ErlangCallbackFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangCallbackFunctionStub extends StubBase<ErlangCallbackFunction> {
  private final StringRef myNameRef;

  public ErlangCallbackFunctionStub(@NotNull StubElement parent, @NotNull IStubElementType elementType,
                                    @Nullable String name) {
    this(parent, elementType, StringRef.fromString(name));
  }

  public ErlangCallbackFunctionStub(@NotNull StubElement parent, @NotNull IStubElementType elementType,
                                    @Nullable StringRef nameRef) {
    super(parent, elementType);
    myNameRef = nameRef;
  }

  @NotNull
  public String getName() {
    return myNameRef != null ? myNameRef.getString() : "";
  }
}
