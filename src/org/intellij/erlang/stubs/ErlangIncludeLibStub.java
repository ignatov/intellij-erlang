/*
 * Copyright 2012-2014 Sergey Ignatov
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
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;
import org.intellij.erlang.psi.ErlangIncludeLib;
import org.jetbrains.annotations.Nullable;

public class ErlangIncludeLibStub extends ErlangStringContainerStub<ErlangIncludeLib> {
  public ErlangIncludeLibStub(StubElement parent, IStubElementType elementType, String name) {
    super(parent, elementType, name);
  }

  public ErlangIncludeLibStub(StubElement parent, IStubElementType elementType, @Nullable StringRef nameRef) {
    super(parent, elementType, nameRef);
  }
}
