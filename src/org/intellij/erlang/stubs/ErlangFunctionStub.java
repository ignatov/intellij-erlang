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

package org.intellij.erlang.stubs;

import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;
import org.intellij.erlang.psi.ErlangFunction;

public class ErlangFunctionStub extends ErlangWithArityStub<ErlangFunction> {
  private final boolean myExported;

  public ErlangFunctionStub(StubElement parent, IStubElementType elementType, String name, int arity, boolean exported) {
    super(parent, elementType, name, arity);
    myExported = exported;
  }

  public ErlangFunctionStub(StubElement parent, IStubElementType elementType, StringRef name, int arity, boolean exported) {
    super(parent, elementType, name, arity);
    myExported = exported;
  }

  public boolean isExported() {
    return myExported;
  }
}
