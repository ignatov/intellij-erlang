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
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangIncludeString;
import org.intellij.erlang.psi.impl.ErlangElementFactory;

abstract class ErlangStringContainerStub<T extends ErlangCompositeElement> extends StubBase<T> {
  private final StringRef myStringRef;

  public ErlangStringContainerStub(StubElement parent, IStubElementType elementType, String name) {
    this(parent, elementType, StringRef.fromString(name));
  }

  public ErlangStringContainerStub(StubElement parent, IStubElementType elementType, StringRef nameRef) {
    super(parent, elementType);
    myStringRef = nameRef;
  }

  public String getString() {
    return myStringRef.getString();
  }
  
  public ErlangIncludeString getIncludeString() {
    return ErlangElementFactory.createIncludeString(getProject(), getString());
  }
}
