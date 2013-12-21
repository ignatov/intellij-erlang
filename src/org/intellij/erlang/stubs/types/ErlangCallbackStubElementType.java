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

import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.ArrayFactory;
import org.intellij.erlang.psi.ErlangCallbackSpec;
import org.intellij.erlang.psi.impl.ErlangCallbackSpecImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.stubs.ErlangCallbackSpecStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangCallbackStubElementType extends ErlangStubElementType<ErlangCallbackSpecStub, ErlangCallbackSpec> {
  public static final ErlangCallbackSpec[] EMPTY_ARRAY = new ErlangCallbackSpec[0];

  public static final ArrayFactory<ErlangCallbackSpec> ARRAY_FACTORY = new ArrayFactory<ErlangCallbackSpec>() {
    @NotNull
    @Override
    public ErlangCallbackSpec[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangCallbackSpec[count];
    }
  };

  public ErlangCallbackStubElementType(String name) {
    super(name);
  }

  @Override
  public ErlangCallbackSpec createPsi(@NotNull ErlangCallbackSpecStub stub) {
    return new ErlangCallbackSpecImpl(stub, this);
  }

  @Override
  public ErlangCallbackSpecStub createStub(@NotNull ErlangCallbackSpec psi, StubElement parentStub) {
    String name = ErlangPsiImplUtil.getCallbackSpecName(psi);
    int arity = ErlangPsiImplUtil.getCallBackSpecArguments(psi).size();
    return new ErlangCallbackSpecStub(parentStub, this, name, arity);
  }

  @Override
  public void serialize(@NotNull ErlangCallbackSpecStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
    dataStream.writeInt(stub.getArity());
  }

  @NotNull
  @Override
  public ErlangCallbackSpecStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangCallbackSpecStub(parentStub, this, dataStream.readName(), dataStream.readInt());
  }
}
