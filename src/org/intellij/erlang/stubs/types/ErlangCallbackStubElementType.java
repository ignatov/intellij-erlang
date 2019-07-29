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

package org.intellij.erlang.stubs.types;

import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import org.intellij.erlang.psi.ErlangCallbackSpec;
import org.intellij.erlang.psi.impl.ErlangCallbackSpecImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.stubs.ErlangCallbackSpecStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangCallbackStubElementType extends ErlangStubElementType<ErlangCallbackSpecStub, ErlangCallbackSpec> {
  public ErlangCallbackStubElementType(@NotNull String name) {
    super(name);
  }

  @Override
  public ErlangCallbackSpec createPsi(@NotNull ErlangCallbackSpecStub stub) {
    return new ErlangCallbackSpecImpl(stub, this);
  }

  @NotNull
  @Override
  public ErlangCallbackSpecStub createStub(@NotNull ErlangCallbackSpec spec, StubElement parentStub) {
    String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
    int arity = ErlangPsiImplUtil.getCallBackSpecArguments(spec).size();
    boolean isOptional = ErlangPsiImplUtil.isOptional(spec);
    return new ErlangCallbackSpecStub(parentStub, this, name, arity, isOptional);
  }

  @Override
  public void serialize(@NotNull ErlangCallbackSpecStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
    dataStream.writeInt(stub.getArity());
    dataStream.writeBoolean(stub.isOptional());
  }

  @NotNull
  @Override
  public ErlangCallbackSpecStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangCallbackSpecStub(parentStub, this, dataStream.readName(), dataStream.readInt(), dataStream.readBoolean());
  }
}
