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
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangFunctionImpl;
import org.intellij.erlang.stubs.ErlangFunctionStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangFunctionStubElementType extends ErlangNamedStubElementType<ErlangFunctionStub, ErlangFunction> {
  public static final ErlangFunction[] EMPTY_ARRAY = new ErlangFunction[0];

  public static final ArrayFactory<ErlangFunction> ARRAY_FACTORY = new ArrayFactory<ErlangFunction>() {
    @NotNull
    @Override
    public ErlangFunction[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangFunction[count];
    }
  };

  public ErlangFunctionStubElementType(String name) {
    super(name);
  }

  @Override
  public ErlangFunction createPsi(@NotNull ErlangFunctionStub stub) {
    return new ErlangFunctionImpl(stub, this);
  }

  @Override
  public ErlangFunctionStub createStub(@NotNull ErlangFunction psi, StubElement parentStub) {
    return new ErlangFunctionStub(parentStub, this, psi.getName(), psi.getArity(), psi.isExported());
  }

  @Override
  public void serialize(@NotNull ErlangFunctionStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
    dataStream.writeInt(stub.getArity());
    dataStream.writeBoolean(stub.isExported());
  }

  @NotNull
  @Override
  public ErlangFunctionStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangFunctionStub(parentStub, this, dataStream.readName(), dataStream.readInt(), dataStream.readBoolean());
  }
}
