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

package org.intellij.erlang.stubs.types;

import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.ArrayFactory;
import org.intellij.erlang.psi.ErlangCallbackFunction;
import org.intellij.erlang.psi.impl.ErlangCallbackFunctionImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.stubs.ErlangCallbackFunctionStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangCallbackFunctionStubElementType extends ErlangStubElementType<ErlangCallbackFunctionStub, ErlangCallbackFunction> {
  public static final ErlangCallbackFunction[] EMPTY_ARRAY = new ErlangCallbackFunction[0];

  public static final ArrayFactory<ErlangCallbackFunction> ARRAY_FACTORY = new ArrayFactory<ErlangCallbackFunction>() {
    @NotNull
    @Override
    public ErlangCallbackFunction[] create(int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangCallbackFunction[count];
    }
  };

  public ErlangCallbackFunctionStubElementType(@NotNull String name) {
    super(name);
  }

  @Override
  public ErlangCallbackFunction createPsi(@NotNull ErlangCallbackFunctionStub stub) {
    return new ErlangCallbackFunctionImpl(stub, this);
  }

  @Override
  public ErlangCallbackFunctionStub createStub(@NotNull ErlangCallbackFunction psi, StubElement parentStub) {
    String name = ErlangPsiImplUtil.getName(psi.getQAtom());
    return new ErlangCallbackFunctionStub(parentStub, this, name);
  }

  @Override
  public void serialize(@NotNull ErlangCallbackFunctionStub stub,
                        @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
  }

  @NotNull
  @Override
  public ErlangCallbackFunctionStub deserialize(@NotNull StubInputStream dataStream,
                                                StubElement parentStub) throws IOException {
    return new ErlangCallbackFunctionStub(parentStub, this, dataStream.readName());
  }
}
