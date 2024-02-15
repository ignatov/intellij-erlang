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
import org.intellij.erlang.psi.ErlangSpecification;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.psi.impl.ErlangSpecificationImpl;
import org.intellij.erlang.stubs.ErlangSpecificationStub;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangSpecificationElementType extends ErlangStubElementType<ErlangSpecificationStub, ErlangSpecification> {
  private static final ErlangSpecification[] EMPTY_ARRAY = new ErlangSpecification[0];

  public static final ArrayFactory<ErlangSpecification> ARRAY_FACTORY = count -> count == 0 ? EMPTY_ARRAY : new ErlangSpecification[count];

  public ErlangSpecificationElementType(@NonNls @NotNull String debugName) {
    super(debugName);
  }

  @Override
  public ErlangSpecification createPsi(@NotNull ErlangSpecificationStub stub) {
    return new ErlangSpecificationImpl(stub, this);
  }

  @Override
  public @NotNull ErlangSpecificationStub createStub(@NotNull ErlangSpecification psi, StubElement parent) {
    return new ErlangSpecificationStub(parent, this, ErlangPsiImplUtil.getName(psi), ErlangPsiImplUtil.getArity(psi));
  }

  @Override
  public void serialize(@NotNull ErlangSpecificationStub stub, @NotNull StubOutputStream stream) throws IOException {
    stream.writeName(stub.getName());
    stream.writeInt(stub.getArity());
  }

  @NotNull
  @Override
  public ErlangSpecificationStub deserialize(@NotNull StubInputStream dataStream,
                                             StubElement parentStub) throws IOException {
    return new ErlangSpecificationStub(parentStub, this, dataStream.readName(), dataStream.readInt());
  }
}
