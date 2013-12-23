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
import org.intellij.erlang.psi.ErlangBehaviour;
import org.intellij.erlang.psi.impl.ErlangBehaviourImpl;
import org.intellij.erlang.stubs.ErlangBehaviourStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangBehaviourStubElementType extends ErlangStubElementType<ErlangBehaviourStub, ErlangBehaviour> {
  public static final ErlangBehaviour[] EMPTY_ARRAY = new ErlangBehaviour[0];

  public static final ArrayFactory<ErlangBehaviour> ARRAY_FACTORY = new ArrayFactory<ErlangBehaviour>() {
    @NotNull
    @Override
    public ErlangBehaviour[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangBehaviour[count];
    }
  };

  public ErlangBehaviourStubElementType(String name) {
    super(name);
  }

  @Override
  public ErlangBehaviour createPsi(@NotNull ErlangBehaviourStub stub) {
    return new ErlangBehaviourImpl(stub, this);
  }

  @Override
  public ErlangBehaviourStub createStub(@NotNull ErlangBehaviour psi, StubElement parentStub) {
    return new ErlangBehaviourStub(parentStub, this, psi.getName());
  }

  @Override
  public void serialize(@NotNull ErlangBehaviourStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
  }

  @NotNull
  @Override
  public ErlangBehaviourStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangBehaviourStub(parentStub, this, dataStream.readName());
  }
}
