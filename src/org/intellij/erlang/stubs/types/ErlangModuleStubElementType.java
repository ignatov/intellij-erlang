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
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.impl.ErlangModuleImpl;
import org.intellij.erlang.stubs.ErlangModuleStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangModuleStubElementType extends ErlangNamedStubElementType<ErlangModuleStub, ErlangModule> {
  public static final ErlangModule[] EMPTY_ARRAY = new ErlangModule[0];

  public static final ArrayFactory<ErlangModule> ARRAY_FACTORY = new ArrayFactory<ErlangModule>() {
    @NotNull
    @Override
    public ErlangModule[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangModule[count];
    }
  };

  public ErlangModuleStubElementType(String name) {
    super(name);
  }

  @Override
  public ErlangModule createPsi(@NotNull ErlangModuleStub stub) {
    return new ErlangModuleImpl(stub, this);
  }

  @Override
  public ErlangModuleStub createStub(@NotNull ErlangModule psi, StubElement parentStub) {
    return new ErlangModuleStub(parentStub, this, psi.getName());
  }

  @Override
  public void serialize(@NotNull ErlangModuleStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
  }

  @NotNull
  @Override
  public ErlangModuleStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangModuleStub(parentStub, this, dataStream.readName());
  }
}
