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
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.impl.ErlangRecordDefinitionImpl;
import org.intellij.erlang.stubs.ErlangRecordDefinitionStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangRecordDefinitionElementType extends ErlangNamedStubElementType<ErlangRecordDefinitionStub, ErlangRecordDefinition> {
  public static final ErlangRecordDefinition[] EMPTY_ARRAY = new ErlangRecordDefinition[0];

  public static final ArrayFactory<ErlangRecordDefinition> ARRAY_FACTORY = new ArrayFactory<ErlangRecordDefinition>() {
    @NotNull
    @Override
    public ErlangRecordDefinition[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangRecordDefinition[count];
    }
  };

  public ErlangRecordDefinitionElementType(String name) {
    super(name);
  }

  @Override
  public ErlangRecordDefinition createPsi(@NotNull ErlangRecordDefinitionStub stub) {
    return new ErlangRecordDefinitionImpl(stub, this);
  }

  @Override
  public ErlangRecordDefinitionStub createStub(@NotNull ErlangRecordDefinition psi, StubElement parentStub) {
    return new ErlangRecordDefinitionStub(parentStub, this, psi.getName());
  }

  @Override
  public void serialize(@NotNull ErlangRecordDefinitionStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getName());
  }

  @NotNull
  @Override
  public ErlangRecordDefinitionStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangRecordDefinitionStub(parentStub, this, dataStream.readName());
  }
}
