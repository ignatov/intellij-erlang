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

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.ArrayFactory;
import org.intellij.erlang.psi.ErlangIncludeLib;
import org.intellij.erlang.psi.ErlangIncludeString;
import org.intellij.erlang.psi.impl.ErlangIncludeLibImpl;
import org.intellij.erlang.stubs.ErlangIncludeLibStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangIncludeLibElementType extends ErlangStubElementType<ErlangIncludeLibStub, ErlangIncludeLib> {
  public static final ErlangIncludeLib[] EMPTY_ARRAY = new ErlangIncludeLib[0];

  public static final ArrayFactory<ErlangIncludeLib> ARRAY_FACTORY = new ArrayFactory<ErlangIncludeLib>() {
    @NotNull
    @Override
    public ErlangIncludeLib[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangIncludeLib[count];
    }
  };

  public ErlangIncludeLibElementType(String name) {
    super(name);
  }

  @Override
  public ErlangIncludeLib createPsi(@NotNull ErlangIncludeLibStub stub) {
    return new ErlangIncludeLibImpl(stub, this);
  }

  @Override
  public ErlangIncludeLibStub createStub(@NotNull ErlangIncludeLib psi, StubElement parentStub) {
    ErlangIncludeString includeString = psi.getIncludeString();
    String text = includeString != null ? StringUtil.unquoteString(includeString.getText()) : "";
    return new ErlangIncludeLibStub(parentStub, this, text);
  }

  @Override
  public void serialize(@NotNull ErlangIncludeLibStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getString());
  }

  @NotNull
  @Override
  public ErlangIncludeLibStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangIncludeLibStub(parentStub, this, dataStream.readName());
  }
}
