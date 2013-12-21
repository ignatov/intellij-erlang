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
import org.intellij.erlang.psi.ErlangInclude;
import org.intellij.erlang.psi.ErlangIncludeString;
import org.intellij.erlang.psi.impl.ErlangIncludeImpl;
import org.intellij.erlang.stubs.ErlangIncludeStub;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangIncludeElementType extends ErlangStubElementType<ErlangIncludeStub, ErlangInclude> {
  public static final ErlangInclude[] EMPTY_ARRAY = new ErlangInclude[0];

  public static final ArrayFactory<ErlangInclude> ARRAY_FACTORY = new ArrayFactory<ErlangInclude>() {
    @NotNull
    @Override
    public ErlangInclude[] create(final int count) {
      return count == 0 ? EMPTY_ARRAY : new ErlangInclude[count];
    }
  };

  public ErlangIncludeElementType(String name) {
    super(name);
  }

  @Override
  public ErlangInclude createPsi(@NotNull ErlangIncludeStub stub) {
    return new ErlangIncludeImpl(stub, this);
  }

  @Override
  public ErlangIncludeStub createStub(@NotNull ErlangInclude psi, StubElement parentStub) {
    ErlangIncludeString includeString = psi.getIncludeString();
    String text = includeString != null ? StringUtil.unquoteString(includeString.getText()) : "";
    return new ErlangIncludeStub(parentStub, this, text);
  }

  @Override
  public void serialize(@NotNull ErlangIncludeStub stub, @NotNull StubOutputStream dataStream) throws IOException {
    dataStream.writeName(stub.getString());
  }

  @NotNull
  @Override
  public ErlangIncludeStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
    return new ErlangIncludeStub(parentStub, this, dataStream.readName());
  }
}
