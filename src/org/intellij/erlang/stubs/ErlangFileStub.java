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

package org.intellij.erlang.stubs;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.stubs.PsiFileStubImpl;
import com.intellij.psi.tree.IStubFileElementType;
import com.intellij.util.io.StringRef;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.stubs.types.ErlangFileElementType;

import java.util.HashSet;

public class ErlangFileStub extends PsiFileStubImpl<ErlangFile> {
  private final boolean myExportAll;
  private final StringRef myParseTransformsRef;

  public ErlangFileStub(ErlangFile file) {
    super(file);
    myExportAll = file.isExportedAll();
    HashSet<String> transforms = new HashSet<String>();
    file.addDeclaredParseTransforms(transforms);
    String join = StringUtil.join(transforms, ",");
    myParseTransformsRef = StringRef.fromString(join);
  }

  public ErlangFileStub(ErlangFile file, boolean exportAll, StringRef parseTransformsRef) {
    super(file);
    myExportAll = exportAll;
    myParseTransformsRef = parseTransformsRef;
  }

  public boolean isExportAll() {
    return myExportAll;
  }

  public String getParseTransforms() {
    return StringRef.toString(myParseTransformsRef);
  }

  @Override
  public IStubFileElementType getType() {
    return ErlangFileElementType.INSTANCE;
  }
}
