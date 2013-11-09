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

package org.intellij.erlang.stubs.index;

import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.jetbrains.annotations.NotNull;

public class ErlangAllNameIndex extends StringStubIndexExtension<ErlangNamedElement> {
  public static final StubIndexKey<String, ErlangNamedElement> KEY = StubIndexKey.createIndexKey("erlang.all.name");
  public static final int VERSION = 0;

  @Override
  public int getVersion() {
    return super.getVersion() + VERSION;
  }

  @NotNull
  public StubIndexKey<String, ErlangNamedElement> getKey() {
    return KEY;
  }
}
