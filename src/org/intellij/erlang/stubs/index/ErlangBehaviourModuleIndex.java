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

package org.intellij.erlang.stubs.index;

import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.stubs.StubIndexKey;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

public class ErlangBehaviourModuleIndex extends StringStubIndexExtension<ErlangModule> {
  private static final String KEY_NAME = "erlang.behaviour.module";
  public static final StubIndexKey<String, ErlangModule> KEY = StubIndexKey.createIndexKey(KEY_NAME);
  public static final int VERSION = 0;

  @Override
  public int getVersion() {
    return super.getVersion() + VERSION;
  }

  @NotNull
  public StubIndexKey<String, ErlangModule> getKey() {
    return KEY;
  }

  @NotNull
  public static Collection<ErlangModule> getModules(@NotNull Project project, @NotNull GlobalSearchScope scope) {
    return StubIndex.getElements(KEY, KEY_NAME, project, scope, ErlangModule.class);
  }

  public static void indicateOccurence(@NotNull IndexSink sink) {
    sink.occurrence(KEY, KEY_NAME);
  }
}
