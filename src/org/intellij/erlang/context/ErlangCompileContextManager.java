/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.context;

import com.intellij.openapi.components.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@State(
  name = "ErlangCompileContexts",
  storages = {
    @Storage(file = StoragePathMacros.PROJECT_FILE),
    @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/erlangCompileContexts.xml", scheme = StorageScheme.DIRECTORY_BASED)
  }
)
public class ErlangCompileContextManager implements PersistentStateComponent<ErlangCompileContextManager> {
  @Tag("compileContexts")
  @AbstractCollection(surroundWithTag = false)
  public List<ErlangCompileContext> myProjectCompileContexts = createDefaultContexts();

  private final Project myProject;

  ErlangCompileContextManager(@NotNull Project project) {
    myProject = project;
  }

  @NotNull
  public static ErlangCompileContextManager getInstance(@NotNull Project project) {
    return ServiceManager.getService(project, ErlangCompileContextManager.class);
  }

  @NotNull
  public ErlangCompileContext getContext(@Nullable VirtualFile file) {
    //TODO build context for a file: merge compiler contexts from file, module and project. Also check if it's under tests source root.
    ErlangCompileContext defaultCompileContext = ContainerUtil.getFirstItem(myProjectCompileContexts);
    ErlangCompileContext context = defaultCompileContext != null ? defaultCompileContext : createDefaultContext();
    context.setProject(myProject);
    return context;
  }

  @Nullable
  @Override
  public ErlangCompileContextManager getState() {
    return this;
  }

  @Override
  public void loadState(ErlangCompileContextManager erlangCompileContextManager) {
    XmlSerializerUtil.copyBean(erlangCompileContextManager, this);
  }

  private static List<ErlangCompileContext> createDefaultContexts() {
    return Arrays.asList(
      createDefaultContext(),
      createDefaultTestContext()
    );
  }

  private static ErlangCompileContext createDefaultContext() {
    return new ErlangCompileContext("Default");
  }

  private static ErlangCompileContext createDefaultTestContext() {
    return new ErlangCompileContext(null, "Test", Collections.singletonMap("TEST", ""), ContainerUtil.<String>emptyList());
  }
}
