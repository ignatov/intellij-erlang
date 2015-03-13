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

package org.intellij.erlang.context;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.options.UnnamedConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.MasterDetailsComponent;
import com.intellij.openapi.ui.NamedItemsListEditor;
import com.intellij.openapi.ui.Namer;
import com.intellij.openapi.util.Cloner;
import com.intellij.openapi.util.Factory;
import com.intellij.util.containers.ContainerUtil;
import gnu.trove.Equality;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

public class ErlangCompileContextListConfigurable extends NamedItemsListEditor<ErlangCompileContext> implements SearchableConfigurable {
  public static final String ID = "Erlang Compile Contexts";

  private final ErlangCompileContextManager myContextManager;

  protected ErlangCompileContextListConfigurable(ErlangCompileContextManager contextManager) {
    //noinspection unchecked
    super(new MyNamer(), new MyFactory(contextManager.getProject()), new MyCloner(), (Equality<ErlangCompileContext>)Equality.CANONICAL, contextManager.getProjectCompileContexts());

    myContextManager = contextManager;
  }

  @Override
  protected UnnamedConfigurable createConfigurable(ErlangCompileContext item) {
    return new ErlangCompileContextConfigurable(item);
  }

  @Nls
  @Override
  public String getDisplayName() {
    return ID;
  }

  @NotNull
  @Override
  public String getId() {
    return ID;
  }

  @Nullable
  @Override
  public Runnable enableSearch(String option) {
    return null;
  }

  @Override
  public void apply() throws ConfigurationException {
    List<ErlangCompileContext> contexts = getCurrentItems();
    Set<String> contextNames = ContainerUtil.newHashSet(contexts.size());
    for (ErlangCompileContext context : contexts) {
      if (!contextNames.add(context.name)) {
        throw new ConfigurationException("Context name is not unique: '" + context.name + "'");
      }
    }

    super.apply();

    myContextManager.setProjectCompileContexts(getItems());
  }

  private static class MyNamer implements Namer<ErlangCompileContext> {
    @Override
    public String getName(ErlangCompileContext erlangCompileContext) {
      return erlangCompileContext.name;
    }

    @Override
    public boolean canRename(ErlangCompileContext item) {
      return true;
    }

    @Override
    public void setName(ErlangCompileContext erlangCompileContext, String name) {
      erlangCompileContext.name = name;
    }
  }

  private static class MyFactory implements Factory<ErlangCompileContext> {
    private final Project myProject;

    private MyFactory(Project project) {
      myProject = project;
    }

    @Override
    public ErlangCompileContext create() {
      return new ErlangCompileContext(myProject, "context");
    }
  }

  private static class MyCloner implements Cloner<ErlangCompileContext> {

    @Override
    public ErlangCompileContext cloneOf(ErlangCompileContext erlangCompileContext) {
      return erlangCompileContext.clone();
    }

    @Override
    public ErlangCompileContext copyOf(ErlangCompileContext erlangCompileContext) {
      return erlangCompileContext.clone();
    }
  }
}
