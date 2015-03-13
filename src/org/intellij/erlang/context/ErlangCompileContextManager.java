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
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.FileContentUtilCore;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.XmlSerializerUtil;
import com.intellij.util.xmlb.annotations.AbstractCollection;
import com.intellij.util.xmlb.annotations.Tag;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

@com.intellij.openapi.components.State(
  name = "ErlangCompileContexts",
  storages = {
    @Storage(file = StoragePathMacros.PROJECT_FILE),
    @Storage(file = StoragePathMacros.PROJECT_CONFIG_DIR + "/erlangCompileContexts.xml", scheme = StorageScheme.DIRECTORY_BASED)
  }
)
public class ErlangCompileContextManager extends AbstractProjectComponent implements PersistentStateComponent<ErlangCompileContextManager.State> {

  public static class State implements Serializable {
    @Tag("activeContext")
    public String myActiveContextName;

    @Tag("contexts")
    @AbstractCollection(surroundWithTag = false, elementTypes = ErlangCompileContext.class)
    public List<ErlangCompileContext> myProjectCompileContexts = createDefaultContexts();

    public State() {
      ErlangCompileContext defaultContext = ContainerUtil.getFirstItem(myProjectCompileContexts);
      myActiveContextName = defaultContext != null ? defaultContext.name : null;
    }

    public State(Project project) {
      this();
      setProject(project);
    }

    public void setProject(Project project) {
      for (ErlangCompileContext context : myProjectCompileContexts) {
        context.project = project;
      }
    }
  }

  private State myState = new State(myProject);

  ErlangCompileContextManager(@NotNull Project project) {
    super(project);
  }

  @NotNull
  public static ErlangCompileContextManager getInstance(@NotNull Project project) {
    return ServiceManager.getService(project, ErlangCompileContextManager.class);
  }

  @NotNull
  public ErlangCompileContext getContext(@Nullable VirtualFile file) {
    //TODO build context for a file: merge compiler contexts from file, module and project. Also check if it's under tests source root.
    return getProjectCompileContext();
  }

  @NotNull
  public String getActiveContextName() {
    return myState.myActiveContextName;
  }

  public void setActiveContext(@NotNull String contextName) {
    ErlangCompileContext oldActiveContext = null;
    ErlangCompileContext newCompileContext = null;
    for (ErlangCompileContext context : myState.myProjectCompileContexts) {
      if (Comparing.equal(context.name, contextName)) {
        newCompileContext = context;
      }
      else if (Comparing.equal(context.name, getActiveContextName())){
        oldActiveContext = context;
      }
    }
    if (newCompileContext != null && oldActiveContext != newCompileContext) {
      myState.myActiveContextName = newCompileContext.name;
      projectCompileContextChanged(oldActiveContext, newCompileContext);
    }
  }

  @NotNull
  public List<String> getAvailableContextNames() {
    return ContainerUtil.map(myState.myProjectCompileContexts, new Function<ErlangCompileContext, String>() {
      @Override
      public String fun(ErlangCompileContext erlangCompileContext) {
        return erlangCompileContext.name;
      }
    });
  }

  List<ErlangCompileContext> getProjectCompileContexts() {
    return myState.myProjectCompileContexts;
  }

  void setProjectCompileContexts(List<ErlangCompileContext> newContexts) {
    if (newContexts.isEmpty()) {
      newContexts.add(createDefaultContext());
    }

    ErlangCompileContext activeContext = getProjectCompileContext();
    ErlangCompileContext updatedActiveContext = null;
    for (ErlangCompileContext context : newContexts) {
      if (Comparing.equal(context.name, activeContext.name)) {
        updatedActiveContext = context;
        break;
      }
    }

    myState.myProjectCompileContexts = newContexts;
    if (!Comparing.equal(activeContext, updatedActiveContext)) {
      if (updatedActiveContext == null) {
        updatedActiveContext = getDefaultCompileContext();
      }
      myState.myActiveContextName = updatedActiveContext.name;
      projectCompileContextChanged(activeContext, updatedActiveContext);
    }
  }

  @NotNull
  Project getProject() {
    return myProject;
  }

  @Override
  public void projectOpened() {
    StatusBar statusBar = WindowManager.getInstance().getStatusBar(myProject);
    if (statusBar != null) {
      statusBar.addWidget(new ErlangCompileContextWidget(myProject));
    }
  }

  @Nullable
  @Override
  public State getState() {
    return XmlSerializerUtil.createCopy(myState);
  }

  @Override
  public void loadState(State state) {
    myState = XmlSerializerUtil.createCopy(state);
    myState.setProject(myProject);
  }


  @NotNull
  private ErlangCompileContext getProjectCompileContext() {
    ErlangCompileContext context = null;
    for (ErlangCompileContext c : myState.myProjectCompileContexts) {
      if (Comparing.equal(c.name, getActiveContextName())) {
        context = c;
        break;
      }
    }
    if (context == null) {
      context = getDefaultCompileContext();
    }
    return context.clone();
  }

  @NotNull
  private ErlangCompileContext getDefaultCompileContext() {
    ErlangCompileContext defaultCompileContext = ContainerUtil.getFirstItem(myState.myProjectCompileContexts);
    ErlangCompileContext context = defaultCompileContext != null ? defaultCompileContext : createDefaultContext();
    context.project = myProject;
    return context;
  }

  private void projectCompileContextChanged(@Nullable ErlangCompileContext from, @NotNull ErlangCompileContext to) {
    //TODO do something smarter here
    StatusBar statusBar = WindowManager.getInstance().getStatusBar(myProject);
    if (statusBar != null) {
      statusBar.updateWidget(ErlangCompileContextWidget.ID);
    }
    List<VirtualFile> allModules = ErlangModuleIndex.getVirtualFiles(myProject, GlobalSearchScope.projectScope(myProject));
    FileContentUtilCore.reparseFiles(allModules);
  }


  private static List<ErlangCompileContext> createDefaultContexts() {
    return ContainerUtil.newArrayList(createDefaultContext(), createDefaultTestContext());
  }

  private static ErlangCompileContext createDefaultContext() {
    return new ErlangCompileContext("Default");
  }

  private static ErlangCompileContext createDefaultTestContext() {
    return new ErlangCompileContext(null, "Test", Collections.singletonMap("TEST", "true"), ContainerUtil.<String>emptyList());
  }
}
