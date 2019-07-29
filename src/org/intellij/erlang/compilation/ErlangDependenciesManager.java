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

package org.intellij.erlang.compilation;

import com.intellij.openapi.compiler.CompilerManager;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupManager;

public class ErlangDependenciesManager implements ProjectComponent {
  protected final Project myProject;

  public ErlangDependenciesManager(Project project) {
    myProject = project;
  }

  @Override
  public void initComponent() {
    StartupManager.getInstance(myProject).registerPostStartupActivity(() -> CompilerManager.getInstance(myProject).addBeforeTask(new ErlangPrepareDependenciesCompileTask()));
  }
}