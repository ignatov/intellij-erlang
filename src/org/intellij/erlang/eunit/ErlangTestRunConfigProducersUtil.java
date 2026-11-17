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

package org.intellij.erlang.eunit;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.search.FilenameIndex;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class ErlangTestRunConfigProducersUtil {
  private ErlangTestRunConfigProducersUtil() {
  }

  public static boolean shouldProduceEunitTestRunConfiguration(@Nullable Project project, @Nullable Module module) {
    return !shouldProduceRebarTestRunConfiguration(project, module);
  }

  //TODO think of a more elegant way to learn if rebar is used in a module.
  public static boolean shouldProduceRebarTestRunConfiguration(@Nullable Project project, @Nullable Module module) {
    if (project == null || module == null) return false;
    if (StringUtil.isEmpty(RebarSettings.getInstance(project).getRebarPath())) return false;
    Collection<VirtualFile> configs = FilenameIndex.getVirtualFilesByName("rebar.config", module.getModuleContentScope());
    return !configs.isEmpty();
  }
}
