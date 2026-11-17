/*
 * Copyright 2012-2024 Sergey Ignatov
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

package org.intellij.erlang.sdk;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.platform.workspace.jps.entities.SdkEntity;
import com.intellij.platform.workspace.storage.EntityStorage;
import com.intellij.workspaceModel.core.fileIndex.WorkspaceFileIndexContributor;
import com.intellij.workspaceModel.core.fileIndex.WorkspaceFileSetRegistrar;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;

public class ErlangExcludeHtmlFileIndexContributor implements WorkspaceFileIndexContributor<SdkEntity> {
  private static final Logger LOG = Logger.getInstance(ErlangExcludeHtmlFileIndexContributor.class);

  @Override
  public @NotNull Class<SdkEntity> getEntityClass() {
    return SdkEntity.class;
  }

  @Override
  public void registerFileSets(SdkEntity entity,
                               @NotNull WorkspaceFileSetRegistrar registrar,
                               @NotNull EntityStorage storage) {
    if (!"Erlang SDK".equals(entity.getType())) {
      return;
    }
    // The function type is spelled out because 2026.2 adds an overload that takes a WorkspaceFileSetExclusionCondition,
    // which makes a bare lambda ambiguous there; this overload is the only one in 2026.1.
    Function1<VirtualFile, Boolean> exclusionCondition = virtualFile -> {
      var name = virtualFile.getName();
      if (name.endsWith("html")) {
        LOG.info("Excluded " + virtualFile.getUrl() + " from Erlang SDK" + entity.getName() + " to speedup indexing");
        return true;
      }
      return false;
    };
    registrar.registerExclusionCondition(entity.getHomePath().append("lib"), exclusionCondition, entity);
  }
}