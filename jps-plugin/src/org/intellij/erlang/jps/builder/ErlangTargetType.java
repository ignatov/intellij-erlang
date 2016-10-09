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

package org.intellij.erlang.jps.builder;

import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildTargetLoader;
import org.jetbrains.jps.builders.ModuleBasedBuildTargetType;
import org.jetbrains.jps.model.JpsDummyElement;
import org.jetbrains.jps.model.JpsModel;
import org.jetbrains.jps.model.module.JpsTypedModule;

import java.util.ArrayList;
import java.util.List;

public class ErlangTargetType extends ModuleBasedBuildTargetType<ErlangTarget> {
  public static final ErlangTargetType INSTANCE = new ErlangTargetType();
  private static final String TYPE_ID = "erlang modules";

  private ErlangTargetType() {
    super(TYPE_ID);
  }

  @NotNull
  @Override
  public List<ErlangTarget> computeAllTargets(@NotNull JpsModel model) {
    List<ErlangTarget> targets = new ArrayList<>();
    for (JpsTypedModule<JpsDummyElement> module : model.getProject().getModules(JpsErlangModuleType.INSTANCE)) {
      targets.add(new ErlangTarget(module, this));
    }
    return targets;
  }

  @NotNull
  @Override
  public BuildTargetLoader<ErlangTarget> createLoader(@NotNull final JpsModel model) {
    return new BuildTargetLoader<ErlangTarget>() {
      @Nullable
      @Override
      public ErlangTarget createTarget(@NotNull String targetId) {
        for (JpsTypedModule<JpsDummyElement> module : model.getProject().getModules(JpsErlangModuleType.INSTANCE)) {
          if (module.getName().equals(targetId)) {
            return new ErlangTarget(module, ErlangTargetType.this);
          }
        }
        return null;
      }
    };
  }
}
