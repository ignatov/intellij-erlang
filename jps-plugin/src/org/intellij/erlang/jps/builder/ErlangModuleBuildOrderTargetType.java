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

package org.intellij.erlang.jps.builder;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.builders.BuildTargetLoader;
import org.jetbrains.jps.builders.BuildTargetType;
import org.jetbrains.jps.model.JpsModel;

import java.util.Collections;
import java.util.List;

public class ErlangModuleBuildOrderTargetType extends BuildTargetType<ErlangModuleBuildOrderTarget> {
  public static final ErlangModuleBuildOrderTargetType INSTANCE = new ErlangModuleBuildOrderTargetType();
  public static final String TYPE_ID = "erlang_build_order";

  private ErlangModuleBuildOrderTargetType() {
    super(TYPE_ID);
  }

  @NotNull
  @Override
  public List<ErlangModuleBuildOrderTarget> computeAllTargets(@NotNull JpsModel model) {
    return Collections.singletonList(new ErlangModuleBuildOrderTarget(model.getProject(), this));
  }

  @NotNull
  @Override
  public BuildTargetLoader<ErlangModuleBuildOrderTarget> createLoader(@NotNull final JpsModel model) {
    return new BuildTargetLoader<>() {
      @Nullable
      @Override
      public ErlangModuleBuildOrderTarget createTarget(@NotNull String targetId) {
        if (targetId.equals(model.getProject().getName())) {
          return new ErlangModuleBuildOrderTarget(model.getProject(), ErlangModuleBuildOrderTargetType.this);
        }
        return null;
      }
    };
  }
}
