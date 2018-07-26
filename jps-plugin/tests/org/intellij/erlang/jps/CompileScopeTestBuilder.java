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
package org.intellij.erlang.jps;

import org.jetbrains.jps.builders.BuildTarget;
import org.jetbrains.jps.builders.BuildTargetType;
import org.jetbrains.jps.incremental.CompileScope;
import org.jetbrains.jps.incremental.CompileScopeImpl;
import org.jetbrains.jps.incremental.TargetTypeRegistry;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class CompileScopeTestBuilder {
  private final boolean myForceBuild;
  private final Set<BuildTargetType<?>> myTargetTypes = new HashSet<>();
  private final Set<BuildTarget<?>> myTargets = new HashSet<>();

  public static CompileScopeTestBuilder rebuild() {
    return new CompileScopeTestBuilder(true);
  }

  public static CompileScopeTestBuilder make() {
    return new CompileScopeTestBuilder(false);
  }

  private CompileScopeTestBuilder(boolean forceBuild) {
    myForceBuild = forceBuild;
  }

  public CompileScope build() {
    Collection<BuildTargetType<?>> typesToForceBuild = myForceBuild ? myTargetTypes : Collections.emptyList();
    return new CompileScopeImpl(myTargetTypes, typesToForceBuild, myTargets, Collections.emptyMap());
  }

  public CompileScopeTestBuilder all() {
    myTargetTypes.addAll(TargetTypeRegistry.getInstance().getTargetTypes());
    return this;
  }
}
