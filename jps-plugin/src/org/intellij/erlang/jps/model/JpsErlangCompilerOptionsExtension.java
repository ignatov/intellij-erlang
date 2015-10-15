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

package org.intellij.erlang.jps.model;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.jps.model.JpsElementChildRole;
import org.jetbrains.jps.model.ex.JpsCompositeElementBase;
import org.jetbrains.jps.model.ex.JpsElementChildRoleBase;

public class JpsErlangCompilerOptionsExtension extends JpsCompositeElementBase<JpsErlangCompilerOptionsExtension> {
  public static final JpsElementChildRole<JpsErlangCompilerOptionsExtension> ROLE = JpsElementChildRoleBase.create("ErlangCompilerOptions");

  private ErlangCompilerOptions myOptions;

  public JpsErlangCompilerOptionsExtension(@NotNull ErlangCompilerOptions options) {
    myOptions = options;
  }

  @NotNull
  @Override
  public JpsErlangCompilerOptionsExtension createCopy() {
    return new JpsErlangCompilerOptionsExtension(new ErlangCompilerOptions(myOptions));
  }

  @NotNull
  public ErlangCompilerOptions getOptions() {
    return myOptions;
  }
}
