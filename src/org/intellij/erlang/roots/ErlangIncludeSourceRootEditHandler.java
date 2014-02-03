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

package org.intellij.erlang.roots;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.actionSystem.CustomShortcutSet;
import com.intellij.openapi.roots.ui.configuration.ModuleSourceRootEditHandler;
import com.intellij.ui.JBColor;
import org.intellij.erlang.jps.model.ErlangIncludeSourceRootType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.model.JpsDummyElement;

import javax.swing.*;
import java.awt.*;

public class ErlangIncludeSourceRootEditHandler extends ModuleSourceRootEditHandler<JpsDummyElement> {
  public ErlangIncludeSourceRootEditHandler() {
    super(ErlangIncludeSourceRootType.INSTANCE);
  }

  @NotNull
  @Override
  public String getRootTypeName() {
    return "Includes";
  }

  @NotNull
  @Override
  public Icon getRootIcon() {
    //TODO set icon
    return AllIcons.Modules.Sources;
  }

  @Nullable
  @Override
  public Icon getFolderUnderRootIcon() {
    return null;
  }

  @Nullable
  @Override
  public CustomShortcutSet getMarkRootShortcutSet() {
    return null;
  }

  @NotNull
  @Override
  public String getRootsGroupTitle() {
    return "Include directories";
  }

  @NotNull
  @Override
  public Color getRootsGroupColor() {
    return new JBColor(new Color(140, 123, 79), new Color(140, 123, 79));
  }

  @NotNull
  @Override
  public String getUnmarkRootButtonText() {
    return "Unmark include directory";
  }
}
