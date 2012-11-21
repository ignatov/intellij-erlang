/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.rebar.config;

import com.intellij.openapi.fileTypes.ExactFileNameMatcher;
import com.intellij.openapi.fileTypes.FileNameMatcher;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangIcons;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public final class RebarFileType extends ErlangFileType {
  public static final RebarFileType INSTANCE = new RebarFileType();

  private RebarFileType() {
  }

  @NotNull
  public static FileNameMatcher getFileNameMatcher() {
    return new ExactFileNameMatcher("rebar.config");
  }

  @NotNull
  @NonNls
  public String getName() {
    return "Rebar";
  }

  @NotNull
  @NonNls
  public String getDescription() {
    return "Rebar configuration file";
  }

  @NotNull
  @NonNls
  public String getDefaultExtension() {
    return "config";
  }

  @Nullable
  public Icon getIcon() {
    return ErlangIcons.TERMS; // TODO It would be nice to have a dedicated icon.
  }

  public String getCharset(@NotNull final VirtualFile file, final byte[] content) {
    return CharsetToolkit.UTF8;
  }

}
