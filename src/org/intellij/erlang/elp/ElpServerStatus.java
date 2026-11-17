/*
 * Copyright 2012-2026 Sergey Ignatov
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


package org.intellij.erlang.elp;

import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;

/**
 * Whether an ELP server is running and serves a file. Implemented in the optional part of the plugin that is loaded with the
 * platform LSP module ({@code erlang-elp-lsp.xml}), so that the code that asks does not depend on LSP classes. Without the LSP
 * module the project has no such service, and nothing is served by ELP.
 */
public interface ElpServerStatus {
  /**
   * @return {@code true} when an ELP server started for the {@link ElpMode#ELP} position is running in the project, would get
   * {@code file}, and announced the capability that {@code feature} needs
   */
  boolean isServing(@NotNull VirtualFile file, @NotNull ElpEngine.Feature feature);

  /**
   * @return {@code true} when an ELP server started for the {@link ElpMode#ELP} position is running in the project and announced
   * the capability that {@code feature} needs
   */
  boolean isServingProject(@NotNull ElpEngine.Feature feature);
}
