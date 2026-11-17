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

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectUtil;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.concurrency.annotations.RequiresReadLock;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Which files ELP gets and which directory it treats as the workspace root.
 */
public final class ElpFiles {
  private ElpFiles() {
  }

  /**
   * Only Erlang modules and headers, matched by extension. The plugin also gives the Erlang file types to
   * {@code rebar.config}, {@code *.app.src}, {@code *.config} and other term files; ELP must not get those.
   */
  public static boolean isElpFile(@NotNull VirtualFile file) {
    return !file.isDirectory() && isElpFileName(file.getName());
  }

  public static boolean isElpFileName(@NotNull String fileName) {
    String extension = FileUtilRt.getExtension(fileName);
    return "erl".equals(extension) || "hrl".equals(extension);
  }

  /**
   * The project's base directory when it holds the file, otherwise the content root that holds the file.
   */
  @Nullable
  @RequiresReadLock
  public static VirtualFile findWorkspaceRoot(@NotNull Project project, @NotNull VirtualFile file) {
    VirtualFile baseDir = ProjectUtil.guessProjectDir(project);
    if (baseDir != null && VfsUtilCore.isAncestor(baseDir, file, false)) return baseDir;
    return ProjectFileIndex.getInstance(project).getContentRootForFile(file);
  }

  /**
   * The workspace roots for which the project may start an ELP server, see {@link #findWorkspaceRoot}: the project's base directory
   * and the content roots outside it.
   */
  @NotNull
  @RequiresReadLock
  public static List<VirtualFile> workspaceRoots(@NotNull Project project) {
    List<VirtualFile> roots = new ArrayList<>();
    VirtualFile baseDir = ProjectUtil.guessProjectDir(project);
    if (baseDir != null) roots.add(baseDir);
    for (VirtualFile contentRoot : ProjectRootManager.getInstance(project).getContentRoots()) {
      if (baseDir == null || !VfsUtilCore.isAncestor(baseDir, contentRoot, false)) roots.add(contentRoot);
    }
    return roots;
  }

  /**
   * Whether the ELP server of the workspace root {@code root} gets {@code file}: an Erlang module or header under the root, by the
   * path the project knows or by the real path of the root, or one outside the project's content, such as the OTP sources, which ELP
   * loads too. A file under another content root is not this server's: ELP does not load it with this root.
   * <p>
   * The one predicate for both sides: the server's supported files ({@code ElpLspClientDescriptor.isSupportedFile}) and the files
   * where the plugin is silent ({@link ElpEngine#ownsFeature}), so that every file has exactly one engine.
   *
   * @param realRoot the real path of {@code root} when it lies under a symlink, see {@link ElpServedRoots#getRealPath}
   */
  @RequiresReadLock
  public static boolean serverGets(@NotNull Project project, @NotNull VirtualFile root, @Nullable String realRoot, @NotNull VirtualFile file) {
    if (!isElpFile(file)) return false;
    if (VfsUtilCore.isAncestor(root, file, false)) return true;
    if (realRoot != null && ElpServedRoots.isUnder(file.getPath(), realRoot)) return true;
    return !project.isDisposed() && !ProjectFileIndex.getInstance(project).isInContent(file);
  }
}
