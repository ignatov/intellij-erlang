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

package org.intellij.erlang.context;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.Stack;
import org.intellij.erlang.index.ErlangApplicationIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.rebar.util.RebarConfigUtil;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.jetbrains.annotations.Nullable;

import java.util.Comparator;
import java.util.List;

public final class ErlangPathResolver {
  private ErlangPathResolver() {
  }

  //TODO use compile context for resolution

  @Nullable
  public static VirtualFile resolveInclude(@Nullable Project project, @Nullable Stack<VirtualFile> owners, @Nullable String includeString) {
    return resolveInclude(project, owners, includeString, false);
  }

  @Nullable
  public static VirtualFile resolveIncludeLib(@Nullable Project project, @Nullable Stack<VirtualFile> owners, @Nullable String includeString) {
    return resolveInclude(project, owners, includeString, true);
  }

  @Nullable
  public static VirtualFile resolveInclude(@Nullable Project project, @Nullable Stack<VirtualFile> owners, @Nullable String includeString, boolean isIncludeLib) {
    if (includeString == null || owners == null) return null;

    //TODO make sure this is the correct way to resolve inclusions
    for (VirtualFile owner : owners) {
      VirtualFile includeFile = isIncludeLib ? resolveIncludeLib(project, owner, includeString) : resolveInclude(project, owner, includeString);
      if (includeFile != null) {
        return includeFile;
      }
    }
    return null;
  }

  @Nullable
  public static VirtualFile resolveInclude(@Nullable Project project, @Nullable VirtualFile owner, @Nullable String includeString) {
    if (owner == null || includeString == null || !owner.isInLocalFileSystem()) return null;

    //try to find a file relatively to owner's direct parent
    VirtualFile parent = owner.getParent();
    VirtualFile relativeToDirectParent = VfsUtilCore.findRelativeFile(includeString, parent);
    if (relativeToDirectParent != null) return relativeToDirectParent;

    //let's search in owner module's include source roots
    Module module = project != null ? ModuleUtilCore.findModuleForFile(owner, project) : null;
    if (module != null) {
      for (VirtualFile includeDirectory : ErlangIncludeDirectoryUtil.getIncludeDirectories(module)) {
        VirtualFile includedFile = VfsUtilCore.findRelativeFile(includeString, includeDirectory);
        if (includedFile != null) return includedFile;
      }
    }

    //let's search in compile context
    List<String> includePaths = project == null ? ContainerUtil.<String>emptyList() :
      ErlangCompileContextManager.getInstance(project).getContext(owner).includePaths;
    for (String includePath : includePaths) {
      VirtualFile includeDirectory = LocalFileSystem.getInstance().findFileByPath(includePath);
      VirtualFile includedFile = VfsUtilCore.findRelativeFile(includeString, includeDirectory);
      if (includedFile != null) return includedFile;
    }

    //TODO consider providing source roots functionality to small IDEs
    //TODO use -pa and -pz from context to locate appRoots
    if (ErlangSystemUtil.isSmallIde()) {
      VirtualFile appRoot = getContainingOtpAppRoot(project, parent);
      if (appRoot == null) return null;

      // try OTP 'include' directory of a containing OTP application
      VirtualFile otpIncludeDirectory = appRoot.findChild("include");
      VirtualFile includedFile = VfsUtilCore.findRelativeFile(includeString, otpIncludeDirectory);
      if (includedFile != null) return includedFile;

      //let's try include paths listed in rebar.config
      ErlangFile rebarConfigPsi = RebarConfigUtil.getRebarConfig(project, appRoot);
      if (rebarConfigPsi != null) {
        for(String includePath : ContainerUtil.reverse(RebarConfigUtil.getIncludePaths(rebarConfigPsi))) {
          VirtualFile includeDirectory = VfsUtilCore.findRelativeFile(includePath, appRoot);
          includedFile = VfsUtilCore.findRelativeFile(includeString, includeDirectory);
          if (includedFile != null) return includedFile;
        }
      }
    }

    return null;
  }

  @Nullable
  public static VirtualFile resolveIncludeLib(@Nullable Project project, @Nullable VirtualFile owner, @Nullable String includeString) {
    if (owner == null || includeString == null || project == null) return null;

    String[] split = includeString.split("/");
    if (split.length >= 2) {
      String libName = split[0];
      String relativePath = StringUtil.join(split, 1, split.length, "/");

      VirtualFile appDir = ErlangApplicationIndex.getApplicationDirectoryByName(libName, GlobalSearchScope.allScope(project));
      VirtualFile includedFile = VfsUtilCore.findRelativeFile(relativePath, appDir);
      if (includedFile != null) {
        return includedFile;
      }
    }
    //either include_lib does not specify a library, or it was not found, falling back to 'include' behaviour.
    return resolveInclude(project, owner, includeString);
  }

  @Nullable
  public static VirtualFile getContainingOtpAppRoot(@Nullable Project project, @Nullable final VirtualFile file) {
    if (file == null || project == null) return null;
    List<VirtualFile> allOtpAppRoots = ErlangApplicationIndex.getAllApplicationDirectories(project, GlobalSearchScope.allScope(project));
    List<VirtualFile> containingOtpAppRoots = ContainerUtil.filter(allOtpAppRoots, new Condition<VirtualFile>() {
      @Override
      public boolean value(VirtualFile appRoot) {
        return VfsUtilCore.isAncestor(appRoot, file, true);
      }
    });
    //sort it in order to have longest path first
    ContainerUtil.sort(containingOtpAppRoots, new Comparator<VirtualFile>() {
      @Override
      public int compare(VirtualFile o1, VirtualFile o2) {
        return o2.getPath().length() - o1.getPath().length();
      }
    });
    return ContainerUtil.getFirstItem(containingOtpAppRoots);
  }
}
