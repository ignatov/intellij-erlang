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

package org.intellij.erlang;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testIntegration.TestFinder;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.Collections;

public class ErlangTestFinder implements TestFinder {
  public static final String EXT = "." + ErlangFileType.MODULE.getDefaultExtension();
  public static String[] SUFFIXES = new String[]{"_test", "_tests"};

  @Override
  public PsiFile findSourceElement(@NotNull PsiElement from) {
    return from.getContainingFile();
  }

  @NotNull
  @Override
  public Collection<PsiElement> findTestsForClass(@NotNull PsiElement element) {
    VirtualFile virtualFile = getVirtualFile(element);
    if (virtualFile == null) return ContainerUtil.emptyList();
    Collection<PsiElement> result = ContainerUtil.newTroveSet();
    Project project = element.getProject();
    for (String suffix : SUFFIXES) {
      Collections.addAll(result,
        FilenameIndex.getFilesByName(project, virtualFile.getNameWithoutExtension() + suffix + EXT, getScope(project)));
    }
    return result;
  }

  @NotNull
  @Override
  public Collection<PsiElement> findClassesForTest(@NotNull PsiElement element) {
    VirtualFile virtualFile = getVirtualFile(element);
    if (virtualFile == null) return ContainerUtil.emptyList();
    Collection<PsiElement> result = ContainerUtil.newTroveSet();
    Project project = element.getProject();
    String name = virtualFile.getNameWithoutExtension();
    int length = name.length();
    for (String suffix : SUFFIXES) {
      if (name.endsWith(suffix)) {
        Collections.addAll(result,
          FilenameIndex.getFilesByName(project, name.substring(0, length - suffix.length()) + EXT, getScope(project)));
      }
    }
    return result;
  }

  private static GlobalSearchScope getScope(Project project) {
    return GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.allScope(project), ErlangFileType.MODULE);
  }

  @Nullable
  private VirtualFile getVirtualFile(PsiElement element) {
    PsiFile file = findSourceElement(element);
    if (file == null || !(file instanceof ErlangFile)) return null;
    return file.getVirtualFile();
  }

  @Override
  public boolean isTest(@NotNull PsiElement element) {
    PsiFile containingFile = findSourceElement(element);
    if (!(containingFile instanceof ErlangFile)) return false;
    return ErlangPsiImplUtil.isEunitTestFile((ErlangFile) containingFile);
  }
}
