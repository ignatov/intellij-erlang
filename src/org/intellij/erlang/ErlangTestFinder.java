/*
 * Copyright 2012-2013 Sergey Ignatov
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
import gnu.trove.THashSet;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Collections;

/**
 * @author ignatov
 */
public class ErlangTestFinder implements TestFinder {
  public static final String[] SUFFIXES = new String[]{"_test", "_tests"};

  @Override
  public PsiFile findSourceElement(@NotNull PsiElement from) {
    return from.getContainingFile();
  }

  @NotNull
  @Override
  public Collection<PsiElement> findTestsForClass(@NotNull PsiElement element) {
    final PsiFile file = findSourceElement(element);
    if (file == null) {
      return Collections.emptyList();
    }
    final Collection<PsiElement> result = new THashSet<PsiElement>();
    Project project = element.getProject();
    final GlobalSearchScope searchScope = GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.allScope(project), ErlangFileType.MODULE);
    VirtualFile virtualFile = file.getVirtualFile();
    if (virtualFile == null) return result;
    for (String suffix : SUFFIXES) {
      Collections.addAll(result, FilenameIndex.getFilesByName(project, virtualFile.getNameWithoutExtension() + suffix + "." + ErlangFileType.MODULE.getDefaultExtension(), searchScope));
    }
    return result;
  }

  @NotNull
  @Override
  public Collection<PsiElement> findClassesForTest(@NotNull PsiElement element) {
    final PsiFile file = findSourceElement(element);
    if (file == null) {
      return Collections.emptyList();
    }
    final Collection<PsiElement> result = new THashSet<PsiElement>();
    Project project = element.getProject();
    final GlobalSearchScope searchScope = GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.allScope(project), ErlangFileType.MODULE);
    VirtualFile virtualFile = file.getVirtualFile();
    if (virtualFile == null) return result;
    String name = virtualFile.getNameWithoutExtension();
    int length = name.length();
    for (String suffix : SUFFIXES) {
      if (name.endsWith(suffix)) {
        Collections.addAll(result, FilenameIndex.getFilesByName(project, name.substring(0, length - suffix.length()) + "." + ErlangFileType.MODULE.getDefaultExtension(), searchScope));
      }
    }
    return result;
  }

  @Override
  public boolean isTest(@NotNull PsiElement element) {
    PsiFile containingFile = findSourceElement(element);
    if (!(containingFile instanceof ErlangFile)) return false;
    return ErlangPsiImplUtil.isEunitTestFile((ErlangFile) containingFile);
  }
}
