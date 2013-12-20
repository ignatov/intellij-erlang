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

package org.intellij.erlang.documentation;

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangDocumentationProvider extends AbstractDocumentationProvider {
  private static final Pattern PATTERN_PSI_LINK = Pattern.compile("(.+?)(#(.*?)-(.*?))?");

  @Nullable
  @Override
  public List<String> getUrlFor(@NotNull PsiElement element, @Nullable PsiElement originalElement) {
    final ElementDocProvider elementDocProvider = ElementDocProviderFactory.create(element);
    if (elementDocProvider != null) {
      return elementDocProvider.getExternalDocUrls();
    }
    return null;
  }

  @Nullable
  @Override
  public String generateDoc(@NotNull PsiElement element, @Nullable PsiElement originalElement) {
    final ElementDocProvider elementDocProvider = ElementDocProviderFactory.create(element);
    if (elementDocProvider != null) {
      return elementDocProvider.getDocText();
    }
    return null;
  }

  @Nullable
  @Override
  public PsiElement getDocumentationElementForLink(@NotNull PsiManager psiManager,
                                                   @NotNull String link,
                                                   @Nullable PsiElement context) {
    final Project project = psiManager.getProject();
    final Matcher linkMatcher = PATTERN_PSI_LINK.matcher(link);
    if (linkMatcher.matches()) {
      final String moduleName = linkMatcher.group(1);
      final PsiFile[] psiFiles = FilenameIndex.getFilesByName(
        project, moduleName + ".erl", GlobalSearchScope.allScope(project));
      for (PsiFile psiFile : psiFiles) {
        if (psiFile instanceof ErlangFile) {
          final ErlangFile erlFile = (ErlangFile) psiFile;
          if (linkMatcher.group(2) == null) {
            return erlFile.getModule();
          }
          else {
            final String functionName = linkMatcher.group(3);
            if (functionName.equals("type")) {
              final String typeName = linkMatcher.group(4);
              return erlFile.getType(typeName);
            }
            else {
              final int arity = Integer.valueOf(linkMatcher.group(4));
              return erlFile.getFunction(functionName, arity);
            }
          }
        }
      }
    }
    return super.getDocumentationElementForLink(psiManager, link, context);
  }
}
