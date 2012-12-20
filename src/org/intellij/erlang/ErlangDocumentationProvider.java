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

package org.intellij.erlang;

import com.intellij.codeInsight.documentation.PlatformDocumentationUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.lang.documentation.CompositeDocumentationProvider;
import com.intellij.lang.documentation.ExternalDocumentationProvider;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangSpecification;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

/**
 * @author ignatov
 */
public class ErlangDocumentationProvider extends AbstractDocumentationProvider implements ExternalDocumentationProvider {
  @Nullable
  @Override
  public List<String> getUrlFor(@NotNull PsiElement element, @Nullable PsiElement originalElement) {
    if (element instanceof ErlangModule) {
      return findUrlForModule((ErlangModule) element);
    }
    else if (element instanceof ErlangFunction) {
      return findUrlForFunction((ErlangFunction) element);
    }
    return null;
  }

  @Nullable
  private static List<String> findUrlForModule(@NotNull ErlangModule erlModule) {
    final VirtualFile virtualFile = getVirtualFile(erlModule);
    if (virtualFile == null) {
      return null;
    }
    return findUrlForVirtualFile(erlModule.getProject(), virtualFile, "");
  }

  @Nullable
  private static List<String> findUrlForFunction(@NotNull ErlangFunction erlFunction) {
    final VirtualFile virtualFile = getVirtualFile(erlFunction);
    if (virtualFile == null) {
      return null;
    }
    final String inDocRef = "#" + erlFunction.getName() + "-" + erlFunction.getArity();
    return findUrlForVirtualFile(erlFunction.getProject(), virtualFile, inDocRef);
  }

  @Nullable
  private static VirtualFile getVirtualFile(@NotNull PsiElement psiElement) {
    final PsiFile containingFile = psiElement.getContainingFile();
    return (containingFile == null ? null : containingFile.getVirtualFile());
  }

  @Nullable
  private static List<String> findUrlForVirtualFile(@NotNull Project project,
                                                    @NotNull VirtualFile virtualFile,
                                                    @NotNull String inDocRef) {
    final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(project).getFileIndex();
    final List<OrderEntry> orderEntries = fileIndex.getOrderEntriesForFile(virtualFile);
    for (OrderEntry orderEntry : orderEntries) {
      final String[] docRootUrls = JavadocOrderRootType.getUrls(orderEntry);
      final String sdkHttpDocRelPath = sdkHttpDocRelPath(virtualFile);
      final List<String> docUrls = PlatformDocumentationUtil.getHttpRoots(docRootUrls, sdkHttpDocRelPath + inDocRef);
      if (docUrls != null) {
        return docUrls;
      }
    }
    return null;
  }

  @Nullable
  private static String sdkHttpDocRelPath(@NotNull VirtualFile virtualFile) {
    final String appDirName = virtualFile.getParent().getParent().getName();
    final String prefix;
    if (appDirName.startsWith("erts")) {
      prefix = "";
    }
    else {
      prefix = "lib/";
    }
    return prefix + appDirName + "/doc/html/" + virtualFile.getNameWithoutExtension() + ".html";
  }

  @Nullable
  @Override
  public String generateDoc(@NotNull PsiElement element, @Nullable PsiElement originalElement) {
    if (element instanceof ErlangFunction) {
      return generateFunctionDoc((ErlangFunction) element);
    }
    else if (element instanceof ErlangModule) {
      return generateModuleDoc((ErlangModule) element);
    }
    return null;
  }

  @Nullable
  private static String generateFunctionDoc(@NotNull ErlangFunction erlangFunction) {
    ErlangFunction prevFunction = PsiTreeUtil.getPrevSiblingOfType(erlangFunction, ErlangFunction.class);
    PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(erlangFunction, PsiComment.class);
    ErlangAttribute attribute = PsiTreeUtil.getPrevSiblingOfType(erlangFunction, ErlangAttribute.class);
    PsiElement spec = attribute != null ? PsiTreeUtil.getChildOfType(attribute, ErlangSpecification.class) : null;
    String commentText = "";
    if (spec instanceof ErlangSpecification && notFromPreviousFunction(spec, prevFunction)) {
      commentText += spec.getText().replaceFirst("spec", "<b>Specification:</b><br/>") + "<br/><br/>";
    }
    if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT && notFromPreviousFunction(comment, prevFunction)) {
      commentText += "<b>Comment:</b><br/>" + getCommentText(comment, "%%", EDOC_FUNCTION_TAGS);
      return commentText;
    }
    return null;
  }

  @Nullable
  private static String generateModuleDoc(@NotNull ErlangModule erlangModule) {
    PsiElement parent = erlangModule.getParent();
    PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(parent, PsiComment.class);
    if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_MODULE_DOC_COMMENT) {
      return getCommentText(comment, "%%%", EDOC_MODULE_TAGS);
    }
    return null;
  }

  private static boolean notFromPreviousFunction(@NotNull PsiElement spec, @Nullable ErlangFunction prevFunction) {
    return (prevFunction == null || (spec.getTextOffset() > prevFunction.getTextOffset()));
  }

  private static String getCommentText(PsiComment comment, final String commentStartsWith, final Set<String> contextTags) {
    String[] lines = StringUtil.splitByLines(comment.getText());
    return StringUtil.join(ContainerUtil.map(lines, new Function<String, String>() {
      @Override
      public String fun(String s) {
        String replace = StringUtil.replace(s, commentStartsWith, "");
        for (String tag : contextTags) {
          replace = replace.replaceAll(tag, "<b>" + tag + "</b>");
        }
        return replace;
      }
    }), "<br/>");
  }

  /** <a href="www.erlang.org/doc/apps/edoc/chapter.html#id59379">Overview tags</a> */
  public static final Set<String> EDOC_OVERVIEW_TAGS = ContainerUtil.set(
    "@author", "@copyright", "@doc", "@reference", "@see", "@since", "@title", "@version"
  );

  /** <a href="www.erlang.org/doc/apps/edoc/chapter.html#id60723">Module tags</a> */
  public static final Set<String> EDOC_MODULE_TAGS = ContainerUtil.set(
    "@author", "@copyright", "@deprecated", "@doc", "@hidden", "@private", "@reference", "@see", "@since", "@version"
  );

  /** <a href="www.erlang.org/doc/apps/edoc/chapter.html#id56868">Function tags</a> */
  public static final Set<String> EDOC_FUNCTION_TAGS = ContainerUtil.set(
    "@deprecated", "@doc", "@equiv", "@hidden", "@private", "@see", "@since", "@spec", "@throws"
  );

  /** <a href="www.erlang.org/doc/apps/edoc/chapter.html#id64336">Generic tags</a> */
  private static final Set<String> EDOC_GENERIC_TAGS = ContainerUtil.set(
    "@clear", "@docfile", "@end", "@headerfile", "@todo", "@TODO", "@type"
  );

  static {
    EDOC_OVERVIEW_TAGS.addAll(EDOC_GENERIC_TAGS);
    EDOC_MODULE_TAGS.addAll(EDOC_GENERIC_TAGS);
    EDOC_FUNCTION_TAGS.addAll(EDOC_GENERIC_TAGS);
  }

  @Nullable
  @Override
  public String fetchExternalDocumentation(@NotNull Project project, @NotNull PsiElement element, @NotNull List<String> docUrls) {
    return null;  // TODO Implement
  }

  @Override
  public boolean hasDocumentationFor(@NotNull PsiElement element, @Nullable PsiElement originalElement) {
    return CompositeDocumentationProvider.hasUrlsFor(this, element, originalElement);
  }

  @Override
  public boolean canPromptToConfigureDocumentation(@NotNull PsiElement element) {
    return false;
  }

  @Override
  public void promptToConfigureDocumentation(@NotNull PsiElement element) {
  }
}
