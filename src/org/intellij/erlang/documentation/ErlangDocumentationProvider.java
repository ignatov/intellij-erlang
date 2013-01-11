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

package org.intellij.erlang.documentation;

import com.intellij.codeInsight.documentation.PlatformDocumentationUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.lang.documentation.CompositeDocumentationProvider;
import com.intellij.lang.documentation.ExternalDocumentationProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangModuleImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
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
    else {
      final PsiElement parent = element.getParent();
      if (parent instanceof ErlangFunctionCallExpression) {
        return findUrlForBif((ErlangFunctionCallExpression) parent);
      }
    }
    return null;
  }

  @Nullable
  private static List<String> findUrlForBif(@NotNull ErlangFunctionCallExpression bifCall) {
    final String bifName = bifCall.getNameIdentifier().getText();
    final int bifArity = bifCall.getArgumentList().getExpressionList().size();
    final PsiElement callExpressionParent = bifCall.getParent();
    if (callExpressionParent instanceof ErlangGlobalFunctionCallExpression) {
      final ErlangModuleRef moduleRef = ((ErlangGlobalFunctionCallExpression) callExpressionParent).getModuleRef();
      final PsiReference psiReference = moduleRef != null ? moduleRef.getReference() : null;
      final PsiElement psiElement = psiReference != null ? psiReference.resolve() : null;
      if (psiElement instanceof ErlangModuleImpl) {
        final ErlangModuleImpl bifPsiModule = (ErlangModuleImpl) psiElement;
        final String bifModuleName = bifPsiModule.getName();
        if (ErlangBifTable.isBif(bifModuleName, bifName, bifArity)) {
          final String inDocRef = "#" + bifName + "-" + bifArity;
          return findUrlForVirtualFile(bifPsiModule.getProject(), getVirtualFile(bifPsiModule), inDocRef);
        }
      }
    }
    return null;
  }

  @Nullable
  private static List<String> findUrlForModule(@NotNull ErlangModule erlModule) {
    final VirtualFile virtualFile = getVirtualFile(erlModule);
    return findUrlForVirtualFile(erlModule.getProject(), virtualFile, "");
  }

  @Nullable
  private static List<String> findUrlForFunction(@NotNull ErlangFunction erlFunction) {
    final VirtualFile virtualFile = getVirtualFile(erlFunction);
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
                                                    @Nullable VirtualFile virtualFile,
                                                    @NotNull String inDocRef) {
    if (virtualFile == null) {
      return null;
    }
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
    ErlangSpecification spec = ErlangPsiImplUtil.getSpecification(erlangFunction);
    PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(erlangFunction, PsiComment.class);
    String commentText = "";
    if (spec != null && ErlangPsiImplUtil.notFromPreviousFunction(spec, prevFunction)) {
      commentText += spec.getText().replaceFirst("spec", "<b>Specification:</b><br/>") + "<br/><br/>";
    }
    if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT && ErlangPsiImplUtil.notFromPreviousFunction(comment, prevFunction)) {
      commentText += "<b>Comment:</b><br/>" + getCommentText(comment, "%%", EDOC_FUNCTION_TAGS);
    }
    return commentText;
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
