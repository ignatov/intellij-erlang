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

package org.intellij.erlang.documentation;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public final class ErlangDocUtil {

  /**
   * <a href="https://www.erlang.org/doc/apps/edoc/chapter.html#generic-tags">Generic tags</a>
   */
  private static final Set<String> EDOC_GENERIC_TAGS = ContainerUtil.newHashSet(
    "@clear", "@docfile", "@end", "@headerfile", "@todo", "@TODO", "@type"
  );

  /**
   * <a href="https://www.erlang.org/doc/apps/edoc/chapter.html#overview-tags">Overview tags</a>
   */
  private static final Set<String> EDOC_OVERVIEW_TAGS = ContainerUtil.newHashSet(
    "@author", "@copyright", "@doc", "@reference", "@see", "@since", "@title", "@version"
  );

  /**
   * <a href="https://www.erlang.org/doc/apps/edoc/chapter.html#module-tags">Module tags</a>
   */
  public static final Set<String> EDOC_MODULE_TAGS = ContainerUtil.newHashSet(
    "@author", "@copyright", "@deprecated", "@doc", "@hidden", "@private", "@reference", "@see", "@since", "@version"
  );

  /**
   * <a href="https://www.erlang.org/doc/apps/edoc/chapter.html#function-tags">Function tags</a>
   */
  public static final Set<String> EDOC_FUNCTION_TAGS = ContainerUtil.newHashSet(
    "@deprecated", "@doc", "@equiv", "@hidden", "@param", "@private", "@returns",
    "@see", "@since", "@spec", "@throws", "@type"
  );


  static {
    EDOC_OVERVIEW_TAGS.addAll(EDOC_GENERIC_TAGS);
    EDOC_MODULE_TAGS.addAll(EDOC_GENERIC_TAGS);

    EDOC_FUNCTION_TAGS.addAll(EDOC_GENERIC_TAGS);
    EDOC_FUNCTION_TAGS.addAll(EDOC_MODULE_TAGS);
  }

  private ErlangDocUtil() {
  }

  public static String getCommentText(@NotNull List<PsiComment> comments,
                                      @NotNull final String commentStartsWith,
                                      @NotNull final Set<String> contextTags) {
    List<String> lines = ContainerUtil.map(comments, PsiElement::getText);
    return StringUtil.join(ContainerUtil.map(lines, s -> {
      String replace = StringUtil.replace(s, commentStartsWith, "");
      for (String tag : contextTags) {
        replace = replace.replaceAll(tag, "<b>" + tag + "</b>");
      }
      return replace;
    }), "<br/>");
  }

  public static String wrapInPreTag(@NotNull String text) {
    return "<pre>" + text + "</pre>";
  }

  @NotNull
  static List<PsiComment> collectPrevComments(@NotNull PsiComment comment) {
    ArrayList<PsiComment> result = new ArrayList<>();
    PsiElement current = comment;
    while (current instanceof PsiComment) {
      result.add((PsiComment) current);
      ASTNode sibling = FormatterUtil.getPreviousNonWhitespaceSibling(current.getNode());
      if (sibling != null) {
        current = sibling.getPsi();
      }
      else {
        current = null;
      }
    }
    return ContainerUtil.reverse(result);
  }
}
