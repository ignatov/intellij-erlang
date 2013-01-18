/*
 * Copyright 2013 Sergey Ignatov
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

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public final class ErlangDocUtil {

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
  static final Set<String> EDOC_GENERIC_TAGS = ContainerUtil.set(
    "@clear", "@docfile", "@end", "@headerfile", "@todo", "@TODO", "@type"
  );

  static {
    EDOC_OVERVIEW_TAGS.addAll(EDOC_GENERIC_TAGS);
    EDOC_MODULE_TAGS.addAll(EDOC_GENERIC_TAGS);
    EDOC_FUNCTION_TAGS.addAll(EDOC_GENERIC_TAGS);
  }

  private ErlangDocUtil() {
  }

  static String getCommentText(@NotNull PsiComment comment,
                               @NotNull final String commentStartsWith,
                               @NotNull final Set<String> contextTags) {
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
}
