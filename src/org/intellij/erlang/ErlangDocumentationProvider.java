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

import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangSpecification;

import java.util.Set;

/**
 * @author ignatov
 */
public class ErlangDocumentationProvider extends AbstractDocumentationProvider {
  @Override
  public String generateDoc(PsiElement element, PsiElement originalElement) {
    if (element instanceof ErlangFunction) {
      ErlangFunction prevFunction = PsiTreeUtil.getPrevSiblingOfType(element, ErlangFunction.class);
      PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(element, PsiComment.class);
      ErlangAttribute attribute = PsiTreeUtil.getPrevSiblingOfType(element, ErlangAttribute.class);
      PsiElement spec = attribute != null ? PsiTreeUtil.getChildOfType(attribute, ErlangSpecification.class) : null;
      String commentText = "";
      if (spec instanceof ErlangSpecification && notFromPreviousFunction(spec, prevFunction)) {
        commentText += spec.getText().replaceFirst("spec", "<b>Specification:</b><br/>") + "<br/><br/>";
      }
      if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT && notFromPreviousFunction(comment, prevFunction)) {
        commentText += "<b>Comment:</b><br/>" + getCommentText(comment, "%%", EDOC_FUNCTION_TAGS);
        return commentText;
      }
    }
    else if (element instanceof ErlangModule) {
      PsiElement parent = element.getParent();
      PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(parent, PsiComment.class);
      if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_MODULE_DOC_COMMENT) {
        return getCommentText(comment, "%%%", EDOC_MODULE_TAGS);
      }
    }
    return null;
  }

  private static boolean notFromPreviousFunction(PsiElement spec, ErlangFunction prevFunction) {
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
}
