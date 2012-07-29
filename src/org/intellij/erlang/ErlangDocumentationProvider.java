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
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;

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
      if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT && (prevFunction == null || (comment.getTextOffset() > prevFunction.getTextOffset()))) {
        return getCommentText(comment, "%%");
      }
    }
    else if (element instanceof ErlangModule) {
      PsiElement parent = element.getParent();
      PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(parent, PsiComment.class);
      if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_MODULE_DOC_COMMENT) {
        return getCommentText(comment, "%%%");
      }
    }
    return null;
  }

  private static String getCommentText(PsiComment comment, final String commentStartsWith) {
    String[] lines = StringUtil.splitByLines(comment.getText());
    return StringUtil.join(ContainerUtil.map(lines, new Function<String, String>() {
      @Override
      public String fun(String s) {
        String replace = StringUtil.replace(s, commentStartsWith, "");
        for (String tag : EDOC_TAGS) {
          replace = replace.replaceAll(tag, "<b>" + tag + "</b>");
        }
        return replace;
      }
    }), "<br/>");
  }

  public static final Set<String> EDOC_TAGS = ContainerUtil.set(
    "@author", "@copyright", "@since", "@see", "@reference", "@doc", "@since", "@title", "@version", "@end", "@spec", "@private"
  );
}
