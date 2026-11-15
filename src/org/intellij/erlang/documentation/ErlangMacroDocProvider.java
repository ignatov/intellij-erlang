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

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

final class ErlangMacroDocProvider implements ElementDocProvider {
  @NotNull
  private final ErlangMacrosDefinition myMacrosDefinition;

  public ErlangMacroDocProvider(@NotNull ErlangMacrosDefinition macrosDefinition) {
    myMacrosDefinition = macrosDefinition;
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @NotNull
  @Override
  public String getDocText() {
    String text = StringUtil.escapeXmlEntities(myMacrosDefinition.getText());
    List<PsiComment> comments = collectLeadingComments(myMacrosDefinition);
    if (!comments.isEmpty()) {
      List<String> lines = ContainerUtil.map(comments, c -> StringUtil.escapeXmlEntities(c.getText().replaceFirst("^%+", "")));
      text += "<br/><br/><b>Comment:</b><br/>" + StringUtil.join(lines, "<br/>");
    }
    PsiFile file = myMacrosDefinition.getContainingFile();
    if (file != null) {
      text += "<br/><br/><b>Defined in:</b> " + StringUtil.escapeXmlEntities(file.getName());
    }
    return ErlangDocUtil.wrapInPreTag(text);
  }

  @NotNull
  private static List<PsiComment> collectLeadingComments(@NotNull PsiElement element) {
    List<PsiComment> result = new ArrayList<>();
    PsiElement prev = element.getPrevSibling();
    while (prev instanceof PsiWhiteSpace || prev instanceof PsiComment && startsLine(prev)) {
      if (prev instanceof PsiComment comment) {
        result.add(comment);
      }
      prev = prev.getPrevSibling();
    }
    return ContainerUtil.reverse(result);
  }

  private static boolean startsLine(@NotNull PsiElement comment) {
    PsiElement before = comment.getPrevSibling();
    return before == null || before instanceof PsiComment || before instanceof PsiWhiteSpace && before.textContains('\n');
  }
}
