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

import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

final class ErlangModuleDocProvider implements ElementDocProvider {
  @NotNull private final ErlangModule myErlangModule;

  public ErlangModuleDocProvider(@NotNull ErlangModule erlangModule) {
    myErlangModule = erlangModule;
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @Nullable
  @Override
  public String getDocText() {
    PsiElement parent = myErlangModule.getParent();
    PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(parent, PsiComment.class);
    if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_MODULE_DOC_COMMENT) {
      return ErlangDocUtil.getCommentsText(ErlangDocUtil.collectPrevComments(comment), "%%%", ErlangDocUtil.EDOC_MODULE_TAGS);
    }
    return null;
  }
}
