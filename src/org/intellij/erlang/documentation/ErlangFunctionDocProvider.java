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
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangSpecification;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

final class ErlangFunctionDocProvider implements ElementDocProvider {
  @NotNull private final ErlangFunction myErlangFunction;

  public ErlangFunctionDocProvider(@NotNull ErlangFunction erlangFunction) {
    myErlangFunction = erlangFunction;
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @Nullable
  @Override
  public String getDocText() {
    final ErlangFunction prevFunction = PsiTreeUtil.getPrevSiblingOfType(myErlangFunction, ErlangFunction.class);
    final ErlangSpecification spec = ErlangPsiImplUtil.getSpecification(myErlangFunction);
    final PsiComment comment = PsiTreeUtil.getPrevSiblingOfType(myErlangFunction, PsiComment.class);

    String commentText = "";
    if (spec != null && ErlangPsiImplUtil.notFromPreviousFunction(spec, prevFunction)) {
      commentText += spec.getText().replaceFirst("spec", "<b>Specification:</b><br/>") + "<br/><br/>";
    }
    if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT &&
      ErlangPsiImplUtil.notFromPreviousFunction(comment, prevFunction)) {
      commentText += "<b>Comment:</b><br/>" + ErlangDocUtil.getCommentsText(
        ErlangDocUtil.collectPrevComments(comment), "%%", ErlangDocUtil.EDOC_FUNCTION_TAGS);
    }
    return commentText;
  }
}
