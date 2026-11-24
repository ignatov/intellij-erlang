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

import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangSpecification;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

final class ErlangFunctionDocProvider implements ElementDocProvider {
  private static final Set<String> FUNCTION_ANNOTATIONS = Set.of("doc", "compile", "dialyzer");

  @NotNull
  private final ErlangFunction myErlangFunction;

  public ErlangFunctionDocProvider(@NotNull ErlangFunction erlangFunction) {
    myErlangFunction = erlangFunction;
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @NotNull
  @Override
  public String getDocText() {
    ErlangFunction prevFunction = PsiTreeUtil.getPrevSiblingOfType(myErlangFunction, ErlangFunction.class);
    ErlangSpecification spec = myErlangFunction.findSpecification();
    PsiElement prev = PsiTreeUtil.skipWhitespacesBackward(myErlangFunction);
    while (prev instanceof ErlangAttribute attribute && isFunctionAnnotation(attribute)) {
      prev = PsiTreeUtil.skipWhitespacesBackward(prev);
    }
    PsiComment comment = ObjectUtils.tryCast(prev, PsiComment.class);

    String commentText = "";
    if (spec != null && ErlangPsiImplUtil.notFromPreviousFunction(spec, prevFunction)) {
      commentText += spec.getText().replaceFirst("spec", "<b>Specification:</b><br/>") + "<br/><br/>";
    }
    if (comment != null && comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT &&
        ErlangPsiImplUtil.notFromPreviousFunction(comment, prevFunction)) {
      commentText += "<b>Comment:</b><br/>" + ErlangDocUtil.getCommentText(
        ErlangDocUtil.collectPrevComments(comment), "%%", ErlangDocUtil.EDOC_FUNCTION_TAGS);
    }
    return ErlangDocUtil.wrapInPreTag(commentText);
  }

  private static boolean isFunctionAnnotation(@NotNull ErlangAttribute attribute) {
    if (attribute.getSpecification() != null) return true;
    ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
    return atomAttribute != null && FUNCTION_ANNOTATIONS.contains(atomAttribute.getName());
  }
}
