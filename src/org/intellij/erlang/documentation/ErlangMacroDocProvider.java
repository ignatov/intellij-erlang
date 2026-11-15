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
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

final class ErlangMacroDocProvider implements ElementDocProvider {
  @NotNull
  private final ErlangMacrosDefinition myMacrosDefinition;
  @Nullable
  private final PsiElement myContext;

  public ErlangMacroDocProvider(@NotNull ErlangMacrosDefinition macrosDefinition, @Nullable PsiElement context) {
    myMacrosDefinition = macrosDefinition;
    myContext = context;
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @NotNull
  @Override
  public String getDocText() {
    StringBuilder result = new StringBuilder();
    for (ErlangMacrosDefinition definition : getDefinitionsForUse()) {
      result.append(ErlangDocUtil.wrapInPreTag(getDocText(definition)));
    }
    return result.toString();
  }

  @NotNull
  private static String getDocText(@NotNull ErlangMacrosDefinition definition) {
    String text = StringUtil.escapeXmlEntities(definition.getText());
    List<PsiComment> comments = collectLeadingComments(definition);
    if (!comments.isEmpty()) {
      List<String> lines = ContainerUtil.map(comments, c -> StringUtil.escapeXmlEntities(c.getText().replaceFirst("^%+", "")));
      text += "<br/><br/><b>Comment:</b><br/>" + StringUtil.join(lines, "<br/>");
    }
    PsiFile file = definition.getContainingFile();
    if (file != null) {
      text += "<br/><br/><b>Defined in:</b> " + StringUtil.escapeXmlEntities(file.getName());
    }
    return text;
  }

  /**
   * The resolved definition is the first one with the macro's name. When the macro is documented at a use,
   * pick the definitions that the preprocessor can expand there: the arity of the use, the definitions before it
   * and after the last {@code -undef} of the name. Definitions in {@code -ifdef}/{@code -else} branches cannot be
   * told apart, so all of them are shown.
   */
  @NotNull
  private List<ErlangMacrosDefinition> getDefinitionsForUse() {
    ErlangMacros use = myContext != null ? PsiTreeUtil.getParentOfType(myContext, ErlangMacros.class, false) : null;
    if (use == null || !(use.getContainingFile() instanceof ErlangFile useFile)) return List.of(myMacrosDefinition);
    String name = myMacrosDefinition.getName();

    Set<ErlangMacrosDefinition> candidates = new LinkedHashSet<>();
    if (myMacrosDefinition.getContainingFile() instanceof ErlangFile definitionFile && definitionFile != useFile) {
      candidates.addAll(ContainerUtil.filter(definitionFile.getMacroses(), d -> name.equals(d.getName())));
    }
    candidates.addAll(ContainerUtil.filter(useFile.getMacroses(), d -> name.equals(d.getName())));

    int useArity = getUseArity(use);
    List<ErlangMacrosDefinition> byArity = ContainerUtil.filter(candidates, d -> getArity(d) == useArity);
    if (byArity.isEmpty()) byArity = ContainerUtil.filter(candidates, d -> getArity(d) == -1);
    if (byArity.isEmpty()) return List.of(myMacrosDefinition);
    if (PsiTreeUtil.getParentOfType(use, ErlangMacrosDefinition.class) != null) return byArity;

    int useOffset = use.getTextRange().getStartOffset();
    int lastUndef = findLastUndefBefore(useFile, name, useOffset);
    List<ErlangMacrosDefinition> visible = ContainerUtil.filter(byArity, d -> {
      if (d.getContainingFile() != useFile) return lastUndef < 0;
      int offset = d.getTextRange().getStartOffset();
      return offset > lastUndef && offset < useOffset;
    });
    return visible.isEmpty() ? byArity : visible;
  }

  private static int getUseArity(@NotNull ErlangMacros use) {
    PsiElement container = use.getParent() instanceof ErlangQAtom qAtom ? qAtom : use;
    PsiElement next = PsiTreeUtil.skipWhitespacesAndCommentsForward(container);
    return next instanceof ErlangArgumentList argumentList ? argumentList.getExpressionList().size() : -1;
  }

  private static int getArity(@NotNull ErlangMacrosDefinition definition) {
    ErlangArgumentDefinitionList arguments = definition.getArgumentDefinitionList();
    return arguments != null ? arguments.getArgumentDefinitionList().size() : -1;
  }

  private static int findLastUndefBefore(@NotNull ErlangFile file, @NotNull String name, int offset) {
    int result = -1;
    for (PsiElement child = file.getFirstChild(); child != null; child = child.getNextSibling()) {
      int start = child.getTextRange().getStartOffset();
      if (start >= offset) break;
      ErlangAtomAttribute attribute = child instanceof ErlangAttribute a ? a.getAtomAttribute() : null;
      if (attribute == null || !"undef".equals(attribute.getName())) continue;
      ErlangMacrosName macrosName = PsiTreeUtil.getChildOfType(attribute, ErlangMacrosName.class);
      if (macrosName != null && name.equals(ErlangPsiImplUtil.getNameIdentifier(macrosName).getText())) {
        result = start;
      }
    }
    return result;
  }

  /**
   * The {@code %%} comments on the lines directly above the definition, without blank lines in between.
   */
  @NotNull
  private static List<PsiComment> collectLeadingComments(@NotNull PsiElement element) {
    List<PsiComment> result = new ArrayList<>();
    PsiElement current = element;
    while (current.getPrevSibling() instanceof PsiWhiteSpace whiteSpace &&
           StringUtil.countNewLines(whiteSpace.getText()) == 1 &&
           whiteSpace.getPrevSibling() instanceof PsiComment comment &&
           comment.getTokenType() == ErlangParserDefinition.ERL_FUNCTION_DOC_COMMENT &&
           startsLine(comment)) {
      result.add(comment);
      current = comment;
    }
    return ContainerUtil.reverse(result);
  }

  private static boolean startsLine(@NotNull PsiElement comment) {
    PsiElement before = comment.getPrevSibling();
    return before == null || before instanceof PsiWhiteSpace && before.textContains('\n');
  }
}
