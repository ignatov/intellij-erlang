/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.patterns.PatternCondition;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.PlatformPatterns.virtualFile;
import static com.intellij.patterns.StandardPatterns.instanceOf;

public class ErlangAppCompletionContributor extends CompletionContributor {
  // source: "FILE SYNTAX" section of http://www.erlang.org/doc/man/app.html
  static final List<String> KEYWORDS =
    ContainerUtil.immutableList("description", "id", "vsn", "modules", "maxP", "maxT", "registered", "applications",
                                "included_applications", "env", "mod", "start_phases", "runtime_dependencies");
  private static final String APPLICATION_KEYWORD = "application";

  @Override
  public void beforeCompletion(@NotNull CompletionInitializationContext context) {
    PsiFile file = context.getFile();
    if (file.getFileType() != ErlangFileType.APP) return;
    int startOffset = context.getStartOffset();
    PsiElement elementAt = file.findElementAt(startOffset);
    PsiElement parent = elementAt != null ? elementAt.getParent() : null;
    if (parent instanceof ErlangTupleExpression) {
      context.setDummyIdentifier(APPLICATION_KEYWORD);
    }
  }

  public ErlangAppCompletionContributor() {
    PsiElementPattern.Capture<PsiElement> psiElementInAppFile =
      psiElement(ErlangTypes.ERL_ATOM_NAME).inFile(instanceOf(ErlangFile.class))
                                           .inVirtualFile(virtualFile().ofType(ErlangFileType.APP));
    //noinspection unchecked
    PsiElementPattern.Capture<PsiElement> placeForAppKeyword = psiElementInAppFile
      .withParents(ErlangAtom.class, ErlangQAtom.class, ErlangConfigExpression.class,
                   ErlangTupleExpression.class, ErlangFile.class).with(positionInTuple(0));
    //noinspection unchecked
    PsiElementPattern.Capture<PsiElement> placeForParameterKeywords = psiElementInAppFile
      .withParents(ErlangAtom.class, ErlangQAtom.class, ErlangConfigExpression.class, ErlangTupleExpression.class,
                   ErlangListExpression.class, ErlangTupleExpression.class, ErlangFile.class).with(positionInTuple(0));

    extend(CompletionType.BASIC, placeForAppKeyword, getProvider(ContainerUtil.list(APPLICATION_KEYWORD)));
    extend(CompletionType.BASIC, placeForParameterKeywords, getProvider(KEYWORDS));
  }

  @NotNull
  private static CompletionProvider<CompletionParameters> getProvider(@NotNull final List<String> keywords) {
    return new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context,
                                    @NotNull CompletionResultSet result) {
        for (String keyword : keywords) {
          result.addElement(PrioritizedLookupElement.withPriority(LookupElementBuilder.create(keyword),
                                                                  ErlangCompletionContributor.KEYWORD_PRIORITY));
        }
      }
    };
  }

  @NotNull
  private static PatternCondition<PsiElement> positionInTuple(final int position) {
    return new PatternCondition<PsiElement>("positionInTuple") {
      @Override
      public boolean accepts(@NotNull PsiElement element, ProcessingContext context) {
        ErlangTupleExpression tuple = PsiTreeUtil.getParentOfType(element, ErlangTupleExpression.class);
        ErlangExpression expression = PsiTreeUtil.getParentOfType(element, ErlangExpression.class);
        List<ErlangExpression> expressions = tuple != null ? tuple.getExpressionList()
                                                           : ContainerUtil.emptyList();
        return expressions.size() > position && expressions.get(position) == expression;
      }
    };
  }
}
