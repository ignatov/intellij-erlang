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

package org.intellij.erlang.search;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.search.TextOccurenceProcessor;
import com.intellij.psi.search.UsageSearchContext;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import com.intellij.util.QueryExecutor;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangAtomSearch implements QueryExecutor<PsiReference, ReferencesSearch.SearchParameters> {
  @Override
  public boolean execute(@NotNull ReferencesSearch.SearchParameters parameters, @NotNull Processor<PsiReference> consumer) {
    PsiElement element = parameters.getElementToSearch();
    if (!(element instanceof ErlangQAtom)) return true;

    String name = ErlangPsiImplUtil.getName((ErlangQAtom) element);
    if (StringUtil.isEmpty(name)) return true;

    SearchScope searchScope = parameters.getEffectiveSearchScope();
    RubyCodeOccurenceProcessor processor = new RubyCodeOccurenceProcessor(element, consumer);
    short searchContext = UsageSearchContext.IN_CODE | UsageSearchContext.IN_STRINGS;
    return PsiSearchHelper.SERVICE.getInstance(element.getProject()).
      processElementsWithWord(processor, searchScope, name, searchContext, true);
  }

  public static class RubyCodeOccurenceProcessor implements TextOccurenceProcessor {
    private PsiElement myElement;
    private Processor<PsiReference> myPsiReferenceProcessor;

    public RubyCodeOccurenceProcessor(@NotNull PsiElement element, @NotNull Processor<PsiReference> psiReferenceProcessor) {
      myElement = element;
      myPsiReferenceProcessor = psiReferenceProcessor;
    }

    public boolean execute(PsiElement element, int offsetInElement) {
      PsiReference ref = element.getReference();
      if (ref!=null && ref.isReferenceTo(myElement)) {
        return myPsiReferenceProcessor.process(ref);
      }
      return true;
    }
  }
}
