/*
 * Copyright 2012-2016 Sergey Ignatov
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

package org.intellij.erlang.psi.impl;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReferenceBase;
import com.intellij.psi.impl.source.resolve.ResolveCache;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.jetbrains.annotations.Nullable;

public abstract class ErlangPsiPolyVariantCachingReferenceBase<T extends PsiElement> extends PsiPolyVariantReferenceBase<T> {
  public ErlangPsiPolyVariantCachingReferenceBase(T element, TextRange range) {
    super(element, range);
  }

  private static final ResolveCache.AbstractResolver<ErlangPsiPolyVariantCachingReferenceBase<?>, PsiElement> MY_RESOLVER =
    (base, incompleteCode) -> base.resolveInner();

  @Nullable
  @Override
  public final PsiElement resolve() {
    if (!getElement().isValid()) return null;
    PsiElement result = ResolveCache.getInstance(getElement().getProject()).resolveWithCaching(this, MY_RESOLVER, false, false);
    // spike 4: when master's (cached) path finds nothing, ask the macro expansion layer. Its answer is cached in the
    // layer's copy, not in the ResolveCache, so a "no copy yet" answer on the UI thread is not kept.
    return result != null ? result : ErlangMacroLayer.resolveFallback(this);
  }

  @Nullable
  public abstract PsiElement resolveInner();

  @Override
  public boolean equals(Object o) {
    return this == o || o instanceof ErlangPsiPolyVariantCachingReferenceBase
                        && getElement() == ((ErlangPsiPolyVariantCachingReferenceBase<?>) o).getElement();
  }

  @Override
  public int hashCode() {
    return getElement().hashCode();
  }
}
