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

package org.intellij.erlang;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.spellchecker.inspections.IdentifierSplitter;
import com.intellij.spellchecker.tokenizer.SpellcheckingStrategy;
import com.intellij.spellchecker.tokenizer.TokenConsumer;
import com.intellij.spellchecker.tokenizer.Tokenizer;
import org.intellij.erlang.psi.ErlangAtom;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangSpellcheckingStrategy extends SpellcheckingStrategy {
  @Override
  public boolean isMyContext(@NotNull PsiElement element) {
    return ErlangLanguage.INSTANCE.is(element.getLanguage());
  }

  @NotNull
  @Override
  public Tokenizer<?> getTokenizer(PsiElement element) {
    if (element instanceof ErlangAtom) {
      PsiElement parent = element.getParent();
      if (parent instanceof ErlangQAtom && ErlangPsiImplUtil.standaloneAtom((ErlangQAtom) parent)) {
        return AtomTokenizer.INSTANCE;
      }
    }
    return super.getTokenizer(element);
  }

  private static class AtomTokenizer extends Tokenizer<ErlangAtom> {
    private static final AtomTokenizer INSTANCE = new AtomTokenizer();

    @Override
    public void tokenize(@NotNull ErlangAtom a, @NotNull TokenConsumer consumer) {
      PsiElement identifier = a.getNameIdentifier();
      TextRange range = identifier.getTextRange();
      if (range.isEmpty()) return;

      int offset = range.getStartOffset() - a.getTextRange().getStartOffset();
      if (offset < 0) {
        offset = range.getStartOffset() - a.getTextRange().getStartOffset();
      }
      String text = identifier.getText();
      consumer.consumeToken(a, text, true, offset, TextRange.allOf(text), IdentifierSplitter.getInstance());
    }
  }
}
