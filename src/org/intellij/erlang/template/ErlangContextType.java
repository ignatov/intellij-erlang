/*
 * Copyright 2012 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
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
package org.intellij.erlang.template;

import com.intellij.codeInsight.template.EverywhereContextType;
import com.intellij.codeInsight.template.TemplateContextType;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class ErlangContextType extends TemplateContextType {
  protected ErlangContextType(@NotNull @NonNls String id, @NotNull String presentableName, @Nullable Class<? extends TemplateContextType> baseContextType) {
    super(id, presentableName, baseContextType);
  }

  @Override
  public boolean isInContext(@NotNull PsiFile file, int offset) {
    if (!PsiUtilCore.getLanguageAtOffset(file, offset).isKindOf(ErlangLanguage.INSTANCE)) return false;
    PsiElement element = file.findElementAt(offset);
    if (element instanceof PsiWhiteSpace) {
      return false;
    }
    return element != null && isInContext(element);
  }

  protected abstract boolean isInContext(PsiElement element);

  public static class Generic extends ErlangContextType {

    protected Generic() {
      super("ERLANG_CODE", "Erlang", EverywhereContextType.class);
    }

    @Override
    protected boolean isInContext(PsiElement element) {
      return true;
    }
  }

  public static class Declaration extends ErlangContextType {
    protected Declaration() {
      super("ERLANG_DECLARATION", "Declaration", Generic.class);
    }

    @Override
    protected boolean isInContext(PsiElement element) {
      PsiElement parent = element.getParent();
      return parent instanceof ErlangFile || (parent instanceof PsiErrorElement && parent.getParent() instanceof ErlangFile);
    }
  }

  public static class Statement extends ErlangContextType {
    protected Statement() {
      super("ERLANG_STATEMENT", "Statement", Generic.class);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected boolean isInContext(PsiElement element) {
      return PsiTreeUtil.getParentOfType(element, ErlangClauseBody.class, ErlangFunction.class) != null;
    }
  }

  public static class Expression extends ErlangContextType {
    protected Expression() {
      super("ERLANG_EXPRESSION", "Expression", Generic.class);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected boolean isInContext(PsiElement element) {
      return PsiTreeUtil.getParentOfType(element, ErlangExpression.class, ErlangFunction.class) != null;
    }
  }
}
