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
package org.intellij.erlang.template;

import com.intellij.codeInsight.template.TemplateActionContext;
import com.intellij.codeInsight.template.TemplateContextType;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.util.PsiUtilCore;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.ErlangClauseBody;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public abstract class ErlangContextType extends TemplateContextType {
  protected ErlangContextType(@NotNull String presentableName) {
    super(presentableName);
  }

  @Override
  public boolean isInContext(@NotNull TemplateActionContext templateActionContext) {
    PsiFile file = templateActionContext.getFile();
    int offset = templateActionContext.getStartOffset();
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
      super("Erlang");
    }

    @Override
    protected boolean isInContext(PsiElement element) {
      return true;
    }
  }

  protected static class Declaration extends ErlangContextType {
    protected Declaration() {
      super("Declaration");
    }

    @Override
    protected boolean isInContext(PsiElement element) {
      PsiFile file = element.getContainingFile();
      if (file != null && ErlangParserUtil.isConsole(file)) return false;
      int offset = element.getTextOffset();
      if (offset > 0 && file != null) {
        PsiElement prev = file.findElementAt(offset - 1);
        if (ErlangPsiImplUtil.is(prev, ErlangTypes.ERL_DOT)) return false;
      }
      PsiElement parent = element.getParent();
      return parent instanceof ErlangFile || parent instanceof PsiErrorElement && parent.getParent() instanceof ErlangFile;
    }
  }

  protected static class Statement extends ErlangContextType {
    protected Statement() {
      super("Statement");
    }

    @Override
    protected boolean isInContext(PsiElement element) {
      return PsiTreeUtil.getParentOfType(element, ErlangClauseBody.class, ErlangFunction.class) != null;
    }
  }

  protected static class Expression extends ErlangContextType {
    protected Expression() {
      super("Expression");
    }

    @Override
    protected boolean isInContext(PsiElement element) {
      return PsiTreeUtil.getParentOfType(element, ErlangExpression.class, ErlangFunction.class) != null;
    }
  }
}
