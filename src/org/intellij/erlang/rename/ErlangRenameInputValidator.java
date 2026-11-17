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

package org.intellij.erlang.rename;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.patterns.ElementPattern;
import com.intellij.patterns.ElementPatternCondition;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.rename.RenameInputValidator;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangRenameInputValidator implements RenameInputValidator {
  @Override
  public @NotNull ElementPattern<? extends PsiElement> getPattern() {
    return new ElementPattern<>() {
      @Override
      public boolean accepts(@Nullable Object o) {
        return false;
      }

      @Override
      public boolean accepts(@Nullable Object o, ProcessingContext context) {
        return o instanceof ErlangQVar || o instanceof ErlangQAtom ||
               o instanceof ErlangFunction || o instanceof ErlangRecordDefinition || o instanceof ErlangModule;
      }

      @Nullable
      @Override
      public ElementPatternCondition<PsiElement> getCondition() {
        return null;
      }
    };
  }

  @Override
  public boolean isInputValid(@NotNull String s, @NotNull PsiElement o, @NotNull ProcessingContext context) {
    try {
      if (o instanceof ErlangQVar) {
        ErlangElementFactory.createQVarFromText(o.getProject(), s);
        return true;
      }
      else if (o instanceof ErlangQAtom || o instanceof ErlangFunction || o instanceof ErlangRecordDefinition || o instanceof ErlangModule) {
        String atomName = ErlangPsiImplUtil.toAtomName(s);
        if (atomName != null) {
          ErlangElementFactory.createAtomFromText(o.getProject(), atomName);
          if (o instanceof ErlangModule) {
            String unquoted = StringUtil.unquoteString(atomName, '\'');
            return unquoted.equals(FileUtil.sanitizeFileName(unquoted));
          }
          return true;
        }
      }
    } catch (Exception ignored) {
    }
    return false;
  }
}
