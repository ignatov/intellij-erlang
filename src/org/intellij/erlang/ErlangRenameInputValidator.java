package org.intellij.erlang;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.patterns.ElementPattern;
import com.intellij.patterns.ElementPatternCondition;
import com.intellij.psi.PsiElement;
import com.intellij.refactoring.rename.RenameInputValidator;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.Nullable;

/**
 * @author ignatov
 */
public class ErlangRenameInputValidator implements RenameInputValidator {
  @Override
  public ElementPattern<? extends PsiElement> getPattern() {
    return new ElementPattern<PsiElement>() {
      @Override
      public boolean accepts(@Nullable Object o) {
        return false;
      }

      @Override
      public boolean accepts(@Nullable Object o, ProcessingContext context) {
        return o instanceof ErlangQVar || o instanceof ErlangQAtom ||
          o instanceof ErlangFunction || o instanceof ErlangRecordDefinition || o instanceof ErlangModule;
      }

      @Override
      public ElementPatternCondition<PsiElement> getCondition() {
        return null;
      }
    };
  }

  @Override
  public boolean isInputValid(String s, PsiElement o, ProcessingContext context) {
    try {
      if (o instanceof ErlangQVar) {
        ErlangElementFactory.createQVarFromText(o.getProject(), s);
        return true;
      }
      else if (o instanceof ErlangQAtom || o instanceof ErlangFunction || o instanceof ErlangRecordDefinition) {
        ErlangElementFactory.createQAtomFromText(o.getProject(), s);
        return true;
      }
      else if (o instanceof ErlangModule) {
        ErlangElementFactory.createQAtomFromText(o.getProject(), s);
        return s != null && s.equals(FileUtil.sanitizeFileName(s));
      }
    } catch (Exception ignored) {
    }
    return false;
  }
}
