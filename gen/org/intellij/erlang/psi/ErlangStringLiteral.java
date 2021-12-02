// This is a generated file. Not intended for manual editing.
package org.intellij.erlang.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiLanguageInjectionHost;
import org.intellij.erlang.ErlangStringLiteralEscaper;

public interface ErlangStringLiteral extends ErlangExpression, PsiLanguageInjectionHost {

  @NotNull
  PsiElement getString();

  boolean isValidHost();

  @NotNull ErlangStringLiteral updateText(@NotNull String text);

  @NotNull ErlangStringLiteralEscaper createLiteralTextEscaper();

}
