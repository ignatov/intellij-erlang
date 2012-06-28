package org.intellij.erlang.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;

/**
 * @author ignatov
 */
@SuppressWarnings("ConstantConditions")
public class ErlangElementFactory {
  private ErlangElementFactory() {
  }

  @NotNull
  public static PsiElement createQAtomFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-" + text + ".");
    return fileFromText.getAttributes().get(0).getAtomAttribute().getQAtom();
  }

  @NotNull
  public static PsiElement createQVarFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "f(" + text + ") -> 0.");
    return fileFromText.getFunctions().get(0).getFunctionClauseList().get(0).getArgumentDefinitionList().get(0).getExpression().getQVar();
  }
}
