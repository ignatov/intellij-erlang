package org.intellij.erlang.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.psi.ErlangFile;

/**
 * @author ignatov
 */
public class ErlangElementFactory {
  private ErlangElementFactory() {
  }

  public static PsiElement createAtomFromText(Project project, String text) {
    ErlangFile fileFromText = (ErlangFile) PsiFileFactory.getInstance(project).createFileFromText("a.erl", ErlangLanguage.INSTANCE, "-" + text + ".");
    return fileFromText.getAttributes().get(0).getAtomAttribute().getQAtom();
  }
}
