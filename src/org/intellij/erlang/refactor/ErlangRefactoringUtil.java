package org.intellij.erlang.refactor;

import com.intellij.codeInsight.PsiEquivalenceUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author ignatov
 */
public class ErlangRefactoringUtil {
  @NotNull
  public static List<PsiElement> getOccurrences(@NotNull final PsiElement pattern, @Nullable final PsiElement context) {
    if (context == null) {
      return Collections.emptyList();
    }
    final List<PsiElement> occurrences = new ArrayList<PsiElement>();
    final PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      public void visitElement(@NotNull final PsiElement element) {
        if (PsiEquivalenceUtil.areElementsEquivalent(element, pattern)) {
          occurrences.add(element);
          return;
        }
        super.visitElement(element);
      }
    };
    context.accept(visitor);
    return occurrences;
  }
}
