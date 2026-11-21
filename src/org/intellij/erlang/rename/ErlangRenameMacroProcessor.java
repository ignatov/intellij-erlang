package org.intellij.erlang.rename;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.jetbrains.annotations.NotNull;

import java.util.Map;

/**
 * Round 6 (next), item 4: a macro defined in the branches of a conditional ({@code -ifdef(TEST)} / {@code -else}) is
 * one macro: renaming one of its definitions renames the definitions of the same name and the same number of
 * parameters that the file sees (the other branches), and their uses. A definition of another arity is another macro
 * (epp chooses by the number of arguments) and keeps its name, as a function of another arity does.
 */
public class ErlangRenameMacroProcessor extends RenamePsiElementProcessor {
  @Override
  public boolean canProcessElement(@NotNull PsiElement element) {
    return element instanceof ErlangMacrosDefinition;
  }

  @Override
  public void prepareRenaming(@NotNull PsiElement element, @NotNull String newName, @NotNull Map<PsiElement, String> allRenames) {
    if (!(element instanceof ErlangMacrosDefinition d) || d.getName() == null) return;
    PsiFile file = d.getContainingFile();
    if (!(file instanceof ErlangFile ef)) return;
    int arity = ErlangMacroResolve.arity(d);
    for (ErlangMacrosDefinition other : ErlangMacroResolve.candidates(ef, d.getName())) {
      if (other != d && ErlangMacroResolve.arity(other) == arity) allRenames.put(other, newName);
    }
  }
}
