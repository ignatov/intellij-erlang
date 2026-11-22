package org.intellij.erlang.rename;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import com.intellij.util.containers.MultiMap;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

/**
 * Renaming a macro definition also renames the definitions that are the same macro, all in the same file:
 * <ul>
 * <li>the alternatives of the same name and number of parameters in the branches of the same conditional
 * ({@code -ifdef(TEST)} / {@code -else}); a definition that is not in force together with it (a header's, one after an
 * {@code -undef}) is another macro and keeps its name;</li>
 * <li>when a directive ({@code -undef}, {@code -ifdef}, {@code -ifndef}) names the macro and the file defines it with
 * another number of parameters, every definition of the name in the file: a directive acts on every arity, so renaming
 * one arity alone would leave the directive half right.</li>
 * </ul>
 */
public class ErlangRenameMacroProcessor extends RenamePsiElementProcessor {
  @Override
  public boolean canProcessElement(@NotNull PsiElement element) {
    return element instanceof ErlangMacrosDefinition;
  }

  /** A name that no macro can have is refused before anything changes (a use the tree keeps bare could not take it). */
  @Override
  public void findExistingNameConflicts(@NotNull PsiElement element, @NotNull String newName, @NotNull MultiMap<PsiElement, String> conflicts) {
    if (ErlangMacroResolve.nameElement(element.getProject(), newName) == null) conflicts.putValue(element, "'" + newName + "' cannot be a macro name");
  }

  @Override
  public void prepareRenaming(@NotNull PsiElement element, @NotNull String newName, @NotNull Map<PsiElement, String> allRenames) {
    if (!(element instanceof ErlangMacrosDefinition d) || d.getName() == null) return;
    if (!(d.getContainingFile() instanceof ErlangFile file) || d.getParent() != file) return;
    String name = d.getName();
    int arity = ErlangMacroResolve.arity(d);
    Map<ErlangMacrosDefinition, Integer> groups = new HashMap<>();
    boolean directive = false, otherArity = false;
    int depth = 0, group = -1;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof ErlangMacrosDefinition m) {
        if (!name.equals(m.getName())) continue;
        groups.put(m, depth == 0 ? -1 : group);
        if (ErlangMacroResolve.arity(m) != arity) otherArity = true;
        continue;
      }
      if (!(c instanceof ErlangAttribute attribute)) continue;
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      String a = attribute.getIfAtomAttribute() != null ? "if" : atom != null ? atom.getName() : null;
      if (a == null) continue;
      switch (a) {
        case "if", "ifdef", "ifndef" -> {
          if (depth++ == 0) group = c.getTextRange().getStartOffset();
        }
        case "endif" -> depth = Math.max(0, depth - 1);
        default -> {
        }
      }
      if (atom != null && ("undef".equals(a) || "ifdef".equals(a) || "ifndef".equals(a))) {
        ErlangMacrosName m = PsiTreeUtil.getChildOfType(atom, ErlangMacrosName.class);
        if (m != null && name.equals(StringUtil.unquoteString(m.getText(), '\''))) directive = true;
      }
    }
    Integer own = groups.get(d);
    for (Map.Entry<ErlangMacrosDefinition, Integer> e : groups.entrySet()) {
      ErlangMacrosDefinition other = e.getKey();
      if (other == d) continue;
      boolean alternative = ErlangMacroResolve.arity(other) == arity && own != null && own != -1 && own.equals(e.getValue());
      if (alternative || directive && otherArity) allRenames.put(other, newName);
    }
  }
}
