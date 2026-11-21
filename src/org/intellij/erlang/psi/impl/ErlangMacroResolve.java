package org.intellij.erlang.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.PsiComment;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangArgumentDefinitionList;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Round 6: the {@code -define} a macro use names, chosen by arity as {@code epp} does: the definition with as many
 * parameters as the use has arguments, else the one without a parameter list (epp then leaves the parentheses as a
 * call), else the first one found. Definitions of the file come before those of its headers.
 */
public final class ErlangMacroResolve {
  /** The use has no argument list. */
  public static final int NO_ARGUMENTS = -1;
  /** The argument list is not closed. */
  public static final int UNKNOWN = -2;

  private static final TokenSet BLOCK_OPENERS = TokenSet.create(ErlangTypes.ERL_BEGIN, ErlangTypes.ERL_CASE, ErlangTypes.ERL_IF,
                                                                ErlangTypes.ERL_RECEIVE, ErlangTypes.ERL_TRY, ErlangTypes.ERL_MAYBE);

  private ErlangMacroResolve() {
  }

  public static @Nullable ErlangMacrosDefinition resolve(@NotNull ErlangFile file, @NotNull String name, int arity) {
    List<ErlangMacrosDefinition> candidates = candidates(file, name);
    if (candidates.isEmpty()) return null;
    if (arity != UNKNOWN) {
      for (ErlangMacrosDefinition d : candidates) if (arity(d) == arity) return d;
    }
    for (ErlangMacrosDefinition d : candidates) if (arity(d) == NO_ARGUMENTS) return d;
    return candidates.get(0);
  }

  /** The definitions of the name that the file sees: its own, then those of the headers it includes. */
  public static @NotNull List<ErlangMacrosDefinition> candidates(@NotNull ErlangFile file, @NotNull String name) {
    List<ErlangMacrosDefinition> r = new ArrayList<>();
    for (ErlangMacrosDefinition d : file.getMacroses()) if (name.equals(d.getName())) r.add(d);
    for (ErlangMacrosDefinition d : ErlangPsiImplUtil.getErlangMacrosFromIncludes(file, true, "")) if (name.equals(d.getName())) r.add(d);
    return r;
  }

  /** The file sees the definition, or sees no definition of its name at all. */
  public static boolean isVisibleOrUnknown(@NotNull ErlangFile file, @NotNull ErlangMacrosDefinition d) {
    String name = d.getName();
    if (name == null) return false;
    List<ErlangMacrosDefinition> c = candidates(file, name);
    return c.isEmpty() || c.contains(d);
  }

  /** Every include of the file, and of the headers it includes, resolves to a file (cached until a PSI change). */
  public static boolean allIncludesResolve(@NotNull ErlangFile file) {
    return com.intellij.psi.util.CachedValuesManager.getCachedValue(file, () -> com.intellij.psi.util.CachedValueProvider.Result.create(
      allIncludesResolve(file, new java.util.HashSet<>()), com.intellij.psi.util.PsiModificationTracker.MODIFICATION_COUNT));
  }

  private static boolean allIncludesResolve(@NotNull ErlangFile file, @NotNull java.util.Set<ErlangFile> seen) {
    if (!seen.add(file)) return true;
    for (org.intellij.erlang.psi.ErlangInclude inc : file.getIncludes()) {
      List<ErlangFile> r = ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file);
      if (r.isEmpty() || !allIncludesResolve(r.get(0), seen)) return false;
    }
    for (org.intellij.erlang.psi.ErlangIncludeLib inc : file.getIncludeLibs()) {
      List<ErlangFile> r = ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file);
      if (r.isEmpty() || !allIncludesResolve(r.get(0), seen)) return false;
    }
    return true;
  }

  /** Number of parameters, or {@link #NO_ARGUMENTS} without a parameter list. */
  public static int arity(@NotNull ErlangMacrosDefinition d) {
    ErlangArgumentDefinitionList list = d.getArgumentDefinitionList();
    return list == null ? NO_ARGUMENTS : list.getArgumentDefinitionList().size();
  }

  /**
   * The number of arguments after a macro name token, counted on the tokens (the tree there may be bare tokens):
   * commas at the top level of the parentheses, where brackets and {@code begin/case/fun/if/receive/try ... end}
   * nest, as in epp.
   */
  public static int arityAfter(@NotNull PsiElement name) {
    PsiElement next = nextToken(name);
    if (next != null && next.getNode().getElementType() == ErlangTypes.ERL_SINGLE_QUOTE && name.getNode().getElementType() == ErlangTypes.ERL_ATOM_NAME) {
      next = nextToken(next);
    }
    if (next == null || next.getNode().getElementType() != ErlangTypes.ERL_PAR_LEFT) return NO_ARGUMENTS;
    int depth = 0, commas = 0;
    boolean any = false;
    for (PsiElement t = nextToken(next); t != null; t = nextToken(t)) {
      var type = t.getNode().getElementType();
      if (type == ErlangTypes.ERL_DOT) return UNKNOWN;
      if (depth == 0 && type == ErlangTypes.ERL_PAR_RIGHT) return any ? commas + 1 : 0;
      any = true;
      if (type == ErlangTypes.ERL_PAR_LEFT || type == ErlangTypes.ERL_BRACKET_LEFT || type == ErlangTypes.ERL_CURLY_LEFT ||
          type == ErlangTypes.ERL_BIN_START || BLOCK_OPENERS.contains(type) ||
          type == ErlangTypes.ERL_FUN && isFunWithClauses(t)) {
        depth++;
      }
      else if (type == ErlangTypes.ERL_PAR_RIGHT || type == ErlangTypes.ERL_BRACKET_RIGHT || type == ErlangTypes.ERL_CURLY_RIGHT ||
               type == ErlangTypes.ERL_BIN_END || type == ErlangTypes.ERL_END) {
        depth--;
        if (depth < 0) return UNKNOWN;
      }
      else if (depth == 0 && type == ErlangTypes.ERL_COMMA) commas++;
    }
    return UNKNOWN;
  }

  private static boolean isFunWithClauses(@NotNull PsiElement fun) {
    PsiElement next = nextToken(fun);
    return next != null && next.getNode().getElementType() == ErlangTypes.ERL_PAR_LEFT;
  }

  private static @Nullable PsiElement nextToken(@NotNull PsiElement e) {
    PsiElement n = PsiTreeUtil.nextLeaf(e);
    while (n != null && (n instanceof PsiWhiteSpace || n instanceof PsiComment || n.getTextLength() == 0)) n = PsiTreeUtil.nextLeaf(n);
    return n;
  }

  /** A single name token (a variable or an unquoted atom) with the given text, or null when it would need quotes. */
  static @Nullable PsiElement nameToken(@NotNull Project project, @NotNull String name) {
    try {
      PsiElement e = ErlangElementFactory.createMacrosFromText(project, name);
      if (!(e instanceof ErlangMacrosName m)) return null;
      if (m.getVar() != null) return m.getVar();
      if (m.getAtom() != null && m.getAtom().getFirstChild() == m.getAtom().getLastChild()) return m.getAtom().getFirstChild();
    }
    catch (Exception ignore) {
    }
    return null;
  }
}
