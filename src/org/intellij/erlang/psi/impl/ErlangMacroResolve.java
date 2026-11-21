package org.intellij.erlang.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.PsiComment;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.ErlangArgumentDefinitionList;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

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
    List<ErlangMacrosDefinition> all = resolveAll(file, name, arity);
    if (!all.isEmpty()) return all.get(0);
    List<ErlangMacrosDefinition> candidates = candidates(file, name);
    return candidates.isEmpty() ? null : candidates.get(0);
  }

  /**
   * Round 6 (next), item 3b: the definitions that epp may choose for a use with this many arguments, in every
   * conditional branch: those with the same number of parameters, else those without a parameter list (epp then
   * leaves the parentheses as a call); with an unclosed argument list, every definition. Empty when none fits (epp:
   * argument mismatch). A use refers to each of them; a use of another arity refers to none of them.
   */
  public static @NotNull List<ErlangMacrosDefinition> matching(@NotNull ErlangFile file, @NotNull String name, int arity) {
    List<ErlangMacrosDefinition> candidates = candidates(file, name);
    if (candidates.isEmpty() || arity == UNKNOWN) return candidates;
    List<ErlangMacrosDefinition> same = new ArrayList<>();
    for (ErlangMacrosDefinition d : candidates) if (arity(d) == arity) same.add(d);
    if (!same.isEmpty()) return same;
    List<ErlangMacrosDefinition> none = new ArrayList<>();
    for (ErlangMacrosDefinition d : candidates) if (arity(d) == NO_ARGUMENTS) none.add(d);
    return none;
  }

  /**
   * Round 6 (next), item 4: the definitions a use resolves to: of {@link #matching}, the one in the active branch when
   * the file tells it (see {@link #branchState}), else all of them except those in a branch known to be inactive (go to
   * declaration then offers them all).
   */
  public static @NotNull List<ErlangMacrosDefinition> resolveAll(@NotNull ErlangFile file, @NotNull String name, int arity) {
    List<ErlangMacrosDefinition> m = matching(file, name, arity);
    if (m.size() <= 1) return m;
    List<ErlangMacrosDefinition> active = new ArrayList<>(), possible = new ArrayList<>();
    for (ErlangMacrosDefinition d : m) {
      int state = branchState(d);
      if (state == ACTIVE) active.add(d);
      if (state != INACTIVE) possible.add(d);
    }
    if (active.size() == 1) return active;
    return possible.isEmpty() ? m : possible;
  }

  public static final int ACTIVE = 1, INACTIVE = 2, UNKNOWN_BRANCH = 3;

  /**
   * Round 6 (next), item 4: whether the branch of a definition is active, as far as its own file tells: every
   * enclosing {@code -ifdef(X)}/{@code -ifndef(X)} (and its {@code -else}) is decided when X is predefined or defined
   * earlier in the file outside any branch; a macro that the file does not define may come from a build flag
   * ({@code -DTEST}), so its branch stays unknown, as does every {@code -if}/{@code -elif} and every header definition.
   * A definition outside any branch is active.
   */
  public static int branchState(@NotNull ErlangMacrosDefinition d) {
    PsiFile file = d.getContainingFile();
    if (!(file instanceof ErlangFile ef) || d.getParent() != file) return UNKNOWN_BRANCH;
    Integer s = com.intellij.psi.util.CachedValuesManager.getCachedValue(ef, () -> com.intellij.psi.util.CachedValueProvider.Result.create(
      computeBranchStates(ef), ef)).get(d);
    return s == null ? UNKNOWN_BRANCH : s;
  }

  private record Condition(@Nullable String macro, boolean ifdef, boolean decidable, boolean inElse) {
    Condition toElse() {
      return new Condition(macro, ifdef, decidable, true);
    }
  }

  private static @NotNull Map<ErlangMacrosDefinition, Integer> computeBranchStates(@NotNull ErlangFile file) {
    Map<ErlangMacrosDefinition, Integer> r = new HashMap<>();
    Set<String> defined = new HashSet<>();
    Deque<Condition> stack = new ArrayDeque<>();
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof ErlangMacrosDefinition d) {
        int state = ACTIVE;
        for (Condition k : stack) {
          if (!k.decidable() || k.macro() == null) {
            state = state == INACTIVE ? INACTIVE : UNKNOWN_BRANCH;
            continue;
          }
          boolean isDefined = defined.contains(k.macro()) || org.intellij.erlang.parser.macro.ErlangMacroExpander.PREDEFINED.contains(k.macro());
          if (!isDefined) {
            state = state == INACTIVE ? INACTIVE : UNKNOWN_BRANCH;
            continue;
          }
          boolean branchActive = k.ifdef() != k.inElse();
          if (!branchActive) state = INACTIVE;
        }
        r.put(d, state);
        if (stack.isEmpty() && d.getName() != null) defined.add(d.getName());
        continue;
      }
      if (!(c instanceof ErlangAttribute attribute)) continue;
      if (attribute.getElseAtomAttribute() != null) {
        Condition top = stack.poll();
        if (top != null) stack.push(top.toElse());
        continue;
      }
      if (attribute.getIfAtomAttribute() != null) {
        stack.push(new Condition(null, true, false, false));
        continue;
      }
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      if (atom == null) continue;
      ErlangMacrosName m = PsiTreeUtil.getChildOfType(atom, ErlangMacrosName.class);
      String macro = m == null ? null : com.intellij.openapi.util.text.StringUtil.unquoteString(m.getText(), '\'');
      switch (atom.getName()) {
        case "ifdef" -> stack.push(new Condition(macro, true, true, false));
        case "ifndef" -> stack.push(new Condition(macro, false, true, false));
        case "elif" -> {
          stack.poll();
          stack.push(new Condition(null, true, false, false));
        }
        case "endif" -> stack.poll();
        case "undef" -> {
          if (macro != null) defined.remove(macro);
        }
        default -> {
        }
      }
    }
    return r;
  }

  /** The definitions of the name that the file sees: its own, then those of the headers it includes. */
  public static @NotNull List<ErlangMacrosDefinition> candidates(@NotNull ErlangFile file, @NotNull String name) {
    List<ErlangMacrosDefinition> r = new ArrayList<>();
    for (ErlangMacrosDefinition d : file.getMacroses()) if (name.equals(d.getName())) r.add(d);
    for (ErlangMacrosDefinition d : ErlangPsiImplUtil.getErlangMacrosFromIncludes(file, true, "")) if (name.equals(d.getName())) r.add(d);
    return r;
  }

  /**
   * Round 6 (next), N2: the target of a macro reference for code that needs one definition (parameter info, record
   * fields of a macro body, the inspections): its single target, else the first of its possible definitions.
   */
  public static @Nullable PsiElement firstTarget(@Nullable com.intellij.psi.PsiReference ref) {
    if (ref == null) return null;
    PsiElement single = ref.resolve();
    if (single != null) return single;
    if (ref instanceof com.intellij.psi.PsiPolyVariantReference p) {
      for (com.intellij.psi.ResolveResult r : p.multiResolve(false)) if (r.getElement() != null) return r.getElement();
    }
    return null;
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

  /**
   * D4: epp's "redefining macro": a second definition of the same name and arity. Only where it is certain: both
   * definitions in this file, outside any conditional, with no {@code -undef} of the name (at any depth) and no
   * include between them (a header may undefine it).
   */
  public static boolean isRedefinition(@NotNull ErlangMacrosDefinition d) {
    if (!(d.getContainingFile() instanceof ErlangFile file)) return false;
    return com.intellij.psi.util.CachedValuesManager.getCachedValue(file, () -> com.intellij.psi.util.CachedValueProvider.Result.create(
      computeRedefinitions(file), file)).contains(d);
  }

  private static @NotNull Set<ErlangMacrosDefinition> computeRedefinitions(@NotNull ErlangFile file) {
    Set<ErlangMacrosDefinition> r = new HashSet<>();
    Map<String, Set<Integer>> seen = new HashMap<>();
    int depth = 0;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (c instanceof ErlangMacrosDefinition d) {
        String name = d.getName();
        if (name != null && depth == 0 && !seen.computeIfAbsent(name, k -> new HashSet<>()).add(arity(d))) r.add(d);
        continue;
      }
      if (c instanceof org.intellij.erlang.psi.ErlangInclude || c instanceof org.intellij.erlang.psi.ErlangIncludeLib ||
          PsiTreeUtil.getChildOfType(c, org.intellij.erlang.psi.ErlangInclude.class) != null ||
          PsiTreeUtil.getChildOfType(c, org.intellij.erlang.psi.ErlangIncludeLib.class) != null) {
        seen.clear();
        continue;
      }
      if (!(c instanceof ErlangAttribute attribute)) continue;
      if (attribute.getIfAtomAttribute() != null) {
        depth++;
        continue;
      }
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      if (atom == null) continue;
      switch (atom.getName()) {
        case "ifdef", "ifndef" -> depth++;
        case "endif" -> depth = Math.max(0, depth - 1);
        case "undef" -> {
          ErlangMacrosName m = PsiTreeUtil.getChildOfType(atom, ErlangMacrosName.class);
          if (m == null) seen.clear();
          else seen.remove(com.intellij.openapi.util.text.StringUtil.unquoteString(m.getText(), '\''));
        }
        default -> {
        }
      }
    }
    return r;
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
