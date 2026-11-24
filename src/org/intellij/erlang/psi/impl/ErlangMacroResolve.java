package org.intellij.erlang.psi.impl;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.macro.ErlangMacroArgSplitter;
import org.intellij.erlang.parser.macro.ErlangMacroExpander;
import org.intellij.erlang.psi.ErlangArgumentDefinition;
import org.intellij.erlang.psi.ErlangArgumentDefinitionList;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangInclude;
import org.intellij.erlang.psi.ErlangIncludeLib;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.intellij.erlang.psi.ErlangMaxExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * The {@code -define} a macro use names, chosen by arity as {@code epp} does: the definition with as many
 * parameters as the use has arguments, else the one without a parameter list (epp then leaves the parentheses as a
 * call), else the first one found. Definitions of the file come before those of its headers.
 */
public final class ErlangMacroResolve {
  /** The use has no argument list. */
  public static final int NO_ARGUMENTS = -1;
  /** The argument list is not closed. */
  public static final int UNKNOWN = -2;

  private ErlangMacroResolve() {
  }

  public static @Nullable ErlangMacrosDefinition resolve(@NotNull ErlangFile file, @NotNull String name, int arity) {
    return resolve(file, name, arity, null);
  }

  /** The first of {@link #resolveAll} at the use, else the first definition the file sees. */
  public static @Nullable ErlangMacrosDefinition resolve(@NotNull ErlangFile file, @NotNull String name, int arity, @Nullable PsiElement use) {
    List<ErlangMacrosDefinition> all = resolveAll(file, name, arity, use);
    if (!all.isEmpty()) return all.get(0);
    List<ErlangMacrosDefinition> candidates = candidatesAt(file, name, use);
    return candidates.isEmpty() ? null : candidates.get(0);
  }

  /**
   * The definitions that epp may choose for a use with this many arguments, in every
   * conditional branch: those with the same number of parameters, else those without a parameter list (epp then
   * leaves the parentheses as a call); with an unclosed argument list, every definition. Empty when none fits (epp:
   * argument mismatch). A use refers to each of them; a use of another arity refers to none of them.
   */
  public static @NotNull List<ErlangMacrosDefinition> matching(@NotNull ErlangFile file, @NotNull String name, int arity) {
    return matching(file, name, arity, null);
  }

  /** {@link #matching} among the definitions in force at the use ({@link #candidatesAt}). */
  public static @NotNull List<ErlangMacrosDefinition> matching(@NotNull ErlangFile file, @NotNull String name, int arity, @Nullable PsiElement use) {
    List<ErlangMacrosDefinition> candidates = candidatesAt(file, name, use);
    if (candidates.isEmpty() || arity == UNKNOWN) return candidates;
    List<ErlangMacrosDefinition> same = new ArrayList<>();
    for (ErlangMacrosDefinition d : candidates) if (arity(d) == arity) same.add(d);
    if (!same.isEmpty()) return same;
    List<ErlangMacrosDefinition> none = new ArrayList<>();
    for (ErlangMacrosDefinition d : candidates) if (arity(d) == NO_ARGUMENTS) none.add(d);
    return none;
  }

  /**
   * The definitions a use resolves to: of {@link #matching}, the one in the active branch when
   * the file tells it (see {@link #branchState}), else all of them except those in a branch known to be inactive (go to
   * declaration then offers them all).
   */
  public static @NotNull List<ErlangMacrosDefinition> resolveAll(@NotNull ErlangFile file, @NotNull String name, int arity) {
    return resolveAll(file, name, arity, null);
  }

  /** {@link #resolveAll} among the definitions in force at the use ({@link #candidatesAt}). */
  public static @NotNull List<ErlangMacrosDefinition> resolveAll(@NotNull ErlangFile file, @NotNull String name, int arity, @Nullable PsiElement use) {
    List<ErlangMacrosDefinition> m = matching(file, name, arity, use);
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
   * Whether the branch of a definition is active, as far as its own file tells: every
   * enclosing {@code -ifdef(X)}/{@code -ifndef(X)} (and its {@code -else}) is decided when X is predefined or defined
   * earlier in the file outside any branch; a macro that the file does not define may come from a build flag
   * ({@code -DTEST}), so its branch stays unknown, as does every {@code -if}/{@code -elif} and every header definition.
   * A definition outside any branch is active.
   */
  public static int branchState(@NotNull ErlangMacrosDefinition d) {
    PsiFile file = d.getContainingFile();
    if (!(file instanceof ErlangFile ef) || d.getParent() != file) return UNKNOWN_BRANCH;
    Integer s = CachedValuesManager.getCachedValue(ef, () -> CachedValueProvider.Result.create(
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
          boolean isDefined = defined.contains(k.macro()) || ErlangMacroExpander.PREDEFINED.contains(k.macro());
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
      String macro = m == null ? null : StringUtil.unquoteString(m.getText(), '\'');
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

  /** The attribute when the name is the one of {@code -ifdef(X)}, {@code -ifndef(X)} or {@code -undef(X)}, else null. */
  public static @Nullable ErlangAttribute directiveOf(@NotNull ErlangMacrosName name) {
    if (!(name.getParent() instanceof ErlangAtomAttribute atom) || !(atom.getParent() instanceof ErlangAttribute attribute)) return null;
    String n = atom.getName();
    return "ifdef".equals(n) || "ifndef".equals(n) || "undef".equals(n) ? attribute : null;
  }

  /**
   * Epp tests the name of a directive against what is defined at that point, any arity: the definitions before the
   * directive in the file, and those of the headers included before it. None: no target (not a definition after it).
   */
  public static @NotNull List<ErlangMacrosDefinition> definedBefore(@NotNull ErlangFile file, @NotNull PsiElement directive, @NotNull String name) {
    int at = directive.getTextRange().getStartOffset();
    List<ErlangMacrosDefinition> r = new ArrayList<>();
    for (ErlangMacrosDefinition d : file.getMacroses()) {
      if (name.equals(d.getName()) && d.getTextRange().getStartOffset() < at) r.add(d);
    }
    List<List<ErlangFile>> included = new ArrayList<>();
    for (ErlangInclude inc : file.getIncludes()) {
      if (inc.getTextRange().getStartOffset() < at) included.add(ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file));
    }
    for (ErlangIncludeLib inc : file.getIncludeLibs()) {
      if (inc.getTextRange().getStartOffset() < at) included.add(ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file));
    }
    for (List<ErlangFile> headers : included) {
      for (ErlangFile h : headers) {
        for (ErlangMacrosDefinition d : h.getMacroses()) if (name.equals(d.getName()) && !r.contains(d)) r.add(d);
        for (ErlangMacrosDefinition d : ErlangPsiImplUtil.getErlangMacrosFromIncludes(h, true, "")) {
          if (name.equals(d.getName()) && !r.contains(d)) r.add(d);
        }
      }
    }
    return r;
  }

  /**
   * The targets of the name of a directive: what is defined before it ({@link #definedBefore}); when nothing is, the
   * definitions of the name in the conditional body that the directive opens (a header guard
   * {@code -ifndef(X). -define(X, 1). -endif.}), so that navigation and rename work from the guard's name.
   */
  public static @NotNull List<ErlangMacrosDefinition> directiveTargets(@NotNull ErlangFile file, @NotNull ErlangAttribute directive,
                                                                     @NotNull String name) {
    List<ErlangMacrosDefinition> before = definedBefore(file, directive, name);
    if (!before.isEmpty() || directive.getParent() != file) return before;
    List<ErlangMacrosDefinition> inBody = new ArrayList<>();
    int depth = 0;
    for (PsiElement c = directive.getNextSibling(); c != null; c = c.getNextSibling()) {
      if (c instanceof ErlangMacrosDefinition d) {
        if (name.equals(d.getName())) inBody.add(d);
        continue;
      }
      if (!(c instanceof ErlangAttribute attribute)) continue;
      if (attribute.getIfAtomAttribute() != null) {
        depth++;
        continue;
      }
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      if (atom == null) continue;
      String a = atom.getName();
      if ("ifdef".equals(a) || "ifndef".equals(a)) depth++;
      else if ("endif".equals(a) && depth-- == 0) break;
    }
    return inBody;
  }

  /** The definitions of the name that the file sees: its own, then those of the headers it includes. */
  public static @NotNull List<ErlangMacrosDefinition> candidates(@NotNull ErlangFile file, @NotNull String name) {
    List<ErlangMacrosDefinition> r = new ArrayList<>();
    for (ErlangMacrosDefinition d : visible(file).all) if (name.equals(d.getName())) r.add(d);
    return r;
  }

  /**
   * The definitions of the name in force at a use, as epp sees them: walking the file's top-level forms up to the use, a
   * {@code -define} outside any conditional replaces the earlier definition of its arity, one inside a conditional is
   * an alternative (the branch may or may not be taken), an {@code -undef} outside any conditional ends every arity of
   * the name, and an {@code -include} brings the header's definitions at that point. Without a use, for a use inside a
   * {@code -define} body (expanded where the macro is used, so later definitions count), or when nothing is in force
   * there (epp would say "undefined macro"; the plugin stays as lenient as before): {@link #candidates}.
   */
  public static @NotNull List<ErlangMacrosDefinition> candidatesAt(@NotNull ErlangFile file, @NotNull String name, @Nullable PsiElement use) {
    if (use == null || use.getContainingFile() != file || PsiTreeUtil.getParentOfType(use, ErlangMacrosDefinition.class) != null) {
      return candidates(file, name);
    }
    List<ErlangMacrosDefinition> r = inForceAt(file, name, use);
    return r.isEmpty() ? candidates(file, name) : r;
  }

  /** The definitions of the name in force at a use of the file outside -define bodies, as {@link #candidatesAt}, without its fallback. */
  private static @NotNull List<ErlangMacrosDefinition> inForceAt(@NotNull ErlangFile file, @NotNull String name, @NotNull PsiElement use) {
    int at = use.getTextRange().getStartOffset();
    Map<Integer, List<ErlangMacrosDefinition>> byArity = new LinkedHashMap<>();
    for (Event e : visible(file).events) {
      if (e.offset >= at) break;
      if (e.definitions != null) {
        for (ErlangMacrosDefinition d : e.definitions) {
          if (name.equals(d.getName())) byArity.computeIfAbsent(arity(d), k -> new ArrayList<>()).add(d);
        }
      }
      else if (e.definition != null) {
        if (!name.equals(e.definition.getName())) continue;
        List<ErlangMacrosDefinition> list = byArity.computeIfAbsent(arity(e.definition), k -> new ArrayList<>());
        if (e.depth == 0) list.clear();
        list.add(e.definition);
      }
      else if (name.equals(e.undef) && e.depth == 0) {
        byArity.clear();
      }
    }
    List<ErlangMacrosDefinition> r = new ArrayList<>();
    for (List<ErlangMacrosDefinition> list : byArity.values()) r.addAll(list);
    return r;
  }

  /** One top-level step of the file for {@link #candidatesAt}: a definition, an undef, or the definitions of an include. */
  private record Event(int offset, int depth, @Nullable ErlangMacrosDefinition definition, @Nullable String undef,
                       @Nullable List<ErlangMacrosDefinition> definitions) {
  }

  private record Visible(@NotNull List<ErlangMacrosDefinition> all, @NotNull List<Event> events) {
  }

  /** The definitions the file sees and the steps of its top level; cached until a PSI change (headers count too). */
  private static @NotNull Visible visible(@NotNull ErlangFile file) {
    return CachedValuesManager.getCachedValue(file, () -> CachedValueProvider.Result.create(
      computeVisible(file), PsiModificationTracker.MODIFICATION_COUNT, ProjectRootManager.getInstance(file.getProject())));
  }

  private static @NotNull Visible computeVisible(@NotNull ErlangFile file) {
    List<ErlangMacrosDefinition> all = new ArrayList<>(file.getMacroses());
    all.addAll(ErlangPsiImplUtil.getErlangMacrosFromIncludes(file, true, ""));
    List<Event> events = new ArrayList<>();
    int depth = 0;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      int offset = c.getTextRange().getStartOffset();
      if (c instanceof ErlangMacrosDefinition d) {
        if (d.getName() != null) events.add(new Event(offset, depth, d, null, null));
        continue;
      }
      ErlangInclude include = c instanceof ErlangInclude i ? i : PsiTreeUtil.getChildOfType(c, ErlangInclude.class);
      ErlangIncludeLib includeLib = c instanceof ErlangIncludeLib i ? i : PsiTreeUtil.getChildOfType(c, ErlangIncludeLib.class);
      if (include != null || includeLib != null) {
        List<ErlangFile> headers = include != null ? ErlangPsiImplUtil.getDirectlyIncludedFiles(include, file)
                                                   : ErlangPsiImplUtil.getDirectlyIncludedFiles(includeLib, file);
        List<ErlangMacrosDefinition> defs = new ArrayList<>();
        for (ErlangFile h : headers) {
          defs.addAll(h.getMacroses());
          defs.addAll(ErlangPsiImplUtil.getErlangMacrosFromIncludes(h, true, ""));
        }
        events.add(new Event(offset, depth, null, null, defs));
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
          if (m != null) events.add(new Event(offset, depth, null, StringUtil.unquoteString(m.getText(), '\''), null));
        }
        default -> {
        }
      }
    }
    return new Visible(List.copyOf(all), List.copyOf(events));
  }

  /**
   * The target of a macro reference for code that needs one definition (parameter info, record
   * fields of a macro body, the inspections): its single target, else the first of its possible definitions.
   */
  public static @Nullable PsiElement firstTarget(@Nullable PsiReference ref) {
    if (ref == null) return null;
    PsiElement single = ref.resolve();
    if (single != null) return single;
    if (ref instanceof PsiPolyVariantReference p) {
      for (ResolveResult r : p.multiResolve(false)) if (r.getElement() != null) return r.getElement();
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

  /**
   * Epp's errors of a {@code -define}: "redefining macro" where it is certain ({@link #isRedefinition}), "badly formed
   * 'define'" for a parameter that is not a variable, "argument 'X' already used"; by element, in order.
   */
  public static @NotNull Map<PsiElement, String> defineProblems(@NotNull ErlangMacrosDefinition d) {
    Map<PsiElement, String> r = new LinkedHashMap<>();
    ErlangMacrosName name = d.getMacrosName();
    if (name != null && isRedefinition(d)) r.put(name, "Redefining macro '" + d.getName() + "'");
    ErlangArgumentDefinitionList list = d.getArgumentDefinitionList();
    if (list == null) return r;
    Set<String> seen = new HashSet<>();
    for (ErlangArgumentDefinition a : list.getArgumentDefinitionList()) {
      String text = a.getText();
      if (!(a.getExpression() instanceof ErlangMaxExpression m) || m.getQVar() == null || !m.getQVar().getText().equals(text)) {
        r.put(a, "Badly formed 'define': a parameter must be a variable");
      }
      else if (!seen.add(text)) {
        r.put(a, "Argument '" + text + "' already used");
      }
    }
    return r;
  }

  /**
   * Epp's "argument mismatch for macro" at a use, or null: no definition takes as many arguments as the use has, and
   * none takes no parameter list (then epp leaves the parentheses as a call). Only where it is certain: every include
   * of the file resolves (no definition is out of sight), the use is in no conditional branch (it may be skipped),
   * and no conditional tests the name (a build flag may give it).
   */
  public static @Nullable String argumentMismatch(@NotNull PsiElement nameLeaf, @NotNull String name) {
    if (!(nameLeaf.getContainingFile() instanceof ErlangFile file) || !allIncludesResolve(file)) return null;
    int arity = arityAfter(nameLeaf);
    if (arity == UNKNOWN) return null;
    List<ErlangMacrosDefinition> candidates = candidates(file, name);
    if (candidates.isEmpty()) return null;
    for (ErlangMacrosDefinition d : candidates) {
      int a = arity(d);
      if (a == arity || a == NO_ARGUMENTS) return null;
    }
    if (isInConditional(nameLeaf) || isTestedByConditional(file, name)) return null;
    String uses = arity == NO_ARGUMENTS ? "no arguments" : arity == 1 ? "1 argument" : arity + " arguments";
    return "Argument mismatch for macro '?" + name + "': no definition takes " + uses;
  }

  /**
   * Epp's "argument mismatch for macro" at a use of a predefined feature macro ({@code FEATURE_ENABLED},
   * {@code FEATURE_AVAILABLE}) that the file defines too. Epp keeps the predefined definition, which takes one argument,
   * next to the file's own ones, so a definition without a parameter list matches only a use without arguments, and
   * only the definitions in force at the use count. For a use outside -define bodies (epp checks a body where the macro
   * is expanded; the caller skips those), and only where it is certain, as {@link #argumentMismatch}; an {@code -undef} of the name in the
   * file or in a header it includes may remove the predefined definition, so then nothing is certain.
   */
  public static boolean isFeatureArgumentMismatch(@NotNull PsiElement nameLeaf, @NotNull String name) {
    if (!(nameLeaf.getContainingFile() instanceof ErlangFile file) || !allIncludesResolve(file)) return false;
    int arity = arityAfter(nameLeaf);
    if (arity == UNKNOWN || arity == 1) return false;
    for (ErlangMacrosDefinition d : inForceAt(file, name, nameLeaf)) if (arity(d) == arity) return false;
    return !isInConditional(nameLeaf) && !isTestedByConditional(file, name) && !isUndefined(file, name, new HashSet<>());
  }

  /** An {@code -undef} of the name, at any depth, in the file or in a header it includes (directly or not). */
  private static boolean isUndefined(@NotNull ErlangFile file, @NotNull String name, @NotNull Set<ErlangFile> seen) {
    if (!seen.add(file)) return false;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (!(c instanceof ErlangAttribute attribute) || attribute.getAtomAttribute() == null) continue;
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      if (!"undef".equals(atom.getName())) continue;
      ErlangMacrosName m = PsiTreeUtil.getChildOfType(atom, ErlangMacrosName.class);
      if (m != null && name.equals(StringUtil.unquoteString(m.getText(), '\''))) return true;
    }
    for (ErlangInclude inc : file.getIncludes()) {
      for (ErlangFile h : ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file)) if (isUndefined(h, name, seen)) return true;
    }
    for (ErlangIncludeLib inc : file.getIncludeLibs()) {
      for (ErlangFile h : ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file)) if (isUndefined(h, name, seen)) return true;
    }
    return false;
  }

  /** The element is in a top-level form inside an {@code -if}, {@code -ifdef} or {@code -ifndef} branch of its file. */
  public static boolean isInConditional(@NotNull PsiElement element) {
    if (!(element.getContainingFile() instanceof ErlangFile file)) return false;
    int at = element.getTextRange().getStartOffset(), depth = 0;
    for (PsiElement c = file.getFirstChild(); c != null && c.getTextRange().getStartOffset() <= at; c = c.getNextSibling()) {
      if (!(c instanceof ErlangAttribute attribute)) continue;
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      String a = attribute.getIfAtomAttribute() != null ? "if" : atom != null ? atom.getName() : null;
      if ("if".equals(a) || "ifdef".equals(a) || "ifndef".equals(a)) depth++;
      else if ("endif".equals(a)) depth = Math.max(0, depth - 1);
    }
    return depth > 0;
  }

  /** An {@code -ifdef} or {@code -ifndef} of the file or of a header it includes (directly or not) tests the name. */
  public static boolean isTestedByConditional(@NotNull ErlangFile file, @NotNull String name) {
    return isTestedByConditional(file, name, new HashSet<>());
  }

  private static boolean isTestedByConditional(@NotNull ErlangFile file, @NotNull String name, @NotNull Set<ErlangFile> seen) {
    if (!seen.add(file)) return false;
    for (PsiElement c = file.getFirstChild(); c != null; c = c.getNextSibling()) {
      if (!(c instanceof ErlangAttribute attribute) || attribute.getAtomAttribute() == null) continue;
      ErlangAtomAttribute atom = attribute.getAtomAttribute();
      if (!"ifdef".equals(atom.getName()) && !"ifndef".equals(atom.getName())) continue;
      ErlangMacrosName m = PsiTreeUtil.getChildOfType(atom, ErlangMacrosName.class);
      if (m != null && name.equals(StringUtil.unquoteString(m.getText(), '\''))) return true;
    }
    for (ErlangInclude inc : file.getIncludes()) {
      for (ErlangFile h : ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file)) if (isTestedByConditional(h, name, seen)) return true;
    }
    for (ErlangIncludeLib inc : file.getIncludeLibs()) {
      for (ErlangFile h : ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file)) if (isTestedByConditional(h, name, seen)) return true;
    }
    return false;
  }

  /** Every include of the file, and of the headers it includes, resolves to a file (cached until a PSI change). */
  public static boolean allIncludesResolve(@NotNull ErlangFile file) {
    return CachedValuesManager.getCachedValue(file, () -> CachedValueProvider.Result.create(
      allIncludesResolve(file, new HashSet<>()), PsiModificationTracker.MODIFICATION_COUNT));
  }

  private static boolean allIncludesResolve(@NotNull ErlangFile file, @NotNull Set<ErlangFile> seen) {
    if (!seen.add(file)) return true;
    for (ErlangInclude inc : file.getIncludes()) {
      List<ErlangFile> r = ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file);
      if (r.isEmpty() || !allIncludesResolve(r.get(0), seen)) return false;
    }
    for (ErlangIncludeLib inc : file.getIncludeLibs()) {
      List<ErlangFile> r = ErlangPsiImplUtil.getDirectlyIncludedFiles(inc, file);
      if (r.isEmpty() || !allIncludesResolve(r.get(0), seen)) return false;
    }
    return true;
  }

  /**
   * Epp's "redefining macro": a second definition of the same name and arity. Only where it is certain: both
   * definitions in this file, outside any conditional, with no {@code -undef} of the name (at any depth) and no
   * include between them (a header may undefine it).
   */
  public static boolean isRedefinition(@NotNull ErlangMacrosDefinition d) {
    if (!(d.getContainingFile() instanceof ErlangFile file)) return false;
    return CachedValuesManager.getCachedValue(file, () -> CachedValueProvider.Result.create(
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
      if (c instanceof ErlangInclude || c instanceof ErlangIncludeLib ||
          PsiTreeUtil.getChildOfType(c, ErlangInclude.class) != null ||
          PsiTreeUtil.getChildOfType(c, ErlangIncludeLib.class) != null) {
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
          else seen.remove(StringUtil.unquoteString(m.getText(), '\''));
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
   * The number of arguments after a macro name token, counted on the tokens (the tree there may be bare tokens) as
   * epp's macro_arg splits them ({@link ErlangMacroArgSplitter}).
   */
  public static int arityAfter(@NotNull PsiElement name) {
    PsiElement next = nextToken(name);
    if (next != null && next.getNode().getElementType() == ErlangTypes.ERL_SINGLE_QUOTE && name.getNode().getElementType() == ErlangTypes.ERL_ATOM_NAME) {
      next = nextToken(next);
    }
    if (next == null || next.getNode().getElementType() != ErlangTypes.ERL_PAR_LEFT) return NO_ARGUMENTS;
    ErlangMacroArgSplitter splitter = new ErlangMacroArgSplitter();
    int separators = 0;
    boolean any = false;
    for (PsiElement t = nextToken(next); t != null; t = nextToken(t)) {
      var type = t.getNode().getElementType();
      if (type == ErlangTypes.ERL_DOT) return UNKNOWN;
      ErlangMacroArgSplitter.Step step = splitter.next(type, t.getText());
      if (step == ErlangMacroArgSplitter.Step.CLOSE) return any ? separators + 1 : 0;
      any = true;
      if (step == ErlangMacroArgSplitter.Step.SEPARATOR) separators++;
    }
    return UNKNOWN;
  }

  private static @Nullable PsiElement nextToken(@NotNull PsiElement e) {
    PsiElement n = PsiTreeUtil.nextLeaf(e);
    while (n != null && (n instanceof PsiWhiteSpace || n instanceof PsiComment || n.getTextLength() == 0)) n = PsiTreeUtil.nextLeaf(n);
    return n;
  }

  /**
   * The element of a macro name with the given text: a variable, an atom, or a quoted atom when the name needs quotes;
   * null when no macro can have that name.
   */
  public static @Nullable PsiElement nameElement(@NotNull Project project, @NotNull String name) {
    for (String text : new String[]{name, '\'' + name + '\''}) {
      try {
        if (ErlangElementFactory.createMacrosFromText(project, text) instanceof ErlangMacrosName m && m.getText().equals(text)) {
          if (m.getVar() != null) return m.getVar();
          if (m.getAtom() != null) return m.getAtom();
        }
      }
      catch (Exception ignore) {
      }
    }
    return null;
  }

  /**
   * Renames a name token that the tree keeps bare (a macro use the parser did not read, a name in a raw
   * {@code -define} body): the token and its quotes, if any, are replaced by the new name, quoted when it needs quotes.
   * False, with nothing changed, when no name element can be made of the new name.
   */
  public static boolean renameBareName(@NotNull PsiElement nameLeaf, @NotNull String newName) {
    PsiElement replacement = nameElement(nameLeaf.getProject(), newName);
    if (replacement == null) return false;
    PsiElement prev = PsiTreeUtil.prevLeaf(nameLeaf), next = PsiTreeUtil.nextLeaf(nameLeaf);
    boolean quoted = prev != null && next != null && prev.getNode().getElementType() == ErlangTypes.ERL_SINGLE_QUOTE &&
                     next.getNode().getElementType() == ErlangTypes.ERL_SINGLE_QUOTE;
    PsiElement first = quoted ? prev : nameLeaf;
    first.getParent().addBefore(replacement, first);
    if (quoted) {
      prev.delete();
      next.delete();
    }
    nameLeaf.delete();
    return true;
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
