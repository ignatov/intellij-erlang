package org.intellij.erlang.rename;

import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.refactoring.listeners.RefactoringElementListener;
import com.intellij.refactoring.rename.RenamePsiElementProcessor;
import com.intellij.refactoring.rename.UnresolvableCollisionUsageInfo;
import com.intellij.usageView.UsageInfo;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.macrolayer.ErlangMacroLayerHeaderTarget;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.search.ErlangFindUsagesHandlerFactory;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Round 5: rename of names that the macro layer resolves, and its conflicts.
 * <ul>
 * <li>A name made by a macro whose declaration is a token of a macro argument or a {@code -define} body
 * ({@link ErlangMacroLayer#isNameMadeByMacro}), and a name in the body of a header macro
 * ({@link ErlangMacroLayerHeaderTarget}): the references are renamed, then the declaration token, in the real file
 * (for a header variable: every occurrence of the variable in that {@code -define} body). Nothing of a copy is
 * written. For a header, the references in every includer are found by the reference search (its use scope).</li>
 * <li>Conflicts: a header variable whose new name is already a variable of a function that uses the macro, in any
 * includer; a header function, record or type whose new name an includer already has; and a function called in a
 * header macro body that the macro's uses resolve to functions of other includers too (renaming the call in the
 * header would break them, leaving it would break this file).</li>
 * </ul>
 */
public class ErlangMacroLayerRenameProcessor extends RenamePsiElementProcessor {
  @Override
  public boolean canProcessElement(@NotNull PsiElement element) {
    return element instanceof ErlangMacroLayerHeaderTarget || ErlangMacroLayer.isNameMadeByMacro(element) ||
           element instanceof ErlangFunction && ErlangMacroLayer.isEnabled();
  }

  /** The functions of the main tree that a made function name continues are renamed with it (their callers too). */
  @Override
  public void prepareRenaming(@NotNull PsiElement element, @NotNull String newName, @NotNull Map<PsiElement, String> allRenames) {
    for (ErlangFunction f : ErlangMacroLayer.functionsContinuingMadeName(element)) allRenames.put(f, newName);
  }

  private static boolean isLayerTarget(@NotNull PsiElement element) {
    return element instanceof ErlangMacroLayerHeaderTarget || ErlangMacroLayer.isNameMadeByMacro(element);
  }

  @Override
  public @NotNull Collection<PsiReference> findReferences(@NotNull PsiElement element, @NotNull SearchScope searchScope,
                                                         boolean searchInCommentsAndStrings) {
    if (!ErlangMacroLayer.isNameMadeByMacro(element)) return super.findReferences(element, searchScope, searchInCommentsAndStrings);
    List<PsiReference> r = Collections.synchronizedList(new ArrayList<>());
    ErlangFindUsagesHandlerFactory.processReferencesToNameMadeByMacro(element, searchScope.intersectWith(element.getUseScope()), r::add);
    return r;
  }

  @Override
  public void renameElement(@NotNull PsiElement element, @NotNull String newName, UsageInfo @NotNull [] usages,
                            @Nullable RefactoringElementListener listener) throws IncorrectOperationException {
    if (!isLayerTarget(element)) {
      super.renameElement(element, newName, usages, listener);
      return;
    }
    // the other clause names of a made function (verbatim tokens after the declaration: renaming them first moves
    // nothing before them), then the declaration tokens, while the target still resolves; then the references
    List<PsiElement> clauses = new ArrayList<>(ErlangMacroLayer.otherClauseNamesOfMadeFunction(element));
    clauses.sort(Comparator.comparingInt((PsiElement e) -> e.getTextRange().getStartOffset()).reversed());
    for (PsiElement leaf : clauses) {
      ErlangAtom atom = PsiTreeUtil.getParentOfType(leaf, ErlangAtom.class, false);
      if (atom != null && atom.getTextRange().equals(leaf.getTextRange())) ErlangPsiImplUtil.renameAtom(atom, newName);
      else ErlangMacroResolve.renameBareName(leaf, newName);
    }
    renameDeclaration(element, newName);
    for (UsageInfo usage : usages) {
      PsiReference ref = usage.getReference();
      if (ref != null) ref.handleElementRename(newName);
    }
  }

  /** Renames the token that declares the name, in its real file; a header variable everywhere in its macro body. */
  private static void renameDeclaration(@NotNull PsiElement element, @NotNull String newName) {
    PsiFile file;
    TextRange range;
    if (element instanceof ErlangMacroLayerHeaderTarget target) {
      file = target.getContainingFile();
      range = target.currentRange();
    }
    else {
      file = element.getContainingFile();
      range = element.getTextRange();
    }
    if (file == null || range == null) throw new IncorrectOperationException("The declaration of the name was not found");
    PsiElement leaf = file.findElementAt(range.getStartOffset());
    ErlangQVar var = PsiTreeUtil.getParentOfType(leaf, ErlangQVar.class, false);
    if (var != null && var.getTextRange().equals(range)) {
      ErlangMacrosBody body = PsiTreeUtil.getParentOfType(var, ErlangMacrosBody.class);
      List<ErlangQVar> all = new ArrayList<>();
      if (body != null) {
        for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(body, ErlangQVar.class)) if (v.getText().equals(var.getText())) all.add(v);
      }
      else all.add(var);
      for (ErlangQVar v : all) ErlangPsiImplUtil.setName(v, newName);
      return;
    }
    ErlangAtom atom = PsiTreeUtil.getParentOfType(leaf, ErlangAtom.class, false);
    if (atom != null && atom.getTextRange().equals(range)) {
      ErlangPsiImplUtil.renameAtom(atom, newName);
      return;
    }
    // a name in a -define body that the parser keeps as raw tokens (sign(N) when N > 0 -> of a fragment macro)
    if (leaf != null && leaf.getTextRange().equals(range) && leaf.getNode().getElementType() == ErlangTypes.ERL_ATOM_NAME &&
        ErlangMacroResolve.nameElement(leaf.getProject(), newName) instanceof ErlangAtom && ErlangMacroResolve.renameBareName(leaf, newName)) {
      return;
    }
    throw new IncorrectOperationException("The declaration of the name was not found");
  }

  @Override
  public void findCollisions(@NotNull PsiElement element, @NotNull String newName, @NotNull Map<? extends PsiElement, String> allRenames,
                             @NotNull List<UsageInfo> result) {
    super.findCollisions(element, newName, allRenames, result);
    if (element instanceof ErlangFunction function) {
      sharedHeaderBodyCalls(function, result);
      return;
    }
    if (!(element instanceof ErlangMacroLayerHeaderTarget target)) return;
    PsiFile header = target.getContainingFile();
    TextRange range = target.currentRange();
    PsiElement leaf = header == null || range == null ? null : header.findElementAt(range.getStartOffset());
    ErlangMacrosDefinition def = PsiTreeUtil.getParentOfType(leaf, ErlangMacrosDefinition.class);
    if (def == null || def.getName() == null) return;
    String macro = def.getName();
    for (ErlangFile includer : ErlangMacroLayer.includersOfHeader(header, macro)) {
      String place = includer.getName();
      switch (target.getKind()) {
        case "variable" -> variableCollisions(includer, macro, newName, element, result);
        case "function" -> {
          for (ErlangFunction f : includer.getFunctions()) {
            if (newName.equals(f.getName())) result.add(collision(f, element, "Function " + newName + "/" + f.getArity() + " already exists in " + place));
          }
        }
        case "record" -> {
          if (includer.getRecord(newName) != null) result.add(collision(includer.getRecord(newName), element, "Record " + newName + " already exists in " + place));
        }
        case "type" -> {
          if (includer.getType(newName) != null) result.add(collision(includer.getType(newName), element, "Type " + newName + " already exists in " + place));
        }
        default -> {
        }
      }
    }
  }

  /** Calls in header macro bodies that rename would change, while the macro's uses also resolve them elsewhere. */
  private static void sharedHeaderBodyCalls(@NotNull ErlangFunction function, @NotNull List<UsageInfo> usages) {
    PsiManager manager = function.getManager();
    for (UsageInfo usage : new ArrayList<>(usages)) {
      PsiReference ref = usage.getReference();
      PsiElement owner = ref == null ? null : ref.getElement();
      if (owner == null || PsiTreeUtil.getParentOfType(owner, ErlangMacrosBody.class) == null) continue;
      PsiFile file = owner.getContainingFile();
      if (file == null || file == function.getContainingFile()) continue;
      PsiElement leaf = file.findElementAt(owner.getTextRange().getStartOffset() + ref.getRangeInElement().getStartOffset());
      if (leaf == null) continue;
      Set<String> others = new TreeSet<>();
      for (PsiElement t : ErlangMacroLayer.resolveInMacroBody(leaf)) {
        if (!manager.areElementsEquivalent(t, function) && t.getContainingFile() != null) others.add(t.getContainingFile().getName());
      }
      if (others.isEmpty()) continue;
      ErlangMacrosDefinition def = PsiTreeUtil.getParentOfType(owner, ErlangMacrosDefinition.class);
      usages.add(collision(owner, function, "The call " + leaf.getText() + " in the body of ?" + (def == null ? "" : def.getName()) +
                                            " in " + file.getName() + " also calls " + leaf.getText() + " of " +
                                            StringUtil.join(others, ", ") + ": renaming it would break " +
                                            (others.size() == 1 ? "that file" : "those files")));
    }
  }

  /** A function of the file that uses the macro already has a variable with the new name. */
  static void variableCollisions(@NotNull ErlangFile file, @NotNull String macro, @NotNull String newName, @NotNull PsiElement renamed,
                                 @NotNull List<UsageInfo> result) {
    for (ErlangFunction f : functionsUsingMacro(file, macro)) {
      for (ErlangQVar v : PsiTreeUtil.findChildrenOfType(f, ErlangQVar.class)) {
        if (v.getText().equals(newName)) {
          result.add(collision(v, renamed, "Variable " + newName + " already exists in " + f.getName() + "/" + f.getArity() +
                                           " of " + file.getName() + ", which uses ?" + macro));
          break;
        }
      }
    }
  }

  private static @NotNull List<ErlangFunction> functionsUsingMacro(@NotNull ErlangFile file, @NotNull String macro) {
    Set<ErlangFunction> r = new LinkedHashSet<>();
    for (ErlangMacrosName name : PsiTreeUtil.findChildrenOfType(file, ErlangMacrosName.class)) {
      if (!macro.equals(name.getText()) || PsiTreeUtil.getParentOfType(name, ErlangMacrosDefinition.class) != null) continue;
      ErlangFunction f = PsiTreeUtil.getParentOfType(name, ErlangFunction.class);
      if (f != null) r.add(f);
    }
    return new ArrayList<>(r);
  }

  static @NotNull UsageInfo collision(@NotNull PsiElement place, @NotNull PsiElement renamed, @NotNull String description) {
    return new UnresolvableCollisionUsageInfo(place, renamed) {
      @Override
      public String getDescription() {
        return description;
      }
    };
  }
}
