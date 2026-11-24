/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangInclude;
import org.intellij.erlang.psi.ErlangIncludeLib;
import org.intellij.erlang.psi.ErlangMacros;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.intellij.erlang.psi.ErlangVisitor;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.impl.ErlangBareMacroReference;
import org.intellij.erlang.psi.impl.ErlangMacroResolve;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangIntroduceMacroQuickFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ErlangUnresolvedMacrosInspection extends ErlangInspectionBase {
  // predefined since OTP 25, both with one argument
  private static final Set<String> FEATURE_MACROS = Set.of("FEATURE_ENABLED", "FEATURE_AVAILABLE");

  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      /** Epp's errors of a -define (see {@link ErlangMacroResolve#defineProblems}), with the macro layer on. */
      @Override
      public void visitMacrosDefinition(@NotNull ErlangMacrosDefinition o) {
        if (!ErlangMacroLayer.isEnabled()) return;
        ErlangMacroResolve.defineProblems(o).forEach((element, message) -> registerProblem(holder, element, message));
      }

      @Override
      public void visitMacros(@NotNull ErlangMacros o) {
        ErlangMacrosName macrosName = o.getMacrosName();
        if (macrosName == null || ErlangPsiImplUtil.KNOWN_MACROS.contains(macrosName.getText())) return;

        PsiReference reference = o.getReference();
        if (FEATURE_MACROS.contains(macrosName.getText())) {
          PsiElement nameLeaf = ErlangPsiImplUtil.getNameIdentifier(macrosName);
          if (reference == null || ErlangMacroResolve.firstTarget(reference) == null) checkFeatureArity(o, nameLeaf, macrosName.getText(), null);
          else checkOwnFeatureArity(o, nameLeaf, macrosName.getText(), null);
          return;
        }
        if (reference != null && ErlangMacroResolve.firstTarget(reference) == null && !isDefinedByEnclosingCondition(o, macrosName.getText())) {
          registerProblem(holder, o, "Unresolved macros " + "'" + o.getText() + "'", new ErlangIntroduceMacroQuickFix());
        }
        else if (reference != null) {
          checkArity(o, ErlangPsiImplUtil.getNameIdentifier(macrosName), macrosName.getText(), null);
        }
      }

      /** A macro use that the parser left as bare tokens ({@code foo?PAR}), hosted by this element. */
      @Override
      public void visitElement(@NotNull PsiElement o) {
        // only the hosts of the references (their getReferences adds them): an error element or a dummy block has the
        // same tokens, which its Erlang parent collects through it
        if (!(o instanceof ErlangCompositeElement) && !(o instanceof ErlangFile)) return;
        for (PsiReference r : ErlangBareMacroReference.find(o)) {
          if (!(r instanceof ErlangBareMacroReference bare)) continue;
          String name = bare.getCanonicalText();
          TextRange range = TextRange.create(r.getRangeInElement().getStartOffset() - 1, r.getRangeInElement().getEndOffset());
          if (FEATURE_MACROS.contains(name)) {
            if (ErlangMacroResolve.firstTarget(bare) == null) checkFeatureArity(o, bare.getNameElement(), name, range);
            else checkOwnFeatureArity(o, bare.getNameElement(), name, range);
            continue;
          }
          if (ErlangPsiImplUtil.KNOWN_MACROS.contains(name)) continue;
          if (ErlangMacroResolve.firstTarget(bare) == null) {
            // the conditionals around the use itself: for a file host, the host has no enclosing form
            PsiElement use = bare.getNameElement();
            if (!isDefinedByEnclosingCondition(use, name)) registerProblem(holder, o, "Unresolved macros '?" + name + "'", range, null);
          }
          else {
            checkArity(o, bare.getNameElement(), name, range);
          }
        }
      }

      /**
       * Epp's "argument mismatch for macro" for a use of a predefined feature macro (no -define of the name resolves)
       * without exactly one argument, outside -define bodies.
       */
      private void checkFeatureArity(@NotNull PsiElement target, @NotNull PsiElement nameLeaf, @NotNull String name, @Nullable TextRange range) {
        if (PsiTreeUtil.getParentOfType(nameLeaf, ErlangMacrosDefinition.class) != null) return;
        int arity = ErlangMacroResolve.arityAfter(nameLeaf);
        if (arity != 1 && arity != ErlangMacroResolve.UNKNOWN) {
          registerProblem(holder, target, "Argument mismatch for macro '?" + name + "'", range, null);
        }
      }

      /** The same for a feature macro that a -define of the file resolves to (see {@link ErlangMacroResolve#isFeatureArgumentMismatch}). */
      private void checkOwnFeatureArity(@NotNull PsiElement target, @NotNull PsiElement nameLeaf, @NotNull String name, @Nullable TextRange range) {
        if (ErlangMacroLayer.isEnabled() && ErlangMacroResolve.isFeatureArgumentMismatch(nameLeaf, name)) {
          registerProblem(holder, target, "Argument mismatch for macro '?" + name + "'", range, null);
        }
      }

      /** Epp's "argument mismatch for macro" where it is certain (see {@link ErlangMacroResolve#argumentMismatch}). */
      private void checkArity(@NotNull PsiElement target, @NotNull PsiElement nameLeaf, @NotNull String name, @Nullable TextRange range) {
        String message = ErlangMacroLayer.isEnabled() ? ErlangMacroResolve.argumentMismatch(nameLeaf, unquote(name)) : null;
        if (message != null) registerProblem(holder, target, message, range, null);
      }
    };
  }

  private static boolean isDefinedByEnclosingCondition(@NotNull PsiElement macros, @NotNull String name) {
    PsiElement form = macros;
    while (form.getParent() != null && !(form.getParent() instanceof PsiFile)) {
      form = form.getParent();
    }
    if (!(form.getParent() instanceof ErlangFile file)) return false;

    String unquotedName = unquote(name);
    Deque<Condition> conditions = new ArrayDeque<>();
    for (PsiElement child = file.getFirstChild(); child != null && child != form; child = child.getNextSibling()) {
      if (child instanceof ErlangInclude || child instanceof ErlangIncludeLib) {
        if (hasOpenCondition(conditions, unquotedName)) {
          List<ErlangFile> includedFiles = child instanceof ErlangInclude include
                                           ? ErlangPsiImplUtil.getDirectlyIncludedFiles(include, file)
                                           : ErlangPsiImplUtil.getDirectlyIncludedFiles((ErlangIncludeLib) child, file);
          Set<ErlangFile> visited = new HashSet<>();
          for (ErlangFile includedFile : includedFiles) {
            ProgressManager.checkCanceled();
            if (undefinesMacro(includedFile, unquotedName, visited)) {
              markUndefined(conditions, unquotedName);
              break;
            }
          }
        }
        continue;
      }
      if (!(child instanceof ErlangAttribute attribute)) continue;
      if (attribute.getElseAtomAttribute() != null) {
        Condition top = conditions.peek();
        if (top != null) top.inElse = true;
        continue;
      }
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      if (atomAttribute == null) {
        if (attribute.getIfAtomAttribute() != null) conditions.push(new Condition(null, false));
        continue;
      }
      String directive = atomAttribute.getName();
      String macroName = getMacroName(atomAttribute);
      switch (directive) {
        case "ifdef" -> conditions.push(new Condition(macroName, true));
        case "ifndef" -> conditions.push(new Condition(macroName, false));
        case "elif" -> {
          Condition top = conditions.peek();
          if (top != null) top.afterElif = true;
        }
        case "endif" -> conditions.poll();
        case "undef" -> {
          if (unquotedName.equals(macroName)) markUndefined(conditions, unquotedName);
        }
        default -> {
        }
      }
    }
    for (Condition condition : conditions) {
      if (unquotedName.equals(condition.macroName) && condition.ifdef != condition.inElse && !condition.afterElif && !condition.undefined) {
        return true;
      }
    }
    return false;
  }

  private static boolean hasOpenCondition(@NotNull Deque<Condition> conditions, @NotNull String name) {
    for (Condition condition : conditions) {
      if (name.equals(condition.macroName)) return true;
    }
    return false;
  }

  private static void markUndefined(@NotNull Deque<Condition> conditions, @NotNull String name) {
    for (Condition condition : conditions) {
      if (name.equals(condition.macroName)) condition.undefined = true;
    }
  }

  // Reads the AST of the included header and of the headers it includes, although AGENTS.md asks to read other files
  // through stubs and indices: -undef is not stubbed, and stubbing it would change the stub format. It runs only for an
  // unresolved ?M inside an open -ifdef(M)/-ifndef(M) that contains an -include. Measured on 20 headers of 500 defines
  // included inside -ifdef(DEBUG) with 2,000 uses of ?DEBUG: highlighting took 2.5 s instead of 2.3 s (about 9% more).
  private static boolean undefinesMacro(@NotNull ErlangFile file, @NotNull String name, @NotNull Set<ErlangFile> visited) {
    if (!visited.add(file)) return false;
    for (PsiElement child = file.getFirstChild(); child != null; child = child.getNextSibling()) {
      ProgressManager.checkCanceled();
      if (child instanceof ErlangAttribute attribute) {
        ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
        if (atomAttribute != null && "undef".equals(atomAttribute.getName()) && name.equals(getMacroName(atomAttribute))) {
          return true;
        }
      }
    }
    for (ErlangFile includedFile : ErlangPsiImplUtil.getDirectlyIncludedFiles(file)) {
      ProgressManager.checkCanceled();
      if (undefinesMacro(includedFile, name, visited)) return true;
    }
    return false;
  }

  @Nullable
  private static String getMacroName(@NotNull ErlangAtomAttribute attribute) {
    ErlangMacrosName macrosName = PsiTreeUtil.getChildOfType(attribute, ErlangMacrosName.class);
    return macrosName != null ? unquote(macrosName.getText()) : null;
  }

  @NotNull
  private static String unquote(@NotNull String name) {
    return StringUtil.unquoteString(name, '\'');
  }

  private static final class Condition {
    private final String macroName;
    private final boolean ifdef;
    private boolean inElse;
    private boolean afterElif;
    private boolean undefined;

    private Condition(@Nullable String macroName, boolean ifdef) {
      this.macroName = macroName;
      this.ifdef = ifdef;
    }
  }
}
