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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangAtomAttribute;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangMacros;
import org.intellij.erlang.psi.ErlangMacrosName;
import org.intellij.erlang.psi.ErlangVisitor;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangIntroduceMacroQuickFix;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.Deque;

public class ErlangUnresolvedMacrosInspection extends ErlangInspectionBase {
  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitMacros(@NotNull ErlangMacros o) {
        ErlangMacrosName macrosName = o.getMacrosName();
        if (macrosName == null || ErlangPsiImplUtil.KNOWN_MACROS.contains(macrosName.getText())) return;

        PsiReference reference = o.getReference();
        if (reference != null && reference.resolve() == null && !isDefinedByEnclosingCondition(o, macrosName.getText())) {
          registerProblem(holder, o, "Unresolved macros " + "'" + o.getText() + "'", new ErlangIntroduceMacroQuickFix());
        }
      }
    };
  }

  private static boolean isDefinedByEnclosingCondition(@NotNull ErlangMacros macros, @NotNull String name) {
    PsiElement form = macros;
    while (form.getParent() != null && !(form.getParent() instanceof PsiFile)) {
      form = form.getParent();
    }
    if (form.getParent() == null) return false;

    Deque<Condition> conditions = new ArrayDeque<>();
    for (PsiElement child = form.getParent().getFirstChild(); child != null && child != form; child = child.getNextSibling()) {
      if (!(child instanceof ErlangAttribute attribute)) continue;
      if (attribute.getElseAtomAttribute() != null) {
        Condition top = conditions.peek();
        if (top != null) top.inElse = true;
        continue;
      }
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      if (atomAttribute == null) continue;
      String directive = atomAttribute.getName();
      String macroName = getMacroName(atomAttribute);
      switch (directive) {
        case "ifdef" -> conditions.push(new Condition(macroName, true));
        case "ifndef" -> conditions.push(new Condition(macroName, false));
        case "endif" -> conditions.poll();
        case "undef" -> {
          for (Condition condition : conditions) {
            if (name.equals(condition.macroName)) condition.undefined = true;
          }
        }
        default -> {
        }
      }
    }
    for (Condition condition : conditions) {
      if (name.equals(condition.macroName) && condition.ifdef != condition.inElse && !condition.undefined) return true;
    }
    return false;
  }

  @Nullable
  private static String getMacroName(@NotNull ErlangAtomAttribute attribute) {
    ErlangMacrosName macrosName = PsiTreeUtil.getChildOfType(attribute, ErlangMacrosName.class);
    return macrosName != null ? macrosName.getText() : null;
  }

  private static final class Condition {
    private final String macroName;
    private final boolean ifdef;
    private boolean inElse;
    private boolean undefined;

    private Condition(@Nullable String macroName, boolean ifdef) {
      this.macroName = macroName;
      this.ifdef = ifdef;
    }
  }
}
