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

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.macrolayer.ErlangMacroLayer;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangSpecification;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFix;
import org.intellij.erlang.quickfixes.ErlangRemoveFunctionFix;
import org.jetbrains.annotations.NotNull;

import java.util.Set;

public class ErlangUnusedFunctionInspection extends ErlangInspectionBase {
  public static final String SHORT_NAME = "ErlangUnusedFunction";

  @Override
  protected boolean canRunOn(@NotNull ErlangFile file) {
    return !file.getName().endsWith(ErlangFileType.HEADER.getDefaultExtension()) && !file.isExportedAll();
  }

  @Override
  protected void checkFile(@NotNull ErlangFile file, @NotNull ProblemsHolder holder) {
    for (ErlangFunction function : unusedFunctions(file)) {
      PsiElement identifier = function.getNameIdentifier();
      String message = "Unused function " + "'" + function.getName() + "/" + function.getArity() + "'";
      LocalQuickFix[] fixes = {new ErlangRemoveFunctionFix(), new ErlangExportFunctionFix(function)};

      holder.registerProblem(identifier, message, ProblemHighlightType.LIKE_UNUSED_SYMBOL, fixes);
    }
  }

  public boolean isReported(@NotNull ErlangFile file, @NotNull ErlangFunction function) {
    return canRunOn(file) && !isSuppressedFor(function.getNameIdentifier()) && unusedFunctions(file).contains(function);
  }

  // a function is used when a call or a fun of it is reachable from the roots: the exports, -on_load and the other
  // attributes, and the functions that are never reported or are suppressed; uses in specs and by the function itself do not count
  private @NotNull Set<ErlangFunction> unusedFunctions(@NotNull ErlangFile file) {
    return CachedValuesManager.getCachedValue(file, () -> {
      Set<String> outOfSight = ErlangUnusedCode.namesUsedOutOfSight(file);
      return CachedValueProvider.Result.create(
        ErlangUnusedCode.unreachable(file, ContainerUtil.filter(file.getFunctions(), f -> isChecked(file, f) && !isSuppressedFor(f.getNameIdentifier())), ErlangFunction.class,
                                     element -> PsiTreeUtil.getParentOfType(element, ErlangSpecification.class) != null,
                                     f -> outOfSight.contains(f.getName())),
        PsiModificationTracker.MODIFICATION_COUNT, ErlangMacroLayer.modeTracker());
    });
  }

  private static boolean isChecked(@NotNull ErlangFile file, @NotNull ErlangFunction function) {
    // with the macro layer: a "function" named by a macro use that continues a form the recovery split
    // (guarded3(X) ?HH2(X) -> X.) is no function of the source; ?test(A) -> A. after a whole form still is one
    if (ErlangMacroLayer.isEnabled() && function.getName().startsWith("?") && ErlangMacroLayer.continuesASplitForm(function)) return false;
    // the rest of a split form (setup?PAR, hello().) when a macro makes hello/0: the references go to the made one
    if (ErlangMacroLayer.isEnabled() && ErlangMacroLayer.isHiddenByMadeFunction(function)) return false;
    // or when the source has hello/0 (hello() -> ok.): the references go to that one
    if (ErlangMacroLayer.preferSourceFunction(file, function) != function) return false;
    return !(ErlangPsiImplUtil.isEunitImported(file) && ErlangPsiImplUtil.isEunitTestFunction(function));
  }
}
