/*
 * Copyright 2012-2013 Sergey Ignatov
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
import com.intellij.codeInspection.ProblemDescriptorBase;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

public class ErlangMacroSubstitutionProblemDescriptor extends ProblemDescriptorBase {
  private final PsiElement myRealProblemElement;

  public ErlangMacroSubstitutionProblemDescriptor(@NotNull PsiElement realProblemElement,
                                                  @NotNull PsiElement macroElement,
                                                  @NotNull String descriptionTemplate,
                                                  boolean onTheFly,
                                                  LocalQuickFix[] fixes,
                                                  @NotNull ProblemHighlightType highlightType) {
    super(macroElement, macroElement, "Problem after macro substitution. " + descriptionTemplate,
      fixes, highlightType, false, null, true, onTheFly);
    myRealProblemElement = realProblemElement;
  }

  public PsiElement getRealProblemElement() {
    return myRealProblemElement;
  }
}
