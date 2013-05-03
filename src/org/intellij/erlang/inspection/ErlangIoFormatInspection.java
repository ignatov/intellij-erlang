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

import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

/**
 * @author ignatov
 */
public class ErlangIoFormatInspection extends ErlangInspectionBase {
  Set<String> MODULE_NAMES = ContainerUtil.set("io", "io_lib");
  Set<String> FUNCTION_NAMES = ContainerUtil.set("format", "fwrite");

  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;

    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitGlobalFunctionCallExpression(@NotNull ErlangGlobalFunctionCallExpression o) {
        ErlangFunctionCallExpression expression = o.getFunctionCallExpression();
        List<ErlangExpression> expressionList = expression.getArgumentList().getExpressionList();
        int size = expressionList.size();

        if (size < 2) return;

        ErlangModuleRef moduleRef = o.getModuleRef();
        PsiReference moduleReference = moduleRef != null ? moduleRef.getReference() : null;
        PsiElement resolve = moduleReference != null ? moduleReference.resolve() : null;

        if (resolve instanceof ErlangModule) {
          if (MODULE_NAMES.contains(((ErlangModule) resolve).getName())) {

            PsiReference reference = expression.getReference();
            PsiElement function = reference != null ? reference.resolve() : null;

            if (function instanceof ErlangFunction) {
              if (FUNCTION_NAMES.contains(((ErlangFunction) function).getName())) {

                List<ErlangExpression> reverse = ContainerUtil.reverse(expressionList);
                ErlangExpression args = reverse.get(0);
                ErlangExpression str = reverse.get(1);

                int strLen = str.getText().length();
                if (args instanceof ErlangListExpression &&
                  (str instanceof ErlangStringLiteral || str instanceof ErlangMaxExpression) && strLen >= 2) {
                  String substring = str.getText().substring(1, strLen - 1);

                  // todo: rewrite, see: http://www.erlang.org/doc/man/io.html#format-1
                  int doubleCount = StringUtil.getOccurrenceCount(substring, "~~");
                  int newLineCount = StringUtil.getOccurrenceCount(substring, "~n");
                  int occurrenceCount = StringUtil.getOccurrenceCount(substring, "~");
                  int totalCount = occurrenceCount - doubleCount * 2 - newLineCount;
                  int agrSize = ((ErlangListExpression) args).getExpressionList().size();
                  if (totalCount != agrSize) {
                    problemsHolder.registerProblem(str, "Wrong number of arguments in format call, should be " + agrSize);
                  }
                }
              }
            }
          }
        }
      }
    });
  }
}
