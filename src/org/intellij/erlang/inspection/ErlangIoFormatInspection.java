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
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangIoFormatInspection extends ErlangInspectionBase {
  private static final Pattern CONTROL_SEQUENCE_PATTERN;
  static {
    String fieldWidth =                 "(?:(\\*)|(?:-?\\d+))?";
    String precision =                  "(?:\\.(?:(\\*)|(?:\\d+))?)";
    String oneOfErlangEscapeSequences = "(?:\\\\(?:[bdefnrstv'\"\\\\]|(?:[0-7]{1,3})|(?:x[0-9a-fA-F]{2})|(?:x\\{[0-9a-fA-F]+\\})|(?:\\^[a-zA-Z])))";
    String paddingCharacter =           "(?:\\.(?:(\\*)|" + oneOfErlangEscapeSequences + "|.))?";
    String controlSequenceModifier =    "[tl]?";
    String controlSequenceType =        "(?:([cfegswpWPBX#bx\\+i])|([~n]))";
    CONTROL_SEQUENCE_PATTERN = Pattern.compile("~" + fieldWidth + "(?:" + precision + paddingCharacter + ")?" + controlSequenceModifier + controlSequenceType);
  }

  private final Set<String> MODULE_NAMES = ContainerUtil.set("io", "io_lib");
  private final Set<String> FUNCTION_NAMES = ContainerUtil.set("format", "fwrite");

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
                if (str instanceof ErlangStringLiteral && strLen >= 2) {
                  String formatString = str.getText().substring(1, strLen - 1);
                  int expectedArgumentsCount;
                  try {
                    expectedArgumentsCount = getExpectedFormatArgsCount(formatString);
                  } catch (InvalidControlSequenceException e) {
                    int start = e.getInvalidSequenceStartIdx() + 1;
                    problemsHolder.registerProblem(str, TextRange.create(start, str.getTextLength() - 1), "Invalid control sequence");
                    return;
                  }
                  if (args instanceof ErlangListExpression) {
                    int passedArgumentsCount = ((ErlangListExpression) args).getExpressionList().size();
                    if (expectedArgumentsCount != passedArgumentsCount) {
                      problemsHolder.registerProblem(str, "Wrong number of arguments in format call, should be " + expectedArgumentsCount);
                    }
                  }
                }
              }
            }
          }
        }
      }
    });
  }

  private static int getExpectedFormatArgsCount(String formatString) throws InvalidControlSequenceException {
    int expectedArgumentsCount = 0;
    int previousMatchEnd = 0;
    Matcher matcher = CONTROL_SEQUENCE_PATTERN.matcher(formatString);
    while (matcher.find()) {
      checkNoControlSequencePresent(formatString, previousMatchEnd, matcher.start());
      for (int i = 1; i < 5; i++) {
        if (matcher.group(i) != null) expectedArgumentsCount++;
      }
      if (matcher.group(4) == null && matcher.group(5) == null) {
        throw new InvalidControlSequenceException(matcher.start());
      }
      previousMatchEnd = matcher.end();
    }
    checkNoControlSequencePresent(formatString, previousMatchEnd, formatString.length());
    return expectedArgumentsCount;
  }

  private static void checkNoControlSequencePresent(String formatString, int begin, int end) throws InvalidControlSequenceException {
    int controlSequenceStart = StringUtil.indexOf(formatString, '~', begin, end);
    if (controlSequenceStart != -1) {
      throw new InvalidControlSequenceException(controlSequenceStart);
    }
  }

  private static class InvalidControlSequenceException extends Exception {
    private int myInvalidSequenceStartIdx;

    InvalidControlSequenceException(int invalidSequenceStartIdx) {
      myInvalidSequenceStartIdx = invalidSequenceStartIdx;
    }

    private int getInvalidSequenceStartIdx() {
      return myInvalidSequenceStartIdx;
    }
  }
}
