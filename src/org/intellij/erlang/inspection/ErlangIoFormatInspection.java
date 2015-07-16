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
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiReference;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//TODO make this inspection work when using -import attribute for io, io_lib
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

  @NotNull
  @Override
  protected ErlangVisitor buildErlangVisitor(@NotNull final ProblemsHolder holder, @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitGlobalFunctionCallExpression(@NotNull ErlangGlobalFunctionCallExpression o) {
        ErlangFunctionCallExpression expression = o.getFunctionCallExpression();
        List<ErlangExpression> expressionList = expression.getArgumentList().getExpressionList();
        int size = expressionList.size();

        if (size < 2) return;

        ErlangModule module = ObjectUtils.tryCast(o.getModuleRef().getReference().resolve(), ErlangModule.class);
        if (module == null || !MODULE_NAMES.contains(module.getName())) return;

        PsiReference ref = expression.getReference();
        ErlangFunction function = ObjectUtils.tryCast(ref != null ? ref.resolve() : null, ErlangFunction.class);
        if (function == null || !FUNCTION_NAMES.contains(function.getName())) return;

        List<ErlangExpression> reverse = ContainerUtil.reverse(expressionList);
        ErlangListExpression args = ObjectUtils.tryCast(reverse.get(0), ErlangListExpression.class);
        ErlangStringLiteral formatLiteral = ObjectUtils.tryCast(reverse.get(1), ErlangStringLiteral.class);
        String formatString = formatLiteral != null ? formatLiteral.getString().getText() : null;
        if (formatString == null || formatString.length() < 2) return;


        int expectedArgumentsCount;
        try {
          expectedArgumentsCount = getExpectedFormatArgsCount(formatString);
        } catch (InvalidControlSequenceException e) {
          registerProblem(holder, formatLiteral, "Invalid control sequence",
            TextRange.create(e.getInvalidSequenceStartIdx(), formatString.length() - 1), null);
          return;
        }

        int passedArgumentsCount = args != null ? args.getExpressionList().size() : -1;
        if (passedArgumentsCount >= 0 && expectedArgumentsCount != passedArgumentsCount) {
          String problemDescription = "Wrong number of arguments in format call, should be " + expectedArgumentsCount;
          registerProblem(holder, formatLiteral, problemDescription);
        }
      }
    };
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
      String controlSequenceType = matcher.group(4);
      if ("P".equals(controlSequenceType)) expectedArgumentsCount++;
      if (controlSequenceType == null && matcher.group(5) == null) {
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
