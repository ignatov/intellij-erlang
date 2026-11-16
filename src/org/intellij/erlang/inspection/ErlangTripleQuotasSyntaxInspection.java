/*
 * Copyright 2012-2024 Sergey Ignatov
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
import org.intellij.erlang.psi.ErlangStringLiteral;
import org.intellij.erlang.psi.ErlangVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangTripleQuotasSyntaxInspection extends ErlangInspectionBase {
  @Override
  protected @NotNull ErlangVisitor buildErlangVisitor(@NotNull ProblemsHolder holder,
                                                      @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
        String text = o.getText();
        int start = openingQuotesStart(text);
        String quotes = "\"".repeat(Math.max(3, countOpeningQuotes(text)));
        if (text.startsWith(quotes, start) && !text.startsWith(quotes + "\n", start)) {
          holder.registerProblem(o, "Not white space after start of triple-quoted string");
        }
        if (text.endsWith(quotes) && !endsWithPattern(text, quotes)) {
          holder.registerProblem(o, "Bad indentation in triple-quoted string");
        }
      }
    };
  }

  // A triple-quoted string or sigil may open with more than three quotes and then ends at the same number (EEP 64)
  private static int countOpeningQuotes(String text) {
    int start = openingQuotesStart(text);
    int end = start;
    while (end < text.length() && text.charAt(end) == '"') end++;
    return end - start;
  }

  private static int openingQuotesStart(String text) {
    return text.startsWith("~") ? (text.length() > 1 && text.charAt(1) != '"' ? 2 : 1) : 0;
  }

  private static boolean endsWithPattern(String tripleQuotedString, String quotes) {
    Matcher matcher = Pattern.compile("\\n\\s*" + quotes + "$").matcher(tripleQuotedString);
    return matcher.find();
  }
}
