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
  private static final Pattern PATTERN = Pattern.compile("\\n\\s*\"\"\"$");

  @Override
  protected @NotNull ErlangVisitor buildErlangVisitor(@NotNull ProblemsHolder holder,
                                                      @NotNull LocalInspectionToolSession session) {
    return new ErlangVisitor() {
      @Override
      public void visitStringLiteral(@NotNull ErlangStringLiteral o) {
        String text = o.getText();
        if (text.startsWith("\"\"\"") && !text.startsWith("\"\"\"\n")) {
          holder.registerProblem(o, "Not white space after start of triple-quoted string");
        }
        if (text.endsWith("\"\"\"") && !endsWithPattern(text)) {
          holder.registerProblem(o, "Bad indentation in triple-quoted string");
        }
      }
    };
  }

  private static boolean endsWithPattern(String tripleQuotedString) {
    Matcher matcher = PATTERN.matcher(tripleQuotedString);
    return matcher.find();
  }
}
