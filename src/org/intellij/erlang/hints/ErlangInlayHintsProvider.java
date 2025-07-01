/*
 * Copyright 2012-2025 Sergey Ignatov
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

package org.intellij.erlang.hints;

import com.intellij.codeInsight.hints.InlayInfo;
import com.intellij.codeInsight.hints.InlayParameterHintsProvider;
import com.intellij.codeInsight.hints.Option;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.ErlangExpression;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangFunctionClause;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Unmodifiable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@SuppressWarnings("UnstableApiUsage")
public class ErlangInlayHintsProvider implements InlayParameterHintsProvider {
  public static final Option SHOW_ALL_PARAMS_OPTION = new Option(
    "erlang.inlay.show.all.clause.params",
    () -> "Show all parameter names from all function clauses",
    true
  );
  
  @Override
  public @NotNull @Unmodifiable List<Option> getSupportedOptions() {
    return Collections.singletonList(SHOW_ALL_PARAMS_OPTION);
  }
  
  @Override
  public @NotNull List<InlayInfo> getParameterHints(@NotNull PsiElement element) {
    if (!(element instanceof ErlangFunctionCallExpression call)) {
      return Collections.emptyList();
    }

    List<ErlangExpression> arguments = call.getArgumentList().getExpressionList();
    
    if (arguments.isEmpty()) {
      return Collections.emptyList();
    }

    // Get information about the target function
    var reference = element.getReference();

    var resolve = reference.resolve();
    if (!(resolve instanceof ErlangFunction function)) return Collections.emptyList();

    var functionClauses = function.getFunctionClauseList();
    if (functionClauses.isEmpty()) return Collections.emptyList();

    // Find clauses with matching arity
    List<ErlangFunctionClause> matchingClauses = functionClauses.stream()
                            .filter(c -> c.getArgumentDefinitionList().getArgumentDefinitionList().size() == arguments.size())
                            .toList();

    if (matchingClauses.isEmpty()) return Collections.emptyList();
    
    List<InlayInfo> hints = new ArrayList<>(arguments.size());
    
    // Check if we need to show all clause parameters
    boolean showAllParams = SHOW_ALL_PARAMS_OPTION.get();
    
    if (showAllParams && matchingClauses.size() > 1) {
      // Show all parameter names from all clauses
      for (int argIndex = 0; argIndex < arguments.size(); argIndex++) {
        final int i = argIndex; // Create a final local variable for use in lambda
        ErlangExpression arg = arguments.get(i);
        
        // Collect parameter names from all matching clauses for this position
        List<String> paramNames = matchingClauses.stream()
            .map(clause -> clause.getArgumentDefinitionList().getArgumentDefinitionList().get(i).getExpression().getText())
            .distinct() // Remove duplicates
            .collect(Collectors.toList());
            
        if (!paramNames.isEmpty()) {
          // Join parameter names with separator
          String displayName = String.join("|", paramNames);
          hints.add(new InlayInfo(displayName, arg.getTextRange().getStartOffset()));
        }
      }
    } else {
      // Default behavior - show just the first clause
      ErlangFunctionClause clause = matchingClauses.getFirst();
      var parameterList = clause.getArgumentDefinitionList().getArgumentDefinitionList();
      
      if (arguments.size() != parameterList.size()) {
        return Collections.emptyList();
      }
      
      for (int i = 0; i < arguments.size(); i++) {
        ErlangExpression arg = arguments.get(i);
        String paramName = parameterList.get(i).getExpression().getText();
        
        hints.add(new InlayInfo(paramName, arg.getTextRange().getStartOffset()));
      }
    }
    
    return hints;
  }

  @Override
  public @NotNull Set<String> getDefaultBlackList() {
    return Collections.emptySet();
  }
}