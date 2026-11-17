/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.codeInsight.hints.declarative.*;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import kotlin.Unit;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.util.*;

/**
 * Parameter name hints at Erlang call sites.
 * <p>
 * The hint for an argument is the parameter name taken from the variables of the function clauses or,
 * failing that, from the {@code -spec}. When the clauses only match on patterns (literals, records, maps...),
 * a short pattern summary is shown instead, never longer than {@link #MAX_SUMMARY_LENGTH} characters.
 */
public class ErlangParameterHintsProvider implements InlayHintsProvider {
  public static final String PROVIDER_ID = "erlang.parameter.hints";
  public static final String PATTERN_SUMMARIES_OPTION = "erlang.parameter.hints.pattern.summaries";

  @VisibleForTesting
  public static final int MAX_SUMMARY_LENGTH = 12;
  private static final String ELLIPSIS = "…";

  @Override
  public @Nullable InlayHintsCollector createCollector(@NotNull PsiFile file, @NotNull Editor editor) {
    return file instanceof ErlangFile ? new Collector() : null;
  }

  private static final class Collector implements SharedBypassCollector {
    private final Map<ErlangFunction, List<Hint>> myCache = new HashMap<>();

    @Override
    public void collectFromElement(@NotNull PsiElement element, @NotNull InlayTreeSink sink) {
      if (!(element instanceof ErlangFunctionCallExpression call)) return;
      List<ErlangExpression> arguments = call.getArgumentList().getExpressionList();
      if (arguments.isEmpty()) return;
      if (!(call.getReference().resolve() instanceof ErlangFunction function)) return;
      List<Hint> hints = myCache.computeIfAbsent(function, ErlangParameterHintsProvider::computeHints);
      if (hints.size() != arguments.size()) return;

      for (int i = 0; i < arguments.size(); i++) {
        Hint hint = hints.get(i);
        if (hint == null) continue;
        ErlangExpression argument = arguments.get(i);
        if (isRedundant(hint, argument, function, arguments.size())) continue;
        if (hint.isName()) {
          addHint(sink, argument, hint.text());
        }
        else {
          sink.whenOptionEnabled(PATTERN_SUMMARIES_OPTION, () -> {
            addHint(sink, argument, hint.text());
            return Unit.INSTANCE;
          });
        }
      }
    }

    private static void addHint(@NotNull InlayTreeSink sink, @NotNull ErlangExpression argument, @NotNull String text) {
      sink.addPresentation(new InlineInlayPosition(argument.getTextRange().getStartOffset(), false, 0), null, null,
                           HintFormat.Companion.getDefault(), builder -> {
          builder.text(text + ":", null);
          return Unit.INSTANCE;
        });
    }
  }

  /**
   * A hint for one parameter position: either a parameter name or a pattern summary.
   */
  private record Hint(@NotNull String text, boolean isName) {
  }

  private static @NotNull List<Hint> computeHints(@NotNull ErlangFunction function) {
    int arity = function.getArity();
    List<List<ErlangExpression>> clausePatterns = new ArrayList<>();
    for (ErlangFunctionClause clause : function.getFunctionClauseList()) {
      List<ErlangArgumentDefinition> definitions = clause.getArgumentDefinitionList().getArgumentDefinitionList();
      if (definitions.size() != arity) continue;
      clausePatterns.add(definitions.stream().map(ErlangArgumentDefinition::getExpression).toList());
    }
    if (clausePatterns.isEmpty()) return Collections.emptyList();

    List<String> specNames = getSpecNames(function, arity);
    List<Hint> result = new ArrayList<>(arity);
    for (int i = 0; i < arity; i++) {
      List<ErlangExpression> patterns = new ArrayList<>(clausePatterns.size());
      for (List<ErlangExpression> clause : clausePatterns) {
        patterns.add(clause.get(i));
      }
      result.add(computeHint(patterns, specNames.get(i)));
    }
    return result;
  }

  private static @Nullable Hint computeHint(@NotNull List<ErlangExpression> patterns, @Nullable String specName) {
    // a clause variable is what the code really uses; a spec name wins only over a one-letter one
    String clauseName = mostCommonVariableName(patterns);
    if (clauseName != null && (specName == null || !isTooShort(clauseName))) return new Hint(clauseName, true);
    if (specName != null) return new Hint(specName, true);
    String summary = summarize(patterns);
    return summary != null ? new Hint(summary, false) : null;
  }

  /**
   * The variable name bound by the most clauses at this position ({@code Name}, {@code Name = Pattern} or
   * {@code Pattern = Name}); the first one wins on a tie.
   */
  private static @Nullable String mostCommonVariableName(@NotNull List<ErlangExpression> patterns) {
    Map<String, Integer> counts = new LinkedHashMap<>();
    for (ErlangExpression pattern : patterns) {
      String name = variableName(pattern);
      if (name != null) counts.merge(name, 1, Integer::sum);
    }
    String best = null;
    int bestCount = 0;
    for (Map.Entry<String, Integer> entry : counts.entrySet()) {
      if (entry.getValue() > bestCount) {
        best = entry.getKey();
        bestCount = entry.getValue();
      }
    }
    return best;
  }

  private static @Nullable String variableName(@Nullable ErlangExpression pattern) {
    pattern = unwrap(pattern);
    if (pattern instanceof ErlangAssignmentExpression assignment) {
      String name = variableName(assignment.getRight());
      return name != null ? name : variableName(assignment.getLeft());
    }
    ErlangQVar var = asVariable(pattern);
    return var != null ? cleanVariableName(var.getName()) : null;
  }

  private static @Nullable ErlangQVar asVariable(@Nullable ErlangExpression expression) {
    expression = unwrap(expression);
    if (expression instanceof ErlangMaxExpression max && max.getQVar() != null && max.getExpressionList().isEmpty()) {
      return max.getQVar();
    }
    return null;
  }

  /**
   * {@code _Unused} becomes {@code Unused}; the anonymous {@code _} has no name.
   */
  private static @Nullable String cleanVariableName(@NotNull String name) {
    String trimmed = StringUtil.trimLeading(name, '_');
    return trimmed.isEmpty() ? null : trimmed;
  }

  private static @NotNull List<String> getSpecNames(@NotNull ErlangFunction function, int arity) {
    String[] names = new String[arity];
    ErlangSpecification specification = function.findSpecification();
    ErlangFunTypeSigs signature = specification != null ? specification.getSignature() : null;
    if (signature != null) {
      for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
        List<ErlangType> types = typeSig.getFunType().getFunTypeArguments().getTypeList();
        if (types.size() != arity) continue;
        for (int i = 0; i < arity; i++) {
          ErlangQVar var = types.get(i).getQVar();
          if (names[i] == null && var != null) names[i] = cleanVariableName(var.getName());
        }
      }
    }
    return Arrays.asList(names);
  }

  /**
   * A short union of the distinct patterns at this position, such as {@code true|false} or {@code #state{}}.
   * It is cut to {@link #MAX_SUMMARY_LENGTH} characters, ending in an ellipsis, so a function with hundreds of
   * literal clauses gives {@code 1|2|3|4|5|6…} rather than every value.
   */
  @VisibleForTesting
  public static @Nullable String summarize(@NotNull List<? extends ErlangExpression> patterns) {
    Set<String> variants = new LinkedHashSet<>();
    for (ErlangExpression pattern : patterns) {
      String variant = shortForm(pattern);
      if (variant != null) variants.add(variant);
    }
    if (variants.isEmpty()) return null;
    if (variants.size() == 1) {
      String single = variants.iterator().next();
      // keep the whole record name, it is what the reader looks for
      return single.startsWith("#") && !single.startsWith("#{") ? single : truncate(single);
    }
    String joined = String.join("|", variants);
    if (joined.length() <= MAX_SUMMARY_LENGTH) return joined;

    StringBuilder prefix = new StringBuilder();
    for (String variant : variants) {
      int length = prefix.isEmpty() ? variant.length() : prefix.length() + 1 + variant.length();
      if (length > MAX_SUMMARY_LENGTH - 1) break;
      if (!prefix.isEmpty()) prefix.append('|');
      prefix.append(variant);
    }
    return prefix.isEmpty() ? truncate(variants.iterator().next()) : prefix + ELLIPSIS;
  }

  private static @NotNull String truncate(@NotNull String text) {
    return text.length() <= MAX_SUMMARY_LENGTH ? text : text.substring(0, MAX_SUMMARY_LENGTH - 1) + ELLIPSIS;
  }

  private static @Nullable String shortForm(@Nullable ErlangExpression pattern) {
    pattern = unwrap(pattern);
    if (pattern == null) return null;
    if (pattern instanceof ErlangAssignmentExpression assignment) {
      String left = shortForm(assignment.getLeft());
      return left != null ? left : shortForm(assignment.getRight());
    }
    if (asVariable(pattern) != null) return null;
    if (pattern instanceof ErlangRecordExpression record && isStandalone(record) && record.getRecordRef() != null) {
      return "#" + record.getRecordRef().getText() + "{}";
    }
    if (pattern instanceof ErlangMapExpression map && isStandalone(map)) {
      return map.getMapTuple().getMapEntryList().isEmpty() ? "#{}" : "#{" + ELLIPSIS + "}";
    }
    String text = normalize(pattern.getText());
    if (text.length() <= MAX_SUMMARY_LENGTH) return text;
    if (pattern instanceof ErlangTupleExpression) return "{" + ELLIPSIS + "}";
    if (pattern instanceof ErlangListExpression) return "[" + ELLIPSIS + "]";
    if (pattern instanceof ErlangBinaryExpression) return "<<" + ELLIPSIS + ">>";
    return text;
  }

  /**
   * {@code #rec{...}} or {@code #{...}} rather than an update of another expression, such as {@code R#rec{...}}.
   */
  private static boolean isStandalone(@NotNull ErlangExpression recordOrMap) {
    return !(recordOrMap.getFirstChild() instanceof ErlangExpression);
  }

  private static @Nullable ErlangExpression unwrap(@Nullable ErlangExpression expression) {
    while (expression instanceof ErlangParenthesizedExpression parenthesized) {
      expression = parenthesized.getExpression();
    }
    return expression;
  }

  private static @NotNull String normalize(@NotNull String text) {
    return text.replaceAll("\\s+", " ").replace("( ", "(").replace(" )", ")").trim();
  }

  /**
   * Skip rules, after the Java and Kotlin parameter hints: the argument already says what the hint would say.
   */
  private static boolean isRedundant(@NotNull Hint hint, @NotNull ErlangExpression argument,
                                     @NotNull ErlangFunction function, int arity) {
    if (!hint.isName()) {
      // the argument is the pattern itself, e.g. foo(ok) for foo(ok) -> ...
      return normalize(argument.getText()).equals(hint.text());
    }
    String name = comparable(hint.text());
    // one-letter names such as X or N1 say nothing
    if (isTooShort(hint.text())) return true;
    // foo(Name) for foo(Name) -> ...
    ErlangQVar var = asVariable(argument);
    if (var != null && name.equals(comparable(var.getName()))) return true;
    // set_timeout(1000) for set_timeout(Timeout) -> ...
    return arity == 1 && comparable(function.getName()).contains(name);
  }

  private static boolean isTooShort(@NotNull String name) {
    return name.length() == 1 || name.length() == 2 && Character.isDigit(name.charAt(1));
  }

  private static @NotNull String comparable(@NotNull String name) {
    return StringUtil.toLowerCase(name.replace("_", ""));
  }
}
