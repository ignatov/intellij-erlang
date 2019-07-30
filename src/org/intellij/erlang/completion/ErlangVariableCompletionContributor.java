/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ProcessingContext;
import kotlin.reflect.jvm.internal.impl.utils.SmartList;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangVarProcessor;
import org.intellij.erlang.psi.impl.ResolveUtil;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangVariableCompletionContributor extends CompletionContributor implements DumbAware {
  public ErlangVariableCompletionContributor() {
    extend(CompletionType.BASIC, psiElement(ErlangTypes.ERL_VAR), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters,
                                    @NotNull ProcessingContext context,
                                    @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        ErlangFile file = (ErlangFile)position.getContainingFile();

        ErlangQVar originalVar = PsiTreeUtil.getParentOfType(parameters.getOriginalPosition(), ErlangQVar.class);
        if (originalVar != null) {
          boolean isDeclaration = inArgumentDefinition(originalVar) || inLeftPartOfAssignment(originalVar);
          PsiReference reference = !isDeclaration ? originalVar.getReference() : null;
          PsiElement resolved = reference != null ? reference.resolve() : null;
          if (isDeclaration || resolved != null && resolved != originalVar) {
            addVariable(result, originalVar.getName());
          }
        }

        if (!(position.getParent() instanceof ErlangRecordExpression)) {
          Collection<String> vars = new HashSet<>();

          PsiElement scopeOwner = PsiTreeUtil.getParentOfType(position,
            ErlangFunctionClause.class, ErlangMacrosDefinition.class, ErlangTypeDefinition.class, ErlangSpecification.class);
          ResolveUtil.treeWalkUp(position, new MyBaseScopeProcessor(vars, position, scopeOwner, false));

          ErlangModule module = file.getModule();
          if (module != null) {
            module.processDeclarations(new MyBaseScopeProcessor(vars, position, scopeOwner, true), ResolveState.initial(), module, module);
          }

          addVariables(result, vars);
        }

        Map<String, ErlangQVar> erlangVarContext = file.getOriginalFile().getUserData(ErlangVarProcessor.ERLANG_VARIABLE_CONTEXT);
        if (erlangVarContext != null && PsiTreeUtil.getParentOfType(position, ErlangColonQualifiedExpression.class) == null) {
          addVariables(result, erlangVarContext.keySet());
        }
      }
    });
    extend(CompletionType.SMART, psiElement(ErlangTypes.ERL_VAR), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters,
                                    @NotNull ProcessingContext context,
                                    @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        Set<ErlangExpressionType> expectedTypes = ErlangCompletionUtil.expectedArgumentTypes(position);
        if (expectedTypes.isEmpty()) return;

        Collection<ErlangQVar> vars = new HashSet<>();
        ErlangFunctionClause clause = PsiTreeUtil.getParentOfType(position, ErlangFunctionClause.class);
        ResolveUtil.treeWalkUp(position, new MyBaseScopeProcessor(vars, position, clause));

        for (ErlangQVar v : vars) {
          if (!inLeftPartOfAssignment(v, true)) continue;

          ErlangAssignmentExpression assign = PsiTreeUtil.getParentOfType(v, ErlangAssignmentExpression.class);
          ErlangExpression right = assign != null ? assign.getRight() : null;
          ErlangExpressionType varType = ErlangExpressionType.create(right);

          if (ErlangCompletionUtil.containsType(expectedTypes, varType)) {
            addVariable(result, v.getName());
          }
        }
      }
    });
  }

  private static void addVariables(@NotNull CompletionResultSet result, @NotNull Iterable<String> variables) {
    for (String variable : variables) {
      addVariable(result, variable);
    }
  }

  private static void addVariable(@NotNull CompletionResultSet result, @NotNull String variable) {
    result.addElement(LookupElementBuilder.create(variable).withIcon(ErlangIcons.VARIABLE));
  }

  private static class MyBaseScopeProcessor implements PsiScopeProcessor {
    private final PsiElement myElement;
    private final PsiElement myScopeOwner;
    private final boolean myForce;
    private final Collection<String> myResult;
    @Nullable private Collection<ErlangQVar> myVars;

    public MyBaseScopeProcessor(@NotNull Collection<String> result, @NotNull PsiElement element, @Nullable PsiElement scopeOwner, boolean force) {
      myElement = element;
      myScopeOwner = scopeOwner;
      myForce = force;
      myResult = result;
    }

    private MyBaseScopeProcessor(@NotNull Collection<ErlangQVar> result, @NotNull PsiElement element, @Nullable PsiElement scopeOwner) {
      this(new SmartList<>(), element, scopeOwner, true);
      myVars = result;
    }

    @Override
    public boolean execute(@NotNull PsiElement psiElement, @NotNull ResolveState resolveState) {
      if (!psiElement.equals(myElement) && psiElement instanceof ErlangQVar && !psiElement.getText().equals("_") && !inColonQualified(myElement)) {
        boolean ancestor = PsiTreeUtil.isAncestor(myScopeOwner, psiElement, false);
        if ((ancestor || myForce) && (inArgumentDefinition(psiElement) || inLeftPartOfAssignment(psiElement) || inFunctionTypeArgument(psiElement))) {
          myResult.add(((ErlangQVar) psiElement).getName());
          if (myVars != null) myVars.add((ErlangQVar) psiElement);
        }
      }
      return true;
    }
  }
}
