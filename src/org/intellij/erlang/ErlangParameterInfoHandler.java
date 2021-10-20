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

package org.intellij.erlang;

import com.intellij.lang.parameterInfo.*;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.SlowOperations;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ErlangParameterInfoHandler implements ParameterInfoHandler<ErlangArgumentList, Object> {
  @Override
  public ErlangArgumentList findElementForParameterInfo(@NotNull CreateParameterInfoContext context) {
    return getErlangArgumentList(context);
  }

  @Override
  public ErlangArgumentList findElementForUpdatingParameterInfo(@NotNull UpdateParameterInfoContext context) {
    return getErlangArgumentList(context);
  }

  @Nullable
  private static ErlangArgumentList getErlangArgumentList(ParameterInfoContext context) {
    PsiElement at = context.getFile().findElementAt(context.getOffset());
    return PsiTreeUtil.getParentOfType(at, ErlangArgumentList.class);
  }

  @Override
  public void showParameterInfo(@NotNull ErlangArgumentList args, @NotNull CreateParameterInfoContext context) {
    ErlangFunctionCallExpression erlFunctionCall = PsiTreeUtil.getParentOfType(args, ErlangFunctionCallExpression.class);
    if (erlFunctionCall != null) {
      PsiReference reference = erlFunctionCall.getReference();
      List<ErlangFunctionClause> clauses = new ArrayList<>();
      if (reference instanceof PsiPolyVariantReference) {
        ResolveResult[] resolveResults = SlowOperations.allowSlowOperations(() -> ((PsiPolyVariantReference) reference).multiResolve(true)) ;
        for (ResolveResult result : resolveResults) {
          PsiElement element = result.getElement();
          if (element instanceof ErlangFunction) {
            clauses.addAll(((ErlangFunction) element).getFunctionClauseList());
          }
        }
      }
      if (clauses.isEmpty()) {
        PsiElement resolve = SlowOperations.allowSlowOperations(reference::resolve);
        if (resolve instanceof ErlangFunction) {
          List<ErlangFunctionClause> clauseList = ((ErlangFunction) resolve).getFunctionClauseList();
          clauses.addAll(clauseList);
        }
      }
      if (!clauses.isEmpty()) {
        clauses.sort((lhs, rhs) -> {
          int lhsSize = lhs.getArgumentDefinitionList().getArgumentDefinitionList().size();
          int rhsSize = rhs.getArgumentDefinitionList().getArgumentDefinitionList().size();
          return Integer.signum(lhsSize - rhsSize);
        });
        context.setItemsToShow(ArrayUtil.toObjectArray(clauses));
      }
      else {
        ErlangGlobalFunctionCallExpression erlGlobalFunctionCall = PsiTreeUtil.getParentOfType(erlFunctionCall, ErlangGlobalFunctionCallExpression.class);
        if (erlGlobalFunctionCall != null) {
          ErlangModuleRef moduleRef = erlGlobalFunctionCall.getModuleRef();
          String moduleName = moduleRef.getText();
          String functionName = erlFunctionCall.getName();
          List<ErlangBifDescriptor> moduleInfo = functionName.equals(ErlangBifTable.MODULE_INFO) ? ErlangBifTable.getBifs("", functionName) : Collections.emptyList();
          context.setItemsToShow(ArrayUtil.toObjectArray(ContainerUtil.concat(ErlangBifTable.getBifs(moduleName, functionName), moduleInfo)));
        }
        else {
          String name = erlFunctionCall.getName();
          context.setItemsToShow(ArrayUtil.toObjectArray(ContainerUtil.concat(ErlangBifTable.getBifs("erlang", name), ErlangBifTable.getBifs("", name))));
        }
      }
      context.showHint(args, args.getTextRange().getStartOffset(), this);
    }
  }

  @Override
  public void updateParameterInfo(@NotNull ErlangArgumentList place, @NotNull UpdateParameterInfoContext context) {
    context.setCurrentParameter(ParameterInfoUtils.getCurrentParameterIndex(place.getNode(), context.getOffset(), ErlangTypes.ERL_COMMA));
  }

  @Override
  public void updateUI(@Nullable Object p, @NotNull ParameterInfoUIContext context) {
    if (p == null) {
      context.setUIComponentEnabled(false);
      return;
    }
    int index = context.getCurrentParameterIndex();

    StringBuilder builder = new StringBuilder();

    boolean disabled = false;
    int start = 0;
    int end = 0;
    if (p instanceof ErlangFunctionClause) {
      final Ref<ErlangFunTypeArguments> argsRef = Ref.create();

      PsiElement parent = ((ErlangFunctionClause) p).getParent();
      ErlangSpecification specification = parent instanceof ErlangFunction ? ((ErlangFunction) parent).findSpecification() : null;
      if (specification != null) {
        specification.accept(new ErlangRecursiveVisitor() {
          @Override
          public void visitFunTypeArguments(@NotNull ErlangFunTypeArguments o) {
            argsRef.setIfNull(o);
          }
        });
      }

      List<ErlangArgumentDefinition> args = ((ErlangFunctionClause) p).getArgumentDefinitionList().getArgumentDefinitionList();

      ErlangFunTypeArguments arguments = argsRef.get();
      List<ErlangType> typeList = arguments == null ? ContainerUtil.emptyList() : arguments.getTypeList();
      boolean typesAvailable = typeList.size() == args.size();

      for (int i = 0; i < args.size(); i++) {
        if (i != 0) builder.append(", ");
        if (index == i) start = builder.length();
        builder.append(args.get(i).getExpression().getText().replaceAll(" ", "").trim());
        if (typesAvailable) {
          ErlangType type = typeList.get(i);
          final ErlangQVar var = type != null ? type.getQVar() : null;
          if (var != null) {
            if (specification != null) {
              final Ref<ErlangType> itemTypeRef = Ref.create();
              specification.accept(new ErlangRecursiveVisitor() {
                @Override
                public void visitTypeGuard(@NotNull ErlangTypeGuard o) {
                  ErlangType item = ContainerUtil.getFirstItem(o.getTypeList());
                  ErlangQVar qVar = item == null ? null : item.getQVar();
                  PsiReference reference = qVar == null ? null : qVar.getReference();
                  PsiElement resolve = reference == null ? null : reference.resolve();
                  if (var.equals(resolve)) {
                    itemTypeRef.setIfNull(item.getType());
                  }
                }
              });
              if (!itemTypeRef.isNull()) {
                builder.append(" :: ");
                builder.append(itemTypeRef.get().getText());
              }
            }
          }
          else if (type != null) {
            builder.append(" :: ");
            builder.append(type.getText());
          }
        }
        if (index == i) end = builder.length();
      }

      ErlangClauseGuard clauseGuard = ((ErlangFunctionClause) p).getClauseGuard();

      String text = clauseGuard != null ? " " + clauseGuard.getText() : "";
      builder.append(text);

      disabled = index >= args.size();
    }
    else if (p instanceof ErlangBifDescriptor) {
      String bifParams = ((ErlangBifDescriptor) p).getParams();
      builder.append(bifParams);
      for (int i = 0; i < index && start != -1; ++i) {
        start = bifParams.indexOf(',', start + 1);
      }
      if (start == -1) {
        disabled = true;
      }
      else {
        end = bifParams.indexOf(',', start + 1);
        end = end == -1 ? bifParams.length() : end;
      }
    }

    if (builder.length() == 0) {
      builder.append("<no parameters>");
    }

    context.setupUIComponentPresentation(builder.toString(), start, end, disabled, false, true,
      context.getDefaultParameterColor());
  }
}
