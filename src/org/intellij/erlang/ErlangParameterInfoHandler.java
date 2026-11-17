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
import com.intellij.util.ObjectUtils;
import com.intellij.util.SlowOperations;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class ErlangParameterInfoHandler implements ParameterInfoHandler<ErlangArgumentList, Object> {
  @Override
  public ErlangArgumentList findElementForParameterInfo(@NotNull CreateParameterInfoContext context) {
    ErlangArgumentList args = getErlangArgumentList(context);
    List<ErlangMacrosDefinition> macrosDefinitions = args != null ? getCalledMacrosDefinitions(args) : Collections.emptyList();
    if (!macrosDefinitions.isEmpty()) {
      context.setItemsToShow(ArrayUtil.toObjectArray(macrosDefinitions));
    }
    return args;
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

  @NotNull
  private static List<ErlangMacrosDefinition> getCalledMacrosDefinitions(@NotNull ErlangArgumentList args) {
    PsiElement parent = args.getParent();
    if (!(parent instanceof ErlangFunctionCallExpression || parent instanceof ErlangGenericFunctionCallExpression)) return Collections.emptyList();
    PsiElement callee = PsiTreeUtil.skipWhitespacesAndCommentsBackward(args);
    ErlangMacros macros = callee instanceof ErlangQAtom ? ((ErlangQAtom) callee).getMacros() : ObjectUtils.tryCast(callee, ErlangMacros.class);
    PsiReference reference = macros != null ? macros.getReference() : null;
    PsiElement resolve = reference != null ? reference.resolve() : null;
    if (!(resolve instanceof ErlangMacrosDefinition) || !(resolve.getContainingFile() instanceof ErlangFile)) return Collections.emptyList();
    ErlangMacrosDefinition resolved = (ErlangMacrosDefinition) resolve;
    ErlangFile file = (ErlangFile) resolve.getContainingFile();
    String name = resolved.getName();
    ConditionalBranches branches = file == args.getContainingFile() ? new ConditionalBranches(file, name, args.getTextOffset()) : null;
    Map<Integer, ErlangMacrosDefinition> definitionsByArity = new TreeMap<>();
    for (ErlangMacrosDefinition definition : file.getMacroses()) {
      ErlangArgumentDefinitionList argumentDefinitionList = definition.getArgumentDefinitionList();
      if (argumentDefinitionList == null || !name.equals(definition.getName())) continue;
      if (branches != null && branches.isUndefined(definition)) continue;
      int arity = argumentDefinitionList.getArgumentDefinitionList().size();
      if (definition == resolved) definitionsByArity.put(arity, definition);
      else definitionsByArity.putIfAbsent(arity, definition);
    }
    return new ArrayList<>(definitionsByArity.values());
  }

  /**
   * Tracks the -if/-ifdef/-ifndef branches open at each point of a file before a call, and the -undef of one macro there.
   * An -undef removes an earlier definition when every branch open at the -undef is also open at the definition or at the call,
   * so it runs whenever one of them is compiled.
   */
  private static class ConditionalBranches {
    private final TreeMap<Integer, List<Integer>> myOpenBranchesAfter = new TreeMap<>();
    private final Map<Integer, List<Integer>> myUndefs = new TreeMap<>();
    private final List<Integer> myCallBranches;

    ConditionalBranches(@NotNull ErlangFile file, @NotNull String macroName, int callOffset) {
      List<Integer> open = new ArrayList<>();
      int nextBranch = 0;
      for (ErlangAttribute attribute : file.getAttributes()) {
        int offset = attribute.getTextOffset();
        if (offset >= callOffset) break;
        ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
        String directive = atomAttribute != null ? atomAttribute.getName() :
                           attribute.getElseAtomAttribute() != null ? "else" :
                           attribute.getNode().findChildByType(ErlangTypes.ERL_IF) != null ? "if" : "";
        switch (directive) {
          case "if", "ifdef", "ifndef" -> open.add(nextBranch++);
          case "else", "elif" -> {
            if (!open.isEmpty()) open.set(open.size() - 1, nextBranch++);
          }
          case "endif" -> {
            if (!open.isEmpty()) open.remove(open.size() - 1);
          }
          case "undef" -> {
            ErlangMacrosName undefName = PsiTreeUtil.getChildOfType(atomAttribute, ErlangMacrosName.class);
            if (undefName != null && macroName.equals(ErlangPsiImplUtil.getNameIdentifier(undefName).getText())) {
              myUndefs.put(offset, List.copyOf(open));
            }
          }
          default -> {}
        }
        myOpenBranchesAfter.put(offset, List.copyOf(open));
      }
      myCallBranches = openBranchesAt(callOffset);
    }

    boolean isUndefined(@NotNull ErlangMacrosDefinition definition) {
      int definitionEnd = definition.getTextRange().getEndOffset();
      List<Integer> definitionBranches = openBranchesAt(definition.getTextRange().getStartOffset());
      for (Map.Entry<Integer, List<Integer>> undef : myUndefs.entrySet()) {
        if (undef.getKey() < definitionEnd) continue;
        List<Integer> undefBranches = undef.getValue();
        if (isPrefix(undefBranches, definitionBranches) || isPrefix(undefBranches, myCallBranches)) return true;
      }
      return false;
    }

    @NotNull
    private List<Integer> openBranchesAt(int offset) {
      Map.Entry<Integer, List<Integer>> entry = myOpenBranchesAfter.lowerEntry(offset);
      return entry != null ? entry.getValue() : Collections.emptyList();
    }

    private static boolean isPrefix(@NotNull List<Integer> prefix, @NotNull List<Integer> list) {
      return prefix.size() <= list.size() && prefix.equals(list.subList(0, prefix.size()));
    }
  }

  @Override
  public void showParameterInfo(@NotNull ErlangArgumentList args, @NotNull CreateParameterInfoContext context) {
    Object[] macrosDefinitions = context.getItemsToShow();
    if (macrosDefinitions != null && macrosDefinitions.length > 0) {
      context.showHint(args, args.getTextRange().getStartOffset(), this);
      return;
    }
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
    else if (p instanceof ErlangMacrosDefinition) {
      ErlangArgumentDefinitionList argumentDefinitionList = ((ErlangMacrosDefinition) p).getArgumentDefinitionList();
      List<ErlangArgumentDefinition> args = argumentDefinitionList != null ? argumentDefinitionList.getArgumentDefinitionList() : Collections.emptyList();
      for (int i = 0; i < args.size(); i++) {
        if (i != 0) builder.append(", ");
        if (index == i) start = builder.length();
        builder.append(args.get(i).getText().trim());
        if (index == i) end = builder.length();
      }
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

    if (builder.isEmpty()) {
      builder.append("<no parameters>");
    }

    context.setupUIComponentPresentation(builder.toString(), start, end, disabled, false, true,
      context.getDefaultParameterColor());
  }
}
