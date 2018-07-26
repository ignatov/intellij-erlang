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

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

class ErlangCompletionUtil {
  private ErlangCompletionUtil() {
  }

  @NotNull
  static Set<ErlangExpressionType> expectedArgumentTypes(@NotNull PsiElement elementInCallArgument) {
    ErlangExpression expr = PsiTreeUtil.getParentOfType(elementInCallArgument, ErlangExpression.class);
    ErlangArgumentList argList = ObjectUtils.tryCast(expr != null ? expr.getParent() : null, ErlangArgumentList.class);
    PsiElement argListOwner = argList != null ? argList.getParent() : null;
    ErlangFunctionCallExpression call = ObjectUtils.tryCast(argListOwner, ErlangFunctionCallExpression.class);
    int argIndex = call != null ? ContainerUtil.indexOfIdentity(argList.getExpressionList(), expr) : -1;
    return argIndex != -1 ? expectedArgumentTypes(call, argIndex) : Collections.emptySet();
  }

  static boolean containsType(@NotNull Set<ErlangExpressionType> typeSet, @NotNull ErlangExpressionType type) {
    for (ErlangExpressionType setItem : typeSet) {
      if (setItem.accept(type)) return true;
    }
    return false;
  }

  @NotNull
  private static Set<ErlangExpressionType> expectedArgumentTypes(@NotNull ErlangFunctionCallExpression call, int argIndex) {
    PsiPolyVariantReference reference = (PsiPolyVariantReference) call.getReference();
    if (reference == null) return Collections.emptySet();

    HashSet<ErlangExpressionType> expectedArgumentTypes = ContainerUtil.newHashSet();
    ResolveResult[] resolveResults = reference.multiResolve(true);
    for (ResolveResult r : resolveResults) {
      ErlangFunction function = ObjectUtils.tryCast(r.getElement(), ErlangFunction.class);
      ErlangSpecification spec = function != null ? function.findSpecification() : null;
      ErlangFunTypeSigs signature = ErlangPsiImplUtil.getSignature(spec);
      List<ErlangTypeSig> sigs = signature != null ? signature.getTypeSigList() : ContainerUtil.emptyList();
      for (ErlangTypeSig sig : sigs) {
        ErlangFunTypeArguments arguments = sig.getFunType().getFunTypeArguments();
        ErlangType type = arguments.getTypeList().get(argIndex);
        if (type != null) {
          collectExpressionTypes(type, expectedArgumentTypes);
        }
      }
    }

    return expectedArgumentTypes;
  }

  private static void collectExpressionTypes(@NotNull ErlangType type, @NotNull Set<ErlangExpressionType> types) {
    for (ErlangType childType : PsiTreeUtil.getChildrenOfTypeAsList(type, ErlangType.class)) {
      collectExpressionTypes(childType, types);
    }
    ErlangTypeRef typeRef = type.getTypeRef();
    String key = typeRef != null ? typeRef.getText() : type.getFirstChild().getText();
    ErlangExpressionType et = ErlangExpressionType.TYPE_MAP.get(key);
    ContainerUtil.addIfNotNull(types, et);
  }
}
