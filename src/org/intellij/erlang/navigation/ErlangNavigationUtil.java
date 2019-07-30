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

package org.intellij.erlang.navigation;

import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.NavigatablePsiElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.ErlangCallbackSpec;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangSpecFun;
import org.intellij.erlang.psi.impl.ErlangCompositeElementImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.List;

public class ErlangNavigationUtil {

  private ErlangNavigationUtil() {
  }

  @NotNull
  public static List<ErlangCallbackSpec> getCallbackSpecs(@NotNull ErlangFunction function) {
    final String functionPresentation = ErlangPsiImplUtil.createFunctionPresentation(function);
    return ContainerUtil.mapNotNull(((ErlangFile)function.getContainingFile()).getBehaviours(), behaviour -> {
      ErlangFile behaviourModule = ErlangPsiImplUtil.resolveToFile(behaviour.getModuleRef());
      return behaviourModule != null ? behaviourModule.getCallbackByName(functionPresentation) : null;
    });
  }

  @Nullable
  public static NavigatablePsiElement getNavigatableSpecFun(final String presentation, PsiElement callbackSpec) {
    ErlangSpecFun specFun = PsiTreeUtil.findChildOfType(callbackSpec, ErlangSpecFun.class);

    if (specFun == null) return null;

    return new ErlangCompositeElementImpl(specFun.getNode()) {
      @Override
      public ItemPresentation getPresentation() {
        return new ItemPresentation() {
          @Nullable
          @Override
          public String getPresentableText() {
            return presentation;
          }

          @NotNull
          @Override
          public String getLocationString() {
            return getContainingFile().getName();
          }

          @Nullable
          @Override
          public Icon getIcon(boolean unused) {
            return ErlangIcons.CALLBACK;
          }
        };
      }

      @Nullable
      @Override
      public Icon getIcon(int flags) {
        return ErlangIcons.CALLBACK;
      }
    };
  }
}
