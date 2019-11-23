/*
 * Copyright 2012-2019 Sergey Ignatov
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

package org.intellij.erlang.runconfig;

import com.intellij.execution.lineMarker.ExecutorAction;
import com.intellij.execution.lineMarker.RunLineMarkerContributor;
import com.intellij.icons.AllIcons;
import com.intellij.lang.injection.InjectedLanguageManager;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.marker.ErlangBehaviourMarkerProvider;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangRunLineMarkerContributor extends RunLineMarkerContributor implements DumbAware {
  @Nullable
  @Override
  public Info getInfo(@NotNull PsiElement element) {
    ErlangFunction function = ErlangBehaviourMarkerProvider.findFunctionFromNameLeaf(element);
    if (function == null || !function.isExported() || function.getArity() != 0) return null;

    PsiFile psiFile = element.getContainingFile();
    if (!(psiFile instanceof ErlangFile)) return null;
    InjectedLanguageManager injectedLanguageManager = InjectedLanguageManager.getInstance(element.getProject());
    if (injectedLanguageManager.isInjectedFragment(psiFile)) return null;

    final AnAction[] actions = ExecutorAction.getActions();
    return new Info(AllIcons.RunConfigurations.TestState.Run, 
                    actions,
                    e -> StringUtil.join(ContainerUtil.mapNotNull(actions, action -> getText(action, e)), "\n"));
  }
}