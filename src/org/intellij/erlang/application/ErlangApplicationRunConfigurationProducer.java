/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang.application;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangApplicationRunConfigurationProducer extends RunConfigurationProducer<ErlangApplicationConfiguration> {
  public ErlangApplicationRunConfigurationProducer() {
    super(ErlangApplicationRunConfigurationType.getInstance());
  }

  @Override
  protected boolean setupConfigurationFromContext(ErlangApplicationConfiguration configuration,
                                                  ConfigurationContext context,
                                                  Ref<PsiElement> sourceElement) {
    PsiElement psiElement = sourceElement.get();
    if (psiElement == null || !psiElement.isValid()) {
      return false;
    }

    ErlangFunction function = PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    PsiFile containingFile = psiElement.getContainingFile();

    if (!(containingFile instanceof ErlangFile) || function == null ||
      ErlangPsiImplUtil.isEunitTestFunction(function) ||
      ErlangPsiImplUtil.isPrivateFunction(containingFile, function)) {
      return false;
    }

    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);
    VirtualFile vFile = containingFile.getVirtualFile();
    if (vFile == null) return false;
    String moduleName = vFile.getNameWithoutExtension();
    String functionName = function.getName();

    configuration.setModuleAndFunction(moduleNameAndFunction(moduleName, functionName));
    configuration.setName(moduleName + "." + functionName);
    if (module != null) {
      configuration.setModule(module);
    }
    return true;
  }

  @Override
  public boolean isConfigurationFromContext(ErlangApplicationConfiguration configuration,
                                            ConfigurationContext context) {
    PsiElement psiElement = context.getPsiLocation();
    if (psiElement == null || !psiElement.isValid()) {
      return false;
    }

    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);
    if (module == null || !module.equals(configuration.getConfigurationModule().getModule())) {
      return false;
    }

    ErlangFunction function = PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    PsiFile containingFile = psiElement.getContainingFile();
    VirtualFile vFile = containingFile != null ? containingFile.getVirtualFile() : null;
    if (function == null || vFile == null) return false;

    return StringUtil.equals(configuration.getModuleAndFunction(),
      moduleNameAndFunction(vFile.getNameWithoutExtension(), function.getName()));
  }

  @NotNull
  private static String moduleNameAndFunction(@NotNull String moduleName, @NotNull String functionName) {
    return moduleName + " " + functionName;
  }
}
