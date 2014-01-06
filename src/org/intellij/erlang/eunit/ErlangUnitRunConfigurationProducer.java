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

package org.intellij.erlang.eunit;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.LinkedHashSet;

public class ErlangUnitRunConfigurationProducer extends RunConfigurationProducer<ErlangUnitRunConfiguration> {
  public ErlangUnitRunConfigurationProducer() {
    super(ErlangUnitRunConfigurationType.getInstance());
  }

  @Override
  protected boolean setupConfigurationFromContext(@NotNull ErlangUnitRunConfiguration configuration,
                                                  @NotNull ConfigurationContext context,
                                                  @NotNull Ref<PsiElement> sourceElement) {
    PsiElement psiElement = sourceElement.get();
    if (psiElement == null || !psiElement.isValid()) {
      return false;
    }

    PsiFile file = psiElement.getContainingFile();
    if (!(file instanceof ErlangFile) || !ErlangPsiImplUtil.isEunitImported((ErlangFile) file) ||
      !ErlangTestRunConfigProducersUtil.shouldProduceEunitTestRunConfiguration(context.getProject(), context.getModule())) {
      return false;
    }

    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);
    if (module != null) {
      configuration.setModule(module);
    }

    LinkedHashSet<String> functionNames = findTestFunctionNames(psiElement);
    if (!functionNames.isEmpty()) {
      configuration.getConfigData().setFunctionNames(functionNames);
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);
      configuration.setName(functionNames.iterator().next() + (functionNames.size() > 1 ? " and " + (functionNames.size() - 1) + " more" : ""));
    }
    else {
      LinkedHashSet<String> moduleNames = findTestModuleNames(context);
      if (moduleNames.isEmpty()) return false;
      configuration.getConfigData().setModuleNames(moduleNames);
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
      configuration.setName(moduleNames.iterator().next() + (moduleNames.size() > 1 ? " and " + (moduleNames.size() - 1) + " more" : ""));
    }

    return true;
  }

  @Override
  public boolean isConfigurationFromContext(@NotNull ErlangUnitRunConfiguration configuration, @NotNull ConfigurationContext context) {
    PsiElement psiElement = context.getPsiLocation();
    if (psiElement == null || !psiElement.isValid()) {
      return false;
    }
    
    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);
    if (module == null || !module.equals(configuration.getConfigurationModule().getModule())) {
      return false;
    }

    ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind configurationKind = configuration.getConfigData().getKind();
    if (configurationKind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION) {
      LinkedHashSet<String> testFunctionNames = findTestFunctionNames(psiElement);
      return !testFunctionNames.isEmpty() && configuration.getConfigData().getFunctionNames().equals(testFunctionNames);
    }
    else if (configurationKind == ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE) {
      return configuration.getConfigData().getModuleNames().equals(findTestModuleNames(context));
    }

    return false;
  }

  @NotNull
  private static LinkedHashSet<String> findTestModuleNames(@NotNull ConfigurationContext context) {
    LinkedHashSet<String> moduleNames = new LinkedHashSet<String>();
    for (ErlangFile f : ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext())) {
      VirtualFile virtualFile = f.getVirtualFile();
      if (virtualFile != null) {
        moduleNames.add(virtualFile.getNameWithoutExtension());
      }
    }
    return moduleNames;
  }

  @NotNull
  private static LinkedHashSet<String> findTestFunctionNames(@NotNull PsiElement context) {
    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(context);
    functions = ContainerUtil.filter(functions, new Condition<ErlangFunction>() {
      @Override
      public boolean value(@NotNull ErlangFunction erlangFunction) {
        return ErlangPsiImplUtil.isEunitTestFunction(erlangFunction);
      }
    });
    LinkedHashSet<String> functionNames = new LinkedHashSet<String>();
    for (ErlangFunction f : functions) {
      functionNames.add(ErlangPsiImplUtil.getQualifiedFunctionName(f));
    }
    return functionNames;
  }
}