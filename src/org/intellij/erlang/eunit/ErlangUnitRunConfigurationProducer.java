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

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.util.Condition;
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

public class ErlangUnitRunConfigurationProducer extends RuntimeConfigurationProducer implements Cloneable {
  private PsiFile myFile;

  public ErlangUnitRunConfigurationProducer() {
    super(ErlangUnitRunConfigurationType.getInstance());
  }

  @Override
  public PsiElement getSourceElement() {
    return myFile;
  }

  @Override
  protected RunnerAndConfigurationSettings createConfigurationByElement(Location location, ConfigurationContext context) {
    PsiElement psiElement = location.getPsiElement();
    myFile = psiElement.getContainingFile();

    if (!(myFile instanceof ErlangFile) || !ErlangPsiImplUtil.isEunitImported((ErlangFile) myFile) ||
      !ErlangTestRunConfigProducersUtil.shouldProduceEunitTestRunConfiguration(context.getProject(), context.getModule())) return null;

    RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(psiElement.getProject(), context);
    ErlangUnitRunConfiguration configuration = (ErlangUnitRunConfiguration) settings.getConfiguration();

    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);
    if (module != null) {
      configuration.setModule(module);
    }

    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(psiElement);

    functions = ContainerUtil.filter(functions, new Condition<ErlangFunction>() {
      @Override
      public boolean value(ErlangFunction erlangFunction) {
        return ErlangPsiImplUtil.isEunitTestFunction(erlangFunction);
      }
    });
    if (!functions.isEmpty()) {
      LinkedHashSet<String> functionNames = new LinkedHashSet<String>();
      for (ErlangFunction f : functions) {
        functionNames.add(ErlangPsiImplUtil.getQualifiedFunctionName(f));
      }
      configuration.getConfigData().setFunctionNames(functionNames);
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);
      configuration.setName(functionNames.iterator().next() + (functionNames.size() > 1 ? " and " + (functionNames.size() - 1) + " more" : ""));
    }
    else {
      LinkedHashSet<String> moduleNames = new LinkedHashSet<String>();
      for (ErlangFile f : ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext())) {
        VirtualFile virtualFile = f.getVirtualFile();
        if (virtualFile != null) {
          moduleNames.add(virtualFile.getNameWithoutExtension());
        }
      }

      if (moduleNames.isEmpty()) return null;

      configuration.getConfigData().setModuleNames(moduleNames);
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
      configuration.setName(moduleNames.iterator().next() + (moduleNames.size() > 1 ? " and " + (moduleNames.size() - 1) + " more" : ""));
    }

    return settings;
  }

  @Override
  public int compareTo(@NotNull Object o) {
    return PREFERED;
  }
}