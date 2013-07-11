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
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.Nullable;

/**
 * @author ignatov
 */
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

    if (!(myFile instanceof ErlangFile) || !ErlangPsiImplUtil.isEunitImported((ErlangFile) myFile)) return null;

    RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(psiElement.getProject(), context);
    ErlangUnitRunConfiguration configuration = (ErlangUnitRunConfiguration) settings.getConfiguration();

    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);
    if (module != null) {
      configuration.setModule(module);
    }

    final VirtualFile vFile = myFile.getVirtualFile();
    if (vFile == null) return null;
    String moduleName = vFile.getNameWithoutExtension();

    //TODO support multiple functions selection
    ErlangFunction function = getParentNullaryFunction(psiElement);
    String functionName = function != null ? function.getName() : null;

    configuration.getConfigData().setModuleNames(ContainerUtil.set(moduleName));

    if (function != null) {
      String qualifiedFunctionName = moduleName + ":" + functionName;

      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);
      configuration.getConfigData().setFunctionNames(ContainerUtil.set(qualifiedFunctionName));
      configuration.setName(qualifiedFunctionName);
    }
    else {
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
      configuration.setName(moduleName);
    }

    return settings;
  }

  @Override
  public int compareTo(Object o) {
    return PREFERED;
  }

  @Nullable
  private static ErlangFunction getParentNullaryFunction(PsiElement psiElement) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    int arity = function != null ? function.getArity() : -1;
    return 0 == arity ? function : null;
  }
}

