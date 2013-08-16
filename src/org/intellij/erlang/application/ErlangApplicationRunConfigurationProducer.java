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

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

/**
 * @author ignatov
 */
public class ErlangApplicationRunConfigurationProducer extends RuntimeConfigurationProducer implements Cloneable {
  private ErlangFunction myFunction;

  public ErlangApplicationRunConfigurationProducer() {
    super(ErlangApplicationRunConfigurationType.getInstance());
  }

  @Override
  public PsiElement getSourceElement() {
    return myFunction;
  }

  @Override
  protected RunnerAndConfigurationSettings createConfigurationByElement(Location location, ConfigurationContext context) {
    PsiElement psiElement = location.getPsiElement();
    myFunction = PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    Project project = psiElement.getProject();
    PsiFile containingFile = psiElement.getContainingFile();

    if (!(containingFile instanceof ErlangFile) ||
        myFunction == null || ErlangPsiImplUtil.isEunitTestFunction(myFunction) ||
        ErlangPsiImplUtil.isPrivateFunction(containingFile, myFunction)) {
      return null;
    }

    RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(project, context);
    ErlangApplicationConfiguration configuration = (ErlangApplicationConfiguration) settings.getConfiguration();

    Module module = ModuleUtilCore.findModuleForPsiElement(psiElement);

    final VirtualFile vFile = containingFile.getVirtualFile();
    if (vFile == null) return null;
    String moduleName = vFile.getNameWithoutExtension();
    String functionName = myFunction.getName();

    configuration.setModuleAndFunction(moduleName + " " + functionName);
    configuration.setName(moduleName + "." + functionName);
    if (module != null) {
      configuration.setModule(module);
    }
    return settings;
  }

  @Override
  public int compareTo(Object o) {
    return PREFERED;
  }
}
