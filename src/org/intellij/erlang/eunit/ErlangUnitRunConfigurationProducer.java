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
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.Nullable;

import java.util.*;

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

    Collection<ErlangFunction> functions = findFunctionTestElements(psiElement);

    if (!functions.isEmpty()) {
      LinkedHashSet<String> functionNames = new LinkedHashSet<String>();
      for (ErlangFunction f : functions) {
        functionNames.add(ErlangPsiImplUtil.getQualifiedFunctionName(f));
      }
      configuration.getConfigData().setFunctionNames(functionNames);
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);
      configuration.setName(functionNames.size() == 1 ? functionNames.iterator().next() : "multi-function");
    }
    else {
      LinkedHashSet<String> moduleNames = new LinkedHashSet<String>();
      for (ErlangFile f : findFileTestElements(context.getProject(), context.getDataContext())) {
        VirtualFile virtualFile = f.getVirtualFile();
        if (virtualFile != null) {
          moduleNames.add(virtualFile.getNameWithoutExtension());
        }
      }

      if (moduleNames.isEmpty()) return null;

      configuration.getConfigData().setModuleNames(moduleNames);
      configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.MODULE);
      configuration.setName(moduleNames.size() == 1 ? moduleNames.iterator().next() : "multi-module");
    }

    return settings;
  }

  public static Collection<ErlangFunction> findFunctionTestElements(PsiElement element) {
    //TODO support multiple functions selection
    SmartList<ErlangFunction> selectedFunctions = new SmartList<ErlangFunction>();
    ErlangFunction function = getParentNullaryFunction(element);

    if (function != null) {
      selectedFunctions.add(function);
    }
    return selectedFunctions;
  }

  public static Collection<ErlangFile> findFileTestElements(Project project, DataContext dataContext) {
    VirtualFile[] selectedFiles = PlatformDataKeys.VIRTUAL_FILE_ARRAY.getData(dataContext);

    if (selectedFiles == null) return Collections.emptyList();

    List<ErlangFile> testFiles = new ArrayList<ErlangFile>(selectedFiles.length);
    for (VirtualFile file : selectedFiles) {
      PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
      if (psiFile instanceof ErlangFile) {
        testFiles.add((ErlangFile) psiFile);
      }
    }
    return testFiles;
  }

  @Override
  public int compareTo(Object o) {
    return PREFERED;
  }

  @Nullable
  private static ErlangFunction getParentNullaryFunction(PsiElement psiElement) {
    ErlangFunction function = psiElement instanceof ErlangFunction ? (ErlangFunction)psiElement : PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    int arity = function != null ? function.getArity() : -1;
    return 0 == arity ? function : null;
  }
}

