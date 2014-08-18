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

package org.intellij.erlang.rebar.runner;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.eunit.ErlangTestRunConfigProducersUtil;
import org.intellij.erlang.eunit.ErlangUnitTestElementUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;

import java.util.Collection;

public class RebarEunitConfigurationProducer extends RunConfigurationProducer<RebarEunitRunConfiguration> {

  public RebarEunitConfigurationProducer() {
    super(RebarEunitRunConfigurationType.getInstance());
  }

  @Override
  protected boolean setupConfigurationFromContext(RebarEunitRunConfiguration configuration,
                                                  ConfigurationContext context,
                                                  Ref<PsiElement> sourceElement) {
    PsiElement psiElement = sourceElement.get();
    if (psiElement == null || !psiElement.isValid() ||
      !ErlangTestRunConfigProducersUtil.shouldProduceRebarTestRunConfiguration(context.getProject(), context.getModule())) {
      return false;
    }

    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(psiElement);
    Collection<ErlangFile> suites = ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext());
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, functions, true);

    if (command.isEmpty()) return false;

    configuration.setModule(context.getModule());
    configuration.setCommand(command);
    configuration.setSkipDependencies(true);
    configuration.setName(createConfigurationName(functions, suites));

    return true;
  }

  @Override
  public boolean isConfigurationFromContext(RebarEunitRunConfiguration configuration, ConfigurationContext context) {
    PsiElement psiElement = context.getPsiLocation();
    if (psiElement == null || !psiElement.isValid()) {
      return false;
    }

    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(psiElement);
    Collection<ErlangFile> suites = ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext());
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, functions, true);

    return configuration.getCommand().equals(command) && configuration.isSkipDependencies();
  }

  private static String createConfigurationName(Collection<ErlangFunction> functions, Collection<ErlangFile> suites) {
    if (suites.isEmpty()) return "Rebar Eunit";

    VirtualFile virtualFile = suites.iterator().next().getVirtualFile();
    String firstModuleName = virtualFile != null ? virtualFile.getNameWithoutExtension() : "";
    String firstFuntionName = functions.size() > 0 ? firstModuleName + ":" + functions.iterator().next().getName() : "";

    if (functions.size() == 1) return firstFuntionName;
    if (suites.size() == 1) return firstModuleName;
    if (functions.size() > 1) return firstFuntionName + " and " + (functions.size() - 1) + " more";

    return firstModuleName + " and " + (suites.size() - 1) + " more";
  }
}
