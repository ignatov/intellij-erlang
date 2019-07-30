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

package org.intellij.erlang.eunit;

import com.intellij.execution.Executor;
import com.intellij.execution.Location;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.testframework.AbstractTestProxy;
import com.intellij.execution.testframework.Filter;
import com.intellij.execution.testframework.TestFrameworkRunningModel;
import com.intellij.execution.testframework.actions.AbstractRerunFailedTestsAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComponentContainer;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.LinkedHashSet;

public class ErlangUnitRerunFailedTestsAction extends AbstractRerunFailedTestsAction {
  public ErlangUnitRerunFailedTestsAction(@NotNull ComponentContainer componentContainer) {
    super(componentContainer);
  }

  @NotNull
  @Override
  protected Filter getFilter(@NotNull Project project, @NotNull GlobalSearchScope scope) {
    return new Filter() {
      @Override
      public boolean shouldAccept(AbstractTestProxy test) {
        return !test.isIgnored() && (test.isInterrupted() || test.isDefect());
      }
    };
  }

  @Nullable
  @Override
  public MyRunProfile getRunProfile(@NotNull ExecutionEnvironment environment) {
    TestFrameworkRunningModel model = getModel();
    if (model == null) return null;
    return new MyRunProfile((RunConfigurationBase) model.getProperties().getConfiguration()) {
      @NotNull
      @Override
      public Module[] getModules() {
        return ((ErlangUnitRunConfiguration)getPeer()).getModules();
      }

      @NotNull
      @Override
      public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) {
        ErlangUnitRunConfiguration runConfiguration = createRerunFailedTestsRunConfiguration();

        return new ErlangUnitRunningState(env, getModules()[0], runConfiguration);
      }

      private ErlangUnitRunConfiguration createRerunFailedTestsRunConfiguration() {
        Project project = getProject();
        ErlangUnitRunConfiguration configuration = new ErlangUnitRunConfiguration(project, "", ErlangUnitRunConfigurationType.getInstance());

        configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);

        LinkedHashSet<String> testsToRerun = new LinkedHashSet<>();
        for (AbstractTestProxy testProxy : getFailedTests(project)) {
          Location location = testProxy.getLocation(project, GlobalSearchScope.allScope(project));
          PsiElement psiElement = location != null ? location.getPsiElement() : null;

          if (!(psiElement instanceof ErlangFunction)) continue;

          ErlangFunction function = (ErlangFunction) psiElement;
          String functionName = ErlangPsiImplUtil.getQualifiedFunctionName(function);
          testsToRerun.add(functionName);
        }

        configuration.getConfigData().setFunctionNames(testsToRerun);
        return configuration;
      }
    };
  }
}
