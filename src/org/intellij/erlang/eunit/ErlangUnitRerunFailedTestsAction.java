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
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComponentContainer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class ErlangUnitRerunFailedTestsAction extends AbstractRerunFailedTestsAction {
  @VisibleForTesting
  static final String NOTIFICATION_GROUP_ID = "Test Results";

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
    Set<String> testsToRerun = collectTestsToRerun(environment.getProject(), getFailedTests(environment.getProject()));
    if (testsToRerun == null) return null;
    return new MyRunProfile((RunConfigurationBase) model.getProperties().getConfiguration()) {
      @NotNull
      @Override
      public Module @NotNull [] getModules() {
        return ((ErlangUnitRunConfiguration)getPeer()).getModules();
      }

      @NotNull
      @Override
      public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) {
        ErlangUnitRunConfiguration runConfiguration = createRerunConfiguration((ErlangUnitRunConfiguration)getPeer(), testsToRerun);

        return new ErlangUnitRunningState(env, getModules()[0], runConfiguration);
      }
    };
  }

  /**
   * The qualified names of the functions to rerun, or null when there is none. Tells the user about the failed
   * tests that cannot be rerun.
   */
  @VisibleForTesting
  @Nullable
  static Set<String> collectTestsToRerun(@NotNull Project project, @NotNull List<? extends AbstractTestProxy> failedTests) {
    List<String> testsNotRerun = new ArrayList<>();
    Set<String> testsToRerun = new LinkedHashSet<>();
    for (ErlangFunction function : findTestFunctionsToRerun(project, failedTests, testsNotRerun)) {
      testsToRerun.add(ErlangPsiImplUtil.getQualifiedFunctionName(function));
    }
    if (testsToRerun.isEmpty() || !testsNotRerun.isEmpty()) {
      notifyTestsNotRerun(project, testsNotRerun, testsToRerun.isEmpty());
    }
    return testsToRerun.isEmpty() ? null : testsToRerun;
  }

  /**
   * The functions that rerun the failed tests. The names of the failed tests for which there is no such function
   * are added to {@code testsNotRerun}.
   */
  @NotNull
  public static List<ErlangFunction> findTestFunctionsToRerun(@NotNull Project project,
                                                              @NotNull List<? extends AbstractTestProxy> failedTests,
                                                              @NotNull Collection<String> testsNotRerun) {
    List<ErlangFunction> functions = new ArrayList<>();
    for (AbstractTestProxy testProxy : failedTests) {
      Location location = testProxy.getLocation(project, GlobalSearchScope.allScope(project));
      PsiElement psiElement = location != null ? location.getPsiElement() : null;
      ErlangFunction function = ErlangUnitTestElementUtil.findRerunnableTestFunction(
        psiElement, ErlangTestLocationProvider.isGeneratedTestLocation(testProxy.getLocationUrl()));
      if (function != null) {
        if (!functions.contains(function)) functions.add(function);
      }
      else if (testProxy.isLeaf()) {
        testsNotRerun.add(testProxy.getName());
      }
    }
    return functions;
  }

  public static void notifyTestsNotRerun(@NotNull Project project, @NotNull List<String> testsNotRerun, boolean nothingToRerun) {
    int maxShownTests = 3;
    String shown = StringUtil.join(ContainerUtil.getFirstItems(testsNotRerun, maxShownTests), ", ");
    String more = testsNotRerun.size() > maxShownTests ? " and " + (testsNotRerun.size() - maxShownTests) + " more" : "";
    String content = testsNotRerun.isEmpty()
                     ? "No test or generator function was found for the failed tests."
                     : "No test or generator function was found for " + StringUtil.escapeXmlEntities(shown + more) + ".";
    Notifications.Bus.notify(new Notification(NOTIFICATION_GROUP_ID,
      nothingToRerun ? "Failed tests cannot be rerun" : "Some failed tests cannot be rerun",
      content, NotificationType.WARNING), project);
  }

  @VisibleForTesting
  @NotNull
  static ErlangUnitRunConfiguration createRerunConfiguration(@NotNull ErlangUnitRunConfiguration original,
                                                             @NotNull Set<String> functionNames) {
    ErlangUnitRunConfiguration configuration =
      new ErlangUnitRunConfiguration(original.getProject(), "", ErlangUnitRunConfigurationType.getInstance());
    configuration.setEnvData(original.getEnvData());
    configuration.getConfigData().setKind(ErlangUnitRunConfiguration.ErlangUnitRunConfigurationKind.FUNCTION);
    configuration.getConfigData().setFunctionNames(functionNames);
    return configuration;
  }
}
