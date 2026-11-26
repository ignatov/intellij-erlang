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

import com.intellij.execution.Executor;
import com.intellij.execution.Location;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.testframework.AbstractTestProxy;
import com.intellij.execution.testframework.Filter;
import com.intellij.execution.testframework.TestFrameworkRunningModel;
import com.intellij.execution.testframework.actions.AbstractRerunFailedTestsAction;
import com.intellij.icons.AllIcons;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComponentContainer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.eunit.ErlangUnitRerunFailedTestsAction;
import org.intellij.erlang.eunit.ErlangUnitRunConfigurationType;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.runconfig.ErlangModuleBasedConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.VisibleForTesting;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class RebarEunitRerunFailedTestsAction extends AbstractRerunFailedTestsAction {
  static {
    // enables rerun failed tests action in RubyMine
    String rerunFailedTestsActionId = "RerunFailedTests";
    ActionManager actionManager = ActionManager.getInstance();
    AnAction rerunFailedTestsAction = actionManager.getAction(rerunFailedTestsActionId);
    if (rerunFailedTestsAction == null) {
      AnAction action = new AnAction(AllIcons.RunConfigurations.RerunFailedTests) {
        @Override
        public void actionPerformed(@NotNull AnActionEvent e) {
          throw new IllegalStateException("Should only be used as a template for other actions!");
        }
      };
      actionManager.registerAction(rerunFailedTestsActionId, action, PluginId.getId("org.jetbrains.erlang"));
    }
  }

  public RebarEunitRerunFailedTestsAction(@NotNull ComponentContainer componentContainer) {
    super(componentContainer);
  }

  @NotNull
  @Override
  protected Filter getFilter(@NotNull Project project, @NotNull GlobalSearchScope globalSearchScope) {
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
      public Module @NotNull [] getModules() {
        return ((RebarEunitRunConfiguration)getPeer()).getModules();
      }

      @NotNull
      @Override
      public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) {
        RebarEunitRunConfiguration runConfiguration = createRerunFailedTestsRunConfiguration();
        return new RebarEunitRunningState(env, runConfiguration);
      }

      private RebarEunitRunConfiguration createRerunFailedTestsRunConfiguration() {
        final Project project = getProject();
        final List<ErlangFunction> failedGeneratedTests = new ArrayList<>();
        final List<String> testsNotRerun = new ArrayList<>();
        String command = createRerunCommand(project, getFailedTests(project), failedGeneratedTests, testsNotRerun);

        if (!failedGeneratedTests.isEmpty()) {
          notifyGeneratedTestsFailed(failedGeneratedTests);
        }
        if (!testsNotRerun.isEmpty()) {
          ErlangUnitRerunFailedTestsAction.notifyTestsNotRerun(project, testsNotRerun, false);
        }

        return createRerunConfiguration((RebarEunitRunConfiguration) getPeer(), command);
      }

      private static void notifyGeneratedTestsFailed(final List<ErlangFunction> failedGeneratedTests) {
        ApplicationManager.getApplication().invokeLater(() -> Notifications.Bus.notify(
          new Notification("TestRunner", "Some tests cannot be rerun directly",
            "Some of failed tests were obtained via generator functions and cannot be rerun directly.\n" +
            createFailedTestsListMessage(failedGeneratedTests),
            NotificationType.WARNING)));
      }

      private static String createFailedTestsListMessage(List<ErlangFunction> failedTests) {
        int maxShownTests = 3;
        List<String> testNames = takeFunctionNames(failedTests, maxShownTests);
        int notShownTestsCount = failedTests.size() - testNames.size();
        String more = notShownTestsCount > 0 ? " and " + notShownTestsCount + " more" : "";
        return "Tests failed: " + StringUtil.join(testNames, ", ") + more;
      }

      private static List<String> takeFunctionNames(List<ErlangFunction> failedFunctions, int n) {
        ArrayList<String> result = new ArrayList<>(n);
        Iterator<ErlangFunction> iterator = failedFunctions.iterator();
        while (iterator.hasNext() && n > 0) {
          result.add(ErlangPsiImplUtil.getQualifiedFunctionName(iterator.next()));
        }
        return result;
      }
    };
  }

  // With rebar3 a failed test that has no test or generator function to rerun reruns its module, so that no failed
  // test is left out.
  @VisibleForTesting
  @NotNull
  static String createRerunCommand(@NotNull Project project,
                                   @NotNull List<? extends AbstractTestProxy> failedTestProxies,
                                   @NotNull List<ErlangFunction> failedGeneratedTests,
                                   @NotNull List<String> testsNotRerun) {
    boolean rebar3 = RebarSettings.getInstance(project).isRebar3();
    Set<String> otherModules = new LinkedHashSet<>();
    List<ErlangFunction> failedTests = new ArrayList<>();
    for (AbstractTestProxy testProxy : failedTestProxies) {
      List<String> notRerun = new ArrayList<>();
      List<ErlangFunction> functions =
        ErlangUnitRerunFailedTestsAction.findTestFunctionsToRerun(project, List.of(testProxy), notRerun);
      for (ErlangFunction function : functions) {
        if (failedTests.contains(function)) continue;
        failedTests.add(function);
        if (function.getArity() != 0) failedGeneratedTests.add(function);
      }
      // A suite (eunit://m) is a defect when one of its tests failed; those tests are rerun by themselves.
      if (rebar3 && functions.isEmpty() &&
          !ContainerUtil.exists(testProxy.getChildren(), child -> child.isDefect() || child.isInterrupted())) {
        Location location = testProxy.getLocation(project, GlobalSearchScope.allScope(project));
        String module = getModuleName(location != null ? location.getPsiElement() : null, testProxy.getLocationUrl());
        if (module != null) {
          otherModules.add(module);
          continue;
        }
      }
      testsNotRerun.addAll(notRerun);
    }
    Set<ErlangFile> suites = new LinkedHashSet<>();
    for (ErlangFunction function : failedTests) {
      PsiFile containingFile = function.getContainingFile();
      if (containingFile instanceof ErlangFile) suites.add((ErlangFile) containingFile);
    }

    if (rebar3) {
      return RebarEunitConfigurationUtil.createRebar3Command(suites, failedTests, otherModules, false);
    }
    return RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, failedTests, false, false);
  }

  @Nullable
  private static String getModuleName(@Nullable PsiElement psiElement, @Nullable String locationUrl) {
    PsiFile file = psiElement != null ? psiElement.getContainingFile() : null;
    VirtualFile virtualFile = file instanceof ErlangFile ? file.getVirtualFile() : null;
    if (virtualFile != null) return virtualFile.getNameWithoutExtension();
    String prefix = ErlangUnitRunConfigurationType.PROTOCOL + "://";
    if (locationUrl == null || !locationUrl.startsWith(prefix)) return null;
    String module = StringUtil.substringBefore(locationUrl.substring(prefix.length()) + ":", ":");
    return StringUtil.isEmpty(module) ? null : module;
  }

  @VisibleForTesting
  @NotNull
  static RebarEunitRunConfiguration createRerunConfiguration(@NotNull RebarEunitRunConfiguration original, @NotNull String command) {
    RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(original.getProject(), "");
    configuration.setCommand(command);
    configuration.setName("");
    configuration.setSkipDependencies(true);
    configuration.setEnvData(original.getEnvData());
    ErlangModuleBasedConfiguration configurationModule = original.getConfigurationModule();
    configuration.setModule(configurationModule != null ? configurationModule.getModule() : null);
    return configuration;
  }
}
