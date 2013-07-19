package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.Location;
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
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

/**
 * @author savenko
 */
@SuppressWarnings("ComponentNotRegistered")
public class RebarEunitRerunFailedTestsAction extends AbstractRerunFailedTestsAction {
  public RebarEunitRerunFailedTestsAction(@NotNull ComponentContainer componentContainer) {
    super(componentContainer);
  }

  @NotNull
  @Override
  protected Filter getFilter(Project project) {
    if (allFailedTestsBelongToSingleSuite(project)) {
      return new Filter() {
        @Override
        public boolean shouldAccept(AbstractTestProxy test) {
          return isFailedTest(test);
        }
      };
    }
    //disables the action.
    return new Filter() {
      @Override
      public boolean shouldAccept(AbstractTestProxy test) {
        return false;
      }
    };
  }

  private boolean allFailedTestsBelongToSingleSuite(final Project project) {
    ErlangFile suite = null;
    TestFrameworkRunningModel model = getModel();
    List<? extends AbstractTestProxy> allTests = model != null ? model.getRoot().getAllTests() : ContainerUtil.<AbstractTestProxy>emptyList();

    for (AbstractTestProxy test : allTests) {
      if (!isFailedTest(test)) continue;
      Location location = test.getLocation(project);
      PsiElement psiElement = location != null ? location.getPsiElement() : null;
      PsiFile containingFile = psiElement != null ? psiElement.getContainingFile() : null;
      if (!(containingFile instanceof ErlangFile)) continue;
      if (suite != null && !suite.equals(containingFile)) return false;
      suite = (ErlangFile) containingFile;
    }

    return true;
  }

  private static boolean isFailedTest(AbstractTestProxy test) {
    return !test.isIgnored() && (test.isInterrupted() || test.isDefect());
  }

  @Nullable
  @Override
  public MyRunProfile getRunProfile() {
    TestFrameworkRunningModel model = getModel();
    if (model == null) return null;
    return new MyRunProfile(model.getProperties().getConfiguration()) {
      @NotNull
      @Override
      public Module[] getModules() {
        return ((RebarEunitRunConfiguration)getPeer()).getModules();
      }

      @Nullable
      @Override
      public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
        RebarEunitRunConfiguration runConfiguration = createRerunFailedTestsRunConfiguration();
        return new RebarRunningState(env, runConfiguration);
      }

      private RebarEunitRunConfiguration createRerunFailedTestsRunConfiguration() {
        final Project project = getProject();
        RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(project, "");

        List<ErlangFunction> failedTests = ContainerUtil.mapNotNull(getFailedTests(project), new Function<AbstractTestProxy, ErlangFunction>() {
          @Nullable
          @Override
          public ErlangFunction fun(AbstractTestProxy testProxy) {
            Location location = testProxy.getLocation(project);
            PsiElement psiElement = location != null ? location.getPsiElement() : null;
            return psiElement instanceof ErlangFunction ? (ErlangFunction) psiElement : null;
          }
        });
        Set<ErlangFile> suites = ContainerUtil.map2Set(failedTests, new Function<ErlangFunction, ErlangFile>() {
          @Nullable
          @Override
          public ErlangFile fun(ErlangFunction function) {
            PsiFile containingFile = function.getContainingFile();
            return containingFile instanceof ErlangFile ? (ErlangFile) containingFile : null;
          }
        });
        suites.remove(null);

        configuration.setCommand(RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, failedTests, false));
        configuration.setName("");
        configuration.setSkipDependencies(true);

        return configuration;
      }
    };
  }
}
