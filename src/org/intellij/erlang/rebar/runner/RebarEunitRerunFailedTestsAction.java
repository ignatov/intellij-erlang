package org.intellij.erlang.rebar.runner;

import com.intellij.execution.ExecutionException;
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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.ComponentContainer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.runconfig.ErlangModuleBasedConfiguration;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

@SuppressWarnings("ComponentNotRegistered")
public class RebarEunitRerunFailedTestsAction extends AbstractRerunFailedTestsAction {
  static {
    // enables rerun failed tests action in RubyMine
    final String rerunFailedTestsActionId = "RerunFailedTests";
    ActionManager actionManager = ActionManager.getInstance();
    AnAction rerunFailedTestsAction = actionManager.getAction(rerunFailedTestsActionId);
    if (rerunFailedTestsAction == null) {
      AbstractRerunFailedTestsAction action = new AbstractRerunFailedTestsAction();
      actionManager.registerAction(rerunFailedTestsActionId, action, PluginId.getId("org.jetbrains.erlang"));
      action.getTemplatePresentation().setIcon(AllIcons.RunConfigurations.RerunFailedTests);
    }
  }

  public RebarEunitRerunFailedTestsAction(@NotNull ComponentContainer componentContainer) {
    super(componentContainer);
  }

  @NotNull
  @Override
  protected Filter getFilter(Project project, GlobalSearchScope globalSearchScope) {
    return new Filter() {
      @Override
      public boolean shouldAccept(AbstractTestProxy test) {
        return !test.isIgnored() && (test.isInterrupted() || test.isDefect());
      }
    };
  }

  @Nullable
  @Override
  public MyRunProfile getRunProfile() {
    TestFrameworkRunningModel model = getModel();
    if (model == null) return null;
    return new MyRunProfile((RunConfigurationBase) model.getProperties().getConfiguration()) {
      @NotNull
      @Override
      public Module[] getModules() {
        return ((RebarEunitRunConfiguration)getPeer()).getModules();
      }

      @Nullable
      @Override
      public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment env) throws ExecutionException {
        RebarEunitRunConfiguration runConfiguration = createRerunFailedTestsRunConfiguration();
        return new RebarEunitRunningState(env, runConfiguration);
      }

      private RebarEunitRunConfiguration createRerunFailedTestsRunConfiguration() {
        final Project project = getProject();
        RebarEunitRunConfiguration configuration = new RebarEunitRunConfiguration(project, "");
        final List<ErlangFunction> failedGeneratedTests = new ArrayList<ErlangFunction>();
        List<ErlangFunction> failedTests = ContainerUtil.mapNotNull(getFailedTests(project), new Function<AbstractTestProxy, ErlangFunction>() {
          @Nullable
          @Override
          public ErlangFunction fun(AbstractTestProxy testProxy) {
            Location location = testProxy.getLocation(project, GlobalSearchScope.allScope(project));
            PsiElement psiElement = location != null ? location.getPsiElement() : null;
            ErlangFunction function = psiElement instanceof ErlangFunction ? (ErlangFunction) psiElement : null;
            if (function != null && function.getArity() != 0) {
              failedGeneratedTests.add(function);
            }
            return function;
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

        if (!failedGeneratedTests.isEmpty()) {
          notifyGeneratedTestsFailed(failedGeneratedTests);
        }

        configuration.setCommand(RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, failedTests, false));
        configuration.setName("");
        configuration.setSkipDependencies(true);
        configuration.setModule(getModule());

        return configuration;
      }

      @Nullable
      private Module getModule() {
        RebarEunitRunConfiguration oldRunConf = (RebarEunitRunConfiguration) getPeer();
        ErlangModuleBasedConfiguration configurationModule = oldRunConf.getConfigurationModule();
        return configurationModule != null ? configurationModule.getModule() : null;
      }

      private void notifyGeneratedTestsFailed(final List<ErlangFunction> failedGeneratedTests) {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
          public void run() {
            Notifications.Bus.notify(
              new Notification("TestRunner", "Some tests cannot be rerun directly",
                "Some of failed tests were obtained via generator functions and cannot be rerun directly.\n" +
                createFailedTestsListMessage(failedGeneratedTests),
                NotificationType.WARNING));
          }
        });
      }

      private String createFailedTestsListMessage(List<ErlangFunction> failedTests) {
        final int maxShownTests = 3;
        List<String> testNames = takeFunctionNames(failedTests, maxShownTests);
        int notShownTestsCount = failedTests.size() - testNames.size();
        String more = notShownTestsCount > 0 ? " and " + notShownTestsCount + " more" : "";
        return "Tests failed: " + StringUtil.join(testNames, ", ") + more;
      }

      private List<String> takeFunctionNames(List<ErlangFunction> failedFunctions, int n) {
        ArrayList<String> result = new ArrayList<String>(n);
        Iterator<ErlangFunction> iterator = failedFunctions.iterator();
        while (iterator.hasNext() && n > 0) {
          result.add(ErlangPsiImplUtil.getQualifiedFunctionName(iterator.next()));
        }
        return result;
      }
    };
  }
}