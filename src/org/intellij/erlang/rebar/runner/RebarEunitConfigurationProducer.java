package org.intellij.erlang.rebar.runner;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.eunit.ErlangTestRunConfigProducersUtil;
import org.intellij.erlang.eunit.ErlangUnitTestElementUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;

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
    if (psiElement == null || !psiElement.isValid()) {
      return false;
    }

    PsiFile file = psiElement.getContainingFile();
    if (!(file instanceof ErlangFile) || !ErlangPsiImplUtil.isEunitImported((ErlangFile) file) ||
      !ErlangTestRunConfigProducersUtil.shouldProduceRebarTestRunConfiguration(context.getProject(), context.getModule())) {
      return false;
    }

    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(psiElement);
    Collection<ErlangFile> suites = ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext());
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, functions, true);

    if (command.isEmpty()) return false;

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
