package org.intellij.erlang.rebar.runner;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.eunit.ErlangTestRunConfigProducersUtil;
import org.intellij.erlang.eunit.ErlangUnitTestElementUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class RebarEunitConfigurationProducer extends RuntimeConfigurationProducer {

  private PsiFile myFile;

  public RebarEunitConfigurationProducer() {
    super(RebarEunitRunConfigurationType.getInstance());
  }

  @Override
  public PsiElement getSourceElement() {
    return myFile;
  }

  @Nullable
  @Override
  protected RunnerAndConfigurationSettings createConfigurationByElement(Location location, ConfigurationContext context) {
    PsiElement psiElement = location.getPsiElement();

    myFile = psiElement.getContainingFile();

    if (!(myFile instanceof ErlangFile) || !ErlangPsiImplUtil.isEunitImported((ErlangFile) myFile) ||
      !ErlangTestRunConfigProducersUtil.shouldProduceRebarTestRunConfiguration(context.getProject(), context.getModule())) return null;

    RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(psiElement.getProject(), context);
    RebarEunitRunConfiguration configuration = (RebarEunitRunConfiguration) settings.getConfiguration();

    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(psiElement);
    Collection<ErlangFile> suites = ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext());
    String command = RebarEunitConfigurationUtil.createDefaultRebarCommand(suites, functions, true);

    if (command.isEmpty()) return null;

    configuration.setCommand(command);
    configuration.setSkipDependencies(true);
    configuration.setName(createConfigurationName(functions, suites));

    return settings;
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

  @Override
  public int compareTo(@NotNull Object o) {
    return PREFERED;
  }
}
