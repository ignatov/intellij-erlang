package org.intellij.erlang.rebar.runner;

import com.intellij.execution.Location;
import com.intellij.execution.RunnerAndConfigurationSettings;
import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.eunit.ErlangUnitTestElementUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

/**
 * @author savenko
 */
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

    if (!(myFile instanceof ErlangFile) || !ErlangPsiImplUtil.isEunitImported((ErlangFile) myFile)) return null;

    RunnerAndConfigurationSettings settings = cloneTemplateConfiguration(psiElement.getProject(), context);
    RebarEunitRunConfiguration configuration = (RebarEunitRunConfiguration) settings.getConfiguration();

    Collection<ErlangFunction> functions = ErlangUnitTestElementUtil.findFunctionTestElements(psiElement);
    Collection<ErlangFile> suites = ErlangUnitTestElementUtil.findFileTestElements(context.getProject(), context.getDataContext());
    StringBuilder commandBuilder = new StringBuilder();
    commandBuilder.append("eunit ");
    if (!appendSuitesOption(commandBuilder, suites)) return null;
    commandBuilder.append(' ');
    appendTestsOption(commandBuilder, functions);

    configuration.setCommand(commandBuilder.toString());
    configuration.setSkipDependencies(true);
    configuration.setUseTestConsole(true);
    configuration.setName(createConfigurationName(functions, suites));

    return settings;
  }

  private static String createConfigurationName(Collection<ErlangFunction> functions, Collection<ErlangFile> suites) {
    if (functions.size() == 1) return functions.iterator().next().getName();
    if (suites.size() == 1) return suites.iterator().next().getName();
    if (functions.size() > 1) return "multi-funciton";
    if (suites.size() > 1) return "multi-suite";
    return "Rebar eunit";
  }

  private static boolean appendSuitesOption(StringBuilder commandBuilder, Collection<ErlangFile> suites) {
    boolean suiteAdded = false;

    commandBuilder.append("suites=");
    for (ErlangFile suiteFile : suites) {
      VirtualFile virtualFile = suiteFile.getVirtualFile();
      if (virtualFile != null) {
        commandBuilder.append(virtualFile.getNameWithoutExtension());
        commandBuilder.append(",");
        suiteAdded = true;
      }
    }
    commandBuilder.setLength(commandBuilder.length() - 1);
    return suiteAdded;
  }

  private static void appendTestsOption(StringBuilder commandBuilder, Collection<ErlangFunction> functions) {
    if (functions.isEmpty()) return;

    for (ErlangFunction f : functions) {
      commandBuilder.append(f.getName());
      commandBuilder.append(",");
    }
    commandBuilder.setLength(commandBuilder.length() - 1);
  }

  @Override
  public int compareTo(Object o) {
    return PREFERED;
  }
}
