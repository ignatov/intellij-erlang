package org.intellij.erlang.rebar.runner;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Set;

class RebarEunitConfigurationUtil {
  private RebarEunitConfigurationUtil() {
  }

  private static boolean appendSuitesOption(StringBuilder commandBuilder, Collection<ErlangFile> suites) {
    boolean suiteAdded = false;
    int lengthBeforeAppendingSuitesOption = commandBuilder.length();

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
    if (!suiteAdded) {
      commandBuilder.setLength(lengthBeforeAppendingSuitesOption);
    }

    return suiteAdded;
  }

  private static void appendTestsOption(StringBuilder commandBuilder, Collection<ErlangFunction> functions) {
    if (functions.isEmpty()) return;

    Set<String> distinctFunctionNames = ContainerUtil.map2Set(functions, new Function<ErlangFunction, String>() {
      @Override
      public String fun(ErlangFunction function) {
        return function.getName();
      }
    });

    commandBuilder.append("tests=");
    commandBuilder.append(StringUtil.join(distinctFunctionNames, ","));
  }

  @NotNull
  static String createDefaultRebarCommand(Collection<ErlangFile> suites, Collection<ErlangFunction> functions, boolean failIfNoSuitesSpecified) {
    StringBuilder commandBuilder = new StringBuilder();
    commandBuilder.append("eunit ");
    if (!appendSuitesOption(commandBuilder, suites) && failIfNoSuitesSpecified) return "";
    commandBuilder.append(' ');
    appendTestsOption(commandBuilder, functions);
    return commandBuilder.toString();
  }
}
