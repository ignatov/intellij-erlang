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

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.ObjectUtils;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

class RebarEunitConfigurationUtil {
  private static final String SUITES = "suites=";
  private static final String TESTS = "tests=";

  private RebarEunitConfigurationUtil() {
  }

  private static boolean appendSuitesOption(StringBuilder commandBuilder, Collection<ErlangFile> suites) {
    boolean suiteAdded = false;
    int lengthBeforeAppendingSuitesOption = commandBuilder.length();

    commandBuilder.append(SUITES);
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

    Set<String> distinctFunctionNames = ContainerUtil.map2Set(functions, ErlangFunction::getName);

    commandBuilder.append(TESTS);
    commandBuilder.append(StringUtil.join(distinctFunctionNames, ","));
  }

  @NotNull
  static String createDefaultRebarCommand(Collection<ErlangFile> suites, Collection<ErlangFunction> functions, boolean failIfNoSuitesSpecified) {
    PsiElement element = ObjectUtils.chooseNotNull(ContainerUtil.getFirstItem(suites), ContainerUtil.getFirstItem(functions));
    boolean rebar3 = element != null && RebarSettings.getInstance(element.getProject()).isRebar3();
    return createDefaultRebarCommand(suites, functions, failIfNoSuitesSpecified, rebar3);
  }

  @NotNull
  static String createDefaultRebarCommand(Collection<ErlangFile> suites,
                                          Collection<ErlangFunction> functions,
                                          boolean failIfNoSuitesSpecified,
                                          boolean rebar3) {
    if (rebar3) return createRebar3Command(suites, functions, failIfNoSuitesSpecified);
    StringBuilder commandBuilder = new StringBuilder();
    commandBuilder.append("eunit ");
    if (!appendSuitesOption(commandBuilder, suites) && failIfNoSuitesSpecified) return "";
    commandBuilder.append(' ');
    appendTestsOption(commandBuilder, functions);
    return commandBuilder.toString();
  }

  // rebar3 ignores suites= and tests= without a message and runs every test; it selects with --module and --test=Module:Function.
  @NotNull
  private static String createRebar3Command(Collection<ErlangFile> suites, Collection<ErlangFunction> functions, boolean failIfNoSuitesSpecified) {
    Set<String> modules = new LinkedHashSet<>();
    for (ErlangFile suite : suites) {
      ContainerUtil.addIfNotNull(modules, getModuleName(suite));
    }
    if (modules.isEmpty() && failIfNoSuitesSpecified) return "";
    List<String> tests = new ArrayList<>();
    for (ErlangFunction function : functions) {
      String module = getModuleName(function.getContainingFile());
      if (module != null) tests.add(module + ":" + function.getName());
    }
    if (!tests.isEmpty()) return "eunit " + StringUtil.join(getRebar3TestOptions(tests), " ");
    return modules.isEmpty() ? "eunit" : "eunit --module=" + StringUtil.join(modules, ",");
  }

  // --test=M:F runs M:F() as one test, so a generator (a name ending in _test_) is passed with --generator=M:F.
  @NotNull
  private static List<String> getRebar3TestOptions(@NotNull Collection<String> tests) {
    Set<String> simpleTests = new LinkedHashSet<>();
    Set<String> generators = new LinkedHashSet<>();
    for (String test : tests) {
      (test.endsWith("_test_") ? generators : simpleTests).add(test);
    }
    List<String> options = new ArrayList<>();
    if (!simpleTests.isEmpty()) options.add("--test=" + StringUtil.join(simpleTests, ","));
    if (!generators.isEmpty()) options.add("--generator=" + StringUtil.join(generators, ","));
    return options;
  }

  @Nullable
  private static String getModuleName(@Nullable PsiFile file) {
    VirtualFile virtualFile = file != null ? file.getVirtualFile() : null;
    return virtualFile != null ? virtualFile.getNameWithoutExtension() : null;
  }

  // The rebar 2 words suites= and tests= of a saved configuration as the options of rebar3. A test name of rebar 2 can be in
  // any of the suites, so with several suites all the tests of the suites are run.
  @NotNull
  static List<String> toRebar3Arguments(@NotNull List<String> arguments) {
    String suites = null;
    String tests = null;
    for (String argument : arguments) {
      if (argument.startsWith(SUITES)) suites = argument.substring(SUITES.length());
      else if (argument.startsWith(TESTS)) tests = argument.substring(TESTS.length());
    }
    if (suites == null) return arguments;

    List<String> modules = StringUtil.split(suites, ",");
    List<String> result = new ArrayList<>();
    for (String argument : arguments) {
      if (argument.startsWith(SUITES)) {
        List<String> testNames = tests != null ? StringUtil.split(tests, ",") : List.of();
        if (modules.size() == 1 && !testNames.isEmpty()) {
          result.addAll(getRebar3TestOptions(ContainerUtil.map(testNames, name -> modules.getFirst() + ":" + name)));
        }
        else if (!modules.isEmpty()) {
          result.add("--module=" + suites);
        }
      }
      else if (!argument.startsWith(TESTS)) {
        result.add(argument);
      }
    }
    return result;
  }
}
