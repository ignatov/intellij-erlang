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

    Set<String> distinctFunctionNames = ContainerUtil.map2Set(functions, ErlangFunction::getName);

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
