/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.testIntegration.TestLocationProvider;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author ignatov
 */
public class ErlangTestLocationProvider implements TestLocationProvider {
  private static final Pattern MODULE_FUNCTION_PATTERN = Pattern.compile("(\\w+):(\\w+)");
  private static final Pattern MODULE_PATTERN = Pattern.compile("(\\w+)");

  @NotNull
  @Override
  public List<Location> getLocation(@NotNull String protocolId, @NotNull String locationData, Project project) {
    if (!ErlangUnitRunConfigurationType.PROTOCOL.equals(protocolId)) return Collections.emptyList();

    SmartList<Location> list = new SmartList<Location>();

    Matcher matcher = MODULE_FUNCTION_PATTERN.matcher(locationData);
    if (matcher.matches()) {
      String module = matcher.group(1);
      String function = matcher.group(2);

      for (PsiFile file : getPsiFiles(project, module)) {
        if (file instanceof ErlangFile) {
          Collection<ErlangFunction> functionsByName = ((ErlangFile) file).getFunctionsByName(function);
          ErlangFunction item = ContainerUtil.getFirstItem(functionsByName);
          if (item != null) {
            list.add(new PsiLocation<PsiElement>(project, item));
          }
        }
      }
    }
    
    if (list.size() > 0) return list;

    matcher = MODULE_PATTERN.matcher(locationData);
    if (matcher.matches()) {
      String module = matcher.group(1);

      for (PsiFile file : getPsiFiles(project, module)) {
        if (file instanceof ErlangFile) {
          for (ErlangAttribute moduleAttributes : ((ErlangFile) file).getAttributes()) {
            ErlangModule m = moduleAttributes.getModule();
            if (m != null) {
              list.add(new PsiLocation<PsiElement>(project, m));
            }
          }
        }
      }
    }

    return list;
  }

  private static PsiFile[] getPsiFiles(Project project, String module) {
    List<ErlangFile> fromProject = ErlangModuleIndex.getFilesByName(project, module, 
      GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.projectScope(project), ErlangFileType.MODULE));
    return fromProject.toArray(new PsiFile[fromProject.size()]);
  }
}