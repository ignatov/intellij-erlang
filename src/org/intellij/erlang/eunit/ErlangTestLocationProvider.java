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

package org.intellij.erlang.eunit;

import com.intellij.execution.Location;
import com.intellij.execution.PsiLocation;
import com.intellij.execution.testframework.sm.runner.SMTestLocator;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.SmartList;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ErlangTestLocationProvider implements SMTestLocator {
  public static final ErlangTestLocationProvider INSTANCE = new ErlangTestLocationProvider();

  private static final Pattern LOCATION_PATTERN = Pattern.compile("^(\\w+)(?::(\\w+)(?::(-?\\d+))?)?$");

  @NotNull
  @Override
  public List<Location> getLocation(@NotNull String protocol,
                                    @NotNull String path,
                                    @NotNull Project project,
                                    @NotNull GlobalSearchScope scope) {
    if (!ErlangUnitRunConfigurationType.PROTOCOL.equals(protocol)) return ContainerUtil.emptyList();

    Matcher matcher = LOCATION_PATTERN.matcher(path);
    if (!matcher.matches()) return ContainerUtil.emptyList();

    String module = matcher.group(1);
    String function = matcher.group(2);
    String line = matcher.group(3);
    List<Location> locations = new SmartList<>();
    Collection<ErlangFile> erlangFiles = getErlangFiles(project, module);

    if (function != null) {
      for (ErlangFile file : erlangFiles) {
        Location testLocation = getTestLocation(project, file, function, line);
        ContainerUtil.addIfNotNull(locations, testLocation);
      }
    }
    if (locations.isEmpty()) {
      for (ErlangFile file : erlangFiles) {
        Location moduleLocation = getModuleLocation(project, file);
        ContainerUtil.addIfNotNull(locations, moduleLocation);
      }
    }
    return locations;
  }

  @Nullable
  private static Location getTestLocation(Project project, ErlangFile file, String function, String line) {
    ErlangFunction f = ContainerUtil.getFirstItem(file.getFunctionsByName(function));
    String fileText = file.getText();
    int lineNumber = StringUtil.parseInt(line, -1);
    if (f == null) return null;
    if (lineNumber != -1) {
      lineNumber -= 1;
      if (lineNumber != StringUtil.offsetToLineNumber(fileText, f.getTextOffset())) {
        PsiElement testElement = findTestElementInLine(file, fileText, lineNumber);
        if (testElement != null) {
          return new PsiLocation<>(project, testElement);
        }
      }
    }
    return new PsiLocation<PsiElement>(project, f);
  }

  @Nullable
  private static Location getModuleLocation(Project project, ErlangFile file) {
    ErlangModule module = file.getModule();
    return module != null ? new PsiLocation<PsiElement>(project, module) : null;
  }

  @Nullable
  private static PsiElement findTestElementInLine(ErlangFile file, String fileText, int line) {
    int firstColumnOffset = StringUtil.lineColToOffset(fileText, line, 0);
    PsiElement element = file.findElementAt(firstColumnOffset);
    ASTNode node = element != null ? element.getNode() : null;
    if (node == null) return null;

    ASTNode nonWhitespaceSibling = FormatterUtil.getNextNonWhitespaceSibling(node);
    return nonWhitespaceSibling != null ? nonWhitespaceSibling.getPsi() : null;
  }

  private static Collection<ErlangFile> getErlangFiles(Project project, String module) {
    return ErlangModuleIndex.getFilesByName(project, module,
      GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.projectScope(project), ErlangFileType.MODULE));
  }
}