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

package org.intellij.erlang.rebar.util;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.execution.process.ScriptRunnerUtil;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class RebarConfigUtil {
  private RebarConfigUtil() {
  }

  @NotNull
  public static String getRebarPath(@Nullable String directory) {
    if (directory != null) {
      File rebar = new File(directory, "rebar");
      if (rebar.exists() && rebar.canExecute()) {
        return rebar.getPath();
      }
    }

    boolean isPosix = SystemInfo.isMac || SystemInfo.isLinux || SystemInfo.isUnix;
    if (!isPosix) return "";

    String output = "";
    try {
      GeneralCommandLine which = new GeneralCommandLine("which");
      which.addParameter("rebar");
      output = ScriptRunnerUtil.getProcessOutput(which);
    } catch (Exception ignored) {
    }
    return output.trim();
  }

  @NotNull
  public static Set<String> getRebarDependencies(@Nullable final Module module, boolean resolveDependencies) throws ExecutionException {
    final Set<String> dependencies = new HashSet<String>();
    List<String> dependencyAppNames = new ArrayList<String>();

    ErlangFile rebarConfig=RebarConfigUtil.getRebarConfig(module.getProject(), module.getModuleFile().getParent());

    if (resolveDependencies) {
      for (String fetchedDep : resolveDependencies(module)) {
        Pattern regexp  = Pattern.compile("==>\\s(.*)\\s\\(get-deps\\)");
        Matcher matcher = regexp.matcher(fetchedDep);

        while(matcher.find()) {
          String found = matcher.group(1);

          if (!found.isEmpty())
            dependencyAppNames.add(found);
        }
      }
    } else {
      dependencyAppNames = RebarConfigUtil.getDependencyAppNames(rebarConfig);
    }

    for (String appName : dependencyAppNames) {
      dependencies.add(String.format("deps%s%s%sebin", File.separator, appName, File.separator));
    }

    dependencies.add("ebin");

    return dependencies;
  }

  private static Set<String> resolveDependencies(@Nullable final Module module) throws ExecutionException {
    Project project = module.getProject();
    VirtualFile projectRoot = module.getModuleFile().getParent();
    String sdkPath = project != null ? ErlangSdkType.getSdkPath(project) : null;
    final String escriptPath = sdkPath != null ?
            JpsErlangSdkType.getScriptInterpreterExecutable(sdkPath).getAbsolutePath() :
            JpsErlangSdkType.getExecutableFileName(JpsErlangSdkType.SCRIPT_INTERPRETER);

    String rebarPath=RebarConfigUtil.getRebarPath(projectRoot.getCanonicalPath());

    GeneralCommandLine commandLine = new GeneralCommandLine();
    commandLine.withWorkDirectory(projectRoot.getCanonicalPath());
    commandLine.setExePath(escriptPath);
    commandLine.addParameter(rebarPath);
    commandLine.addParameter("get-deps");

    ProcessOutput output = ErlangSystemUtil.execute(commandLine);

    return new HashSet<String>(output.getStdoutLines(true));
  }

  @NotNull
  public static List<String> getIncludePaths(@NotNull ErlangFile rebarConfig) {
    final List<String> includePaths = ContainerUtil.newArrayList();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "erl_opts", section -> ErlangTermFileUtil.processConfigSection(section, "i", includeOptionValue -> {
      if (includeOptionValue instanceof ErlangStringLiteral) {
        includePaths.add(getStringLiteralText((ErlangStringLiteral) includeOptionValue));
      }
      else {
        for (ErlangStringLiteral includePath : PsiTreeUtil.findChildrenOfType(includeOptionValue, ErlangStringLiteral.class)) {
          includePaths.add(getStringLiteralText(includePath));
        }
      }
    }));
    return includePaths;
  }

  @NotNull
  public static List<String> getDependencyAppNames(@NotNull ErlangFile rebarConfig) {
    final List<String> dependencyAppNames = ContainerUtil.newArrayList();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "deps", tuplesList -> {
      List<ErlangTupleExpression> dependencyTuples = ErlangTermFileUtil.findNamedTuples(tuplesList);
      for (ErlangTupleExpression namedTuple : dependencyTuples) {
        dependencyAppNames.add(ErlangTermFileUtil.getNameOfNamedTuple(namedTuple));
      }
    });
    return dependencyAppNames;
  }

  @NotNull
  public static List<String> getParseTransforms(@Nullable ErlangFile rebarConfig) {
    final List<String> parseTransforms = ContainerUtil.newArrayList();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "erl_opts", section -> ErlangTermFileUtil.processConfigSection(section, "parse_transform", configExpression -> {
      ErlangQAtom parseTransform = PsiTreeUtil.getChildOfType(configExpression, ErlangQAtom.class);
      ErlangAtom parseTransformAtom = parseTransform != null ? parseTransform.getAtom() : null;
      if (parseTransformAtom != null) {
        parseTransforms.add(parseTransformAtom.getName());
      }
    }));
    return parseTransforms;
  }

  @NotNull
  private static String getStringLiteralText(@NotNull ErlangStringLiteral literal) {
    return StringUtil.unquoteString(literal.getString().getText());
  }

  @Nullable
  public static ErlangFile getRebarConfig(@NotNull Project project, @Nullable VirtualFile otpAppRoot) {
    VirtualFile rebarConfig = otpAppRoot != null ? otpAppRoot.findChild("rebar.config") : null;
    PsiFile rebarConfigPsi = rebarConfig != null && !rebarConfig.isDirectory() ? PsiManager.getInstance(project).findFile(rebarConfig) : null;
    return rebarConfigPsi instanceof ErlangFile ? (ErlangFile) rebarConfigPsi : null;
  }
}
