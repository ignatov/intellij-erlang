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

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public final class RebarConfigUtil {
  private RebarConfigUtil() {
  }

  @NotNull
  public static List<String> getIncludePaths(@NotNull ErlangFile rebarConfig) {
    final List<String> includePaths = new SmartList<>();
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
    final List<String> dependencyAppNames = new SmartList<>();
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
    final List<String> parseTransforms = new SmartList<>();
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
