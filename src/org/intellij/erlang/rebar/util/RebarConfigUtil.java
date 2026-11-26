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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Consumer;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public final class RebarConfigUtil {
  private RebarConfigUtil() {
  }

  @NotNull
  public static List<String> getIncludePaths(@NotNull ErlangFile rebarConfig) {
    final List<String> includePaths = new ArrayList<>();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "erl_opts", section -> ErlangTermFileUtil.processConfigSection(section, "i", includeOptionValue -> {
      if (includeOptionValue instanceof ErlangStringLiteral) {
        ContainerUtil.addIfNotNull(includePaths, getStringLiteralText((ErlangStringLiteral) includeOptionValue));
      }
      else {
        for (ErlangStringLiteral includePath : PsiTreeUtil.findChildrenOfType(includeOptionValue, ErlangStringLiteral.class)) {
          ContainerUtil.addIfNotNull(includePaths, getStringLiteralText(includePath));
        }
      }
    }));
    return includePaths;
  }

  /**
   * Returns the deps of the top level and of the profiles. rebar3 applies only the prod profile to a dependency,
   * so for the rebar.config of a dependency app only the deps of that profile are read.
   */
  @NotNull
  public static List<String> getDependencyAppNames(@NotNull ErlangFile rebarConfig, boolean isDependencyApp) {
    final List<String> dependencyAppNames = new ArrayList<>();
    Consumer<ErlangExpression> depsConsumer = depsList -> {
      if (!(depsList instanceof ErlangListExpression)) return;
      for (ErlangExpression dependency : ((ErlangListExpression) depsList).getExpressionList()) {
        String name = ErlangTermFileUtil.getNameOfNamedTuple(dependency);
        if (name == null && dependency instanceof ErlangConfigExpression) {
          PsiElement qAtom = dependency.getFirstChild();
          ErlangAtom atom = qAtom instanceof ErlangQAtom ? ((ErlangQAtom) qAtom).getAtom() : null;
          name = atom != null ? atom.getName() : null;
        }
        ContainerUtil.addIfNotNull(dependencyAppNames, name);
      }
    };
    ErlangTermFileUtil.processConfigSection(rebarConfig, "deps", depsConsumer);
    ErlangTermFileUtil.processConfigSection(rebarConfig, "profiles", profilesList -> {
      if (!(profilesList instanceof ErlangListExpression)) return;
      for (ErlangExpression profile : ((ErlangListExpression) profilesList).getExpressionList()) {
        String profileName = ErlangTermFileUtil.getNameOfNamedTuple(profile);
        if (profileName == null || isDependencyApp && !"prod".equals(profileName)) continue;
        List<ErlangExpression> profileTuple = ((ErlangTupleExpression) profile).getExpressionList();
        if (profileTuple.size() >= 2) {
          ErlangTermFileUtil.processConfigSection(profileTuple.get(1), "deps", depsConsumer);
        }
      }
    });
    return dependencyAppNames;
  }

  @NotNull
  public static List<String> getParseTransforms(@Nullable ErlangFile rebarConfig) {
    final List<String> parseTransforms = new ArrayList<>();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "erl_opts", section -> ErlangTermFileUtil.processConfigSection(section, "parse_transform", configExpression -> {
      ErlangQAtom parseTransform = PsiTreeUtil.getChildOfType(configExpression, ErlangQAtom.class);
      ErlangAtom parseTransformAtom = parseTransform != null ? parseTransform.getAtom() : null;
      if (parseTransformAtom != null) {
        parseTransforms.add(parseTransformAtom.getName());
      }
    }));
    return parseTransforms;
  }

  /**
   * Returns the text of a string or of a ~s/~S string sigil, or null for a binary or triple-quoted sigil.
   */
  @Nullable
  private static String getStringLiteralText(@NotNull ErlangStringLiteral literal) {
    PsiElement string = literal.getString();
    if (string != null) return StringUtil.unquoteString(string.getText());
    PsiElement sigil = literal.getSigilString();
    String sigilText = sigil != null ? sigil.getText() : "";
    boolean isStringSigil = sigilText.startsWith("~s") || sigilText.startsWith("~S");
    if (!isStringSigil || sigilText.length() < 4 || sigilText.startsWith("\"\"\"", 2)) return null;
    return sigilText.substring(3, sigilText.length() - 1);
  }

  @Nullable
  public static ErlangFile getRebarConfig(@NotNull Project project, @Nullable VirtualFile otpAppRoot) {
    VirtualFile rebarConfig = otpAppRoot != null ? otpAppRoot.findChild("rebar.config") : null;
    PsiFile rebarConfigPsi = rebarConfig != null && !rebarConfig.isDirectory() ? PsiManager.getInstance(project).findFile(rebarConfig) : null;
    return rebarConfigPsi instanceof ErlangFile ? (ErlangFile) rebarConfigPsi : null;
  }
}
