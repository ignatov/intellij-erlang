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

import java.util.List;

public final class RebarConfigUtil {
  private RebarConfigUtil() {
  }

  @NotNull
  public static List<String> getIncludePaths(@NotNull ErlangFile rebarConfig) {
    final List<String> includePaths = ContainerUtil.newArrayList();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "erl_opts", new Consumer<ErlangExpression>() {
      @Override
      public void consume(ErlangExpression section) {
        ErlangTermFileUtil.processConfigSection(section, "i", new Consumer<ErlangExpression>() {
          @Override
          public void consume(ErlangExpression includeOptionValue) {
            if (includeOptionValue instanceof ErlangStringLiteral) {
              includePaths.add(getStringLiteralText((ErlangStringLiteral) includeOptionValue));
            }
            else {
              for (ErlangStringLiteral includePath : PsiTreeUtil.findChildrenOfType(includeOptionValue, ErlangStringLiteral.class)) {
                includePaths.add(getStringLiteralText(includePath));
              }
            }
          }
        });
      }
    });
    return includePaths;
  }

  @NotNull
  public static List<String> getDependencyAppNames(@NotNull ErlangFile rebarConfig) {
    final List<String> dependencyAppNames = ContainerUtil.newArrayList();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "deps", new Consumer<ErlangExpression>() {
      @Override
      public void consume(ErlangExpression tuplesList) {
        List<ErlangTupleExpression> dependencyTuples = ErlangTermFileUtil.findNamedTuples(tuplesList);
        for (ErlangTupleExpression namedTuple : dependencyTuples) {
          dependencyAppNames.add(ErlangTermFileUtil.getNameOfNamedTuple(namedTuple));
        }
      }
    });
    return dependencyAppNames;
  }

  @NotNull
  public static List<String> getParseTransforms(@Nullable ErlangFile rebarConfig) {
    final List<String> parseTransforms = ContainerUtil.newArrayList();
    ErlangTermFileUtil.processConfigSection(rebarConfig, "erl_opts", new Consumer<ErlangExpression>() {
      @Override
      public void consume(ErlangExpression section) {
        ErlangTermFileUtil.processConfigSection(section, "parse_transform", new Consumer<ErlangExpression>() {
          @Override
          public void consume(ErlangExpression configExpression) {
            ErlangQAtom parseTransform = PsiTreeUtil.getChildOfType(configExpression, ErlangQAtom.class);
            PsiElement parseTransformAtom = parseTransform != null ? parseTransform.getAtom() : null;
            if (parseTransformAtom != null) {
              parseTransforms.add(parseTransformAtom.getText());
            }
          }
        });
      }
    });
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
