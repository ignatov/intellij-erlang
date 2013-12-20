package org.intellij.erlang.rebar.util;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Consumer;
import com.intellij.util.Function;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.util.List;

public final class ErlangTermFileUtil {
  private ErlangTermFileUtil() {
  }

  @NotNull
  public static List<ErlangTupleExpression> findNamedTuples(@Nullable ErlangExpression listExpression) {
    return findNamedTuples(listExpression, null);
  }

  @NotNull
  public static List<ErlangTupleExpression> findNamedTuples(@Nullable ErlangExpression listExpression, @Nullable String name) {
    ErlangListExpression propList = listExpression instanceof ErlangListExpression ? (ErlangListExpression) listExpression : null;
    return propList != null ? findNamedTuples(propList.getExpressionList(), name) : ContainerUtil.<ErlangTupleExpression>emptyList();
  }

  @NotNull
  public static List<ErlangTupleExpression> findNamedTuples(@NotNull List<ErlangExpression> configExpressions, @Nullable final String name) {
    return ContainerUtil.mapNotNull(configExpressions, new Function<ErlangExpression, ErlangTupleExpression>() {
      @Nullable
      @Override
      public ErlangTupleExpression fun(ErlangExpression expression) {
        String tupleName = getNameOfNamedTuple(expression);
        boolean isValidName = name == null && tupleName != null || name != null && name.equals(tupleName);
        return isValidName ? (ErlangTupleExpression) expression : null;
      }
    });
  }

  @Nullable
  public static String getNameOfNamedTuple(@Nullable ErlangExpression expression) {
    ErlangTupleExpression tupleExpression = expression instanceof ErlangTupleExpression ? (ErlangTupleExpression) expression : null;
    List<ErlangExpression> expressions = tupleExpression != null ? tupleExpression.getExpressionList() : null;
    ErlangExpression configExpression = expressions != null && !expressions.isEmpty() ? expressions.get(0) : null;
    PsiElement nameQAtom = configExpression instanceof ErlangConfigExpression ? configExpression.getFirstChild() : null;
    PsiElement atom = nameQAtom instanceof ErlangQAtom ? ((ErlangQAtom) nameQAtom).getAtom() : null;
    return atom != null ? atom.getText() : null;
  }

  @Nullable
  public static ErlangFile createPsi(@NotNull VirtualFile file) {
    if (file.getFileType() != ErlangFileType.APP && file.getFileType() != ErlangFileType.TERMS) return null;
    try {
      String text = StringUtil.convertLineSeparators(VfsUtilCore.loadText(file));
      Project defaultProject = ProjectManager.getInstance().getDefaultProject();
      return (ErlangFile) PsiFileFactory.getInstance(defaultProject).createFileFromText(file.getName(), file.getFileType(), text);
    } catch (IOException e) {
      return null;
    }
  }

  public static void processConfigSection(@Nullable PsiElement configRoot, @NotNull String sectionName, @NotNull Consumer<ErlangExpression> sectionConsumer) {
    List<ErlangTupleExpression> erlOptTuples = findNamedTuples(PsiTreeUtil.getChildrenOfTypeAsList(configRoot, ErlangExpression.class), sectionName);
    for (ErlangTupleExpression erlOptTuple : erlOptTuples) {
      List<ErlangExpression> expressions = erlOptTuple.getExpressionList();
      ErlangExpression optionsList = expressions.size() >= 2 ? expressions.get(1) : null;
      if (optionsList == null) continue;
      sectionConsumer.consume(optionsList);
    }
  }
}
