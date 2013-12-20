package org.intellij.erlang.eunit;

import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class ErlangUnitTestElementUtil {
  private ErlangUnitTestElementUtil() {
  }

  @NotNull
  public static Collection<ErlangFunction> findFunctionTestElements(@Nullable PsiElement element) {
    return ContainerUtil.createMaybeSingletonList(getZeroArityFunction(element));
  }

  public static Collection<ErlangFile> findFileTestElements(Project project, DataContext dataContext) {
    VirtualFile[] selectedFiles = CommonDataKeys.VIRTUAL_FILE_ARRAY.getData(dataContext);

    if (selectedFiles == null) return Collections.emptyList();

    List<ErlangFile> testFiles = new ArrayList<ErlangFile>(selectedFiles.length);
    for (VirtualFile file : selectedFiles) {
      PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
      if (psiFile instanceof ErlangFile) {
        testFiles.add((ErlangFile) psiFile);
      }
    }
    return testFiles;
  }

  @Nullable
  public static ErlangFunction getZeroArityFunction(@Nullable PsiElement psiElement) {
    ErlangFunction function = psiElement instanceof ErlangFunction ? (ErlangFunction)psiElement : PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    int arity = function == null ? -1 : function.getArity();
    return 0 == arity ? function : null;
  }
}
