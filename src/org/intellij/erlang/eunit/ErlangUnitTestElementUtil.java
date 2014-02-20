package org.intellij.erlang.eunit;

import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
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
    if (element != null) {
      PsiFile containingFile = element.getContainingFile();
      if (getFileTestElement(containingFile) == null) {
        return ContainerUtil.emptyList();
      }
    }
    return ContainerUtil.createMaybeSingletonList(getZeroArityFunction(element));
  }

  public static Collection<ErlangFile> findFileTestElements(Project project, DataContext dataContext) {
    VirtualFile[] selectedFiles = CommonDataKeys.VIRTUAL_FILE_ARRAY.getData(dataContext);

    if (selectedFiles == null) return Collections.emptyList();

    List<ErlangFile> testFiles = new ArrayList<ErlangFile>(selectedFiles.length);
    PsiManager psiManager = PsiManager.getInstance(project);
    for (VirtualFile file : selectedFiles) {
      ContainerUtil.addIfNotNull(testFiles, getFileTestElement(psiManager.findFile(file)));
      PsiDirectory directory = psiManager.findDirectory(file);
      if (directory != null) {
        addTestFilesFromDirectory(testFiles, directory);
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

  private static void addTestFilesFromDirectory(List<ErlangFile> files, PsiDirectory directory) {
    for (PsiFile psiFile : directory.getFiles()) {
      ContainerUtil.addIfNotNull(files, getFileTestElement(psiFile));
    }
    for (PsiDirectory psiDirectory : directory.getSubdirectories()) {
      addTestFilesFromDirectory(files, psiDirectory);
    }
  }

  @Nullable
  private static ErlangFile getFileTestElement(@Nullable PsiFile psiFile) {
    return psiFile instanceof ErlangFile && ErlangPsiImplUtil.isEunitImported((ErlangFile) psiFile) ? (ErlangFile) psiFile : null;
  }
}
