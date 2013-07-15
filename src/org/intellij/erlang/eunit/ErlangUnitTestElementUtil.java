package org.intellij.erlang.eunit;

import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.SmartList;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * @author savenko
 */
public class ErlangUnitTestElementUtil {

  private ErlangUnitTestElementUtil() {
  }

  public static Collection<ErlangFunction> findFunctionTestElements(PsiElement element) {
    //TODO support multiple functions selection
    SmartList<ErlangFunction> selectedFunctions = new SmartList<ErlangFunction>();
    ErlangFunction function = getParentNullaryFunction(element);

    if (function != null) {
      selectedFunctions.add(function);
    }
    return selectedFunctions;
  }

  public static Collection<ErlangFile> findFileTestElements(Project project, DataContext dataContext) {
    VirtualFile[] selectedFiles = PlatformDataKeys.VIRTUAL_FILE_ARRAY.getData(dataContext);

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
  public static ErlangFunction getParentNullaryFunction(PsiElement psiElement) {
    ErlangFunction function = psiElement instanceof ErlangFunction ? (ErlangFunction)psiElement : PsiTreeUtil.getParentOfType(psiElement, ErlangFunction.class);
    int arity = function != null ? function.getArity() : -1;
    return 0 == arity ? function : null;
  }
}
