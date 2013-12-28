package org.intellij.erlang.inspection;

import com.intellij.codeInspection.*;
import com.intellij.codeInspection.ex.DisableInspectionToolAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangModuleType;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.intellij.erlang.psi.ErlangFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangFacetConfigurationIssueInspection extends LocalInspectionTool {
  @Nullable
  @Override
  public ProblemDescriptor[] checkFile(@NotNull final PsiFile psiFile, @NotNull final InspectionManager manager, final boolean isOnTheFly) {
    if (!(psiFile instanceof ErlangFile)) return ProblemDescriptor.EMPTY_ARRAY;
    VirtualFile file = psiFile.getVirtualFile();
    final Module module = file != null ? ModuleUtilCore.findModuleForFile(file, psiFile.getProject()) : null;
    if (module == null) return ProblemDescriptor.EMPTY_ARRAY;
    ErlangFacet facet = ErlangFacet.getFacet(module);
    if (facet == null) return createProblemDescriptors(psiFile, "Erlang facet is not configured.", manager, isOnTheFly);
    List<String> notIncludedPaths = getIncludeFoldersNotInIncludePath(module, facet.getConfiguration());
    if (!notIncludedPaths.isEmpty()) {
      return createProblemDescriptors(psiFile, "Some 'include' folders are not marked as include paths.", manager, isOnTheFly);
    }
    return ProblemDescriptor.EMPTY_ARRAY;
  }

  private static ProblemDescriptor[] createProblemDescriptors(PsiFile psiFile, String message, InspectionManager manager, boolean isOnTheFly) {
    return new ProblemDescriptor[]{
      manager.createProblemDescriptor(
        psiFile, message, new LocalQuickFix[]{new ErlangFacetQuickFix(true), new ErlangFacetQuickFix(false)},
        ProblemHighlightType.GENERIC_ERROR_OR_WARNING, isOnTheFly, false)
    };
  }

  private static List<String> getIncludeFoldersNotInIncludePath(Module module, ErlangFacetConfiguration configuration) {
    final List<String> includePaths = configuration.getIncludePaths();
    List<String> includeFolderPaths = ErlangFacetConfiguration.getIncludeFolderPaths(module);
    return ContainerUtil.filter(includeFolderPaths, new Condition<String>() {
      @Override
      public boolean value(String includeFolderPath) {
        return !includePaths.contains(includeFolderPath);
      }
    });
  }

  private static class ErlangFacetQuickFix implements LocalQuickFix {
    private boolean myDoFix;
    private final String myName;

    protected ErlangFacetQuickFix(boolean doFix) {
      myName = doFix ? "Setup facets" : "Dismiss";
      myDoFix = doFix;
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      if (myDoFix) {
        for (Module module : ModuleManager.getInstance(project).getModules()) {
          doFix(module);
        }
      }
      else {
        new DisableInspectionToolAction(new ErlangFacetConfigurationIssueInspection()).invoke(project, null, descriptor.getPsiElement().getContainingFile());
      }
    }

    @NotNull
    @Override
    public String getName() {
      return myName;
    }

    @NotNull
    @Override
    public String getFamilyName() {
      return myName;
    }

    private static void doFix(@NotNull Module module) {
      if (ModuleType.get(module) != ErlangModuleType.getInstance()) return;
      ErlangFacet facet = ErlangFacet.getFacet(module);
      if (facet == null) {
        ErlangFacet.createFacet(module);
      }
      else {
        ErlangFacetConfiguration configuration = facet.getConfiguration();
        configuration.addIncludeDirectoriesToIncludePath(module);
      }
    }
  }
}
