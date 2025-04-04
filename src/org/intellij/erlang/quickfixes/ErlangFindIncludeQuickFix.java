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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectUtil;
import com.intellij.openapi.ui.popup.*;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiEditorUtil;
import com.intellij.util.FileContentUtilCore;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * @author mark-dev
 */
public class ErlangFindIncludeQuickFix extends ErlangQuickFixBase {
  private static final char INCLUDE_STRING_PATH_SEPARATOR = '/';
  /*
   * if true after adding facets include string will be renamed to direct link on hrl file
   * eg: -include("pr285_helper/include/internal_communication.hrl")
   * will be renamed to -include("internal_communication.hrl").
   */
  private final boolean setDirectHrlLink;


  public ErlangFindIncludeQuickFix(boolean setDirectHrlLink) {
    this.setDirectHrlLink = setDirectHrlLink;
  }

  @NotNull
  @Override
  public String getFamilyName() {
    return "Find include";
  }

  public void applyFix(@NotNull Project project,
                       @NotNull ProblemDescriptor problemDescriptor) {

    PsiElement problem = problemDescriptor.getPsiElement();
    if (problem == null) return;

    //Looks for a file that is referenced by include string
    String includeString = StringUtil.unquoteString(problem.getText());
    String includeFileName = getFileName(includeString);
    PsiFile[] matchFiles = searchFileInsideProject(project, includeFileName);
    if (matchFiles.length == 0) {
      return;
    }

    ApplicationManager.getApplication().assertIsDispatchThread();

    //Single file found
    if (matchFiles.length == 1) {
      CommandProcessor.getInstance().executeCommand(project, () -> ApplicationManager.getApplication().runWriteAction(() -> {
        fixUsingIncludeFile(problem, matchFiles[0]);
        renameIncludeString(project, problem, setDirectHrlLink, includeString, includeFileName);
        FileContentUtilCore.reparseFiles(Collections.singletonList(problem.getContainingFile().getVirtualFile()));
      }), "Include File", "Include File");
    }
    //Multiple files -- allow user select which file should be imported
    if (matchFiles.length > 1) {
      displayPopupListDialog(project, problem, matchFiles, setDirectHrlLink, includeString, includeFileName);
    }
  }

  @Override
  public @NotNull IntentionPreviewInfo generatePreview(@NotNull Project project,
                                                       @NotNull ProblemDescriptor previewDescriptor) {
    return IntentionPreviewInfo.EMPTY;
  }

  private static void renameIncludeString(Project project,
                                          PsiElement problem,
                                          boolean setDirectHrlLink,
                                          String includeString,
                                          String includeFileName) {
    //Rename include string according setDirectHrlLink
    if (setDirectHrlLink && !includeString.equals(includeFileName)) {
      problem.replace(ErlangElementFactory.createIncludeString(project, includeFileName));
    }
  }

  private static void displayPopupListDialog(final Project project,
                                             final PsiElement problem,
                                             final PsiFile[] files,
                                             final boolean setDirectHrlLink,
                                             final String includeString,
                                             final String includeFileName
  ) {
    final Editor problemEditor = PsiEditorUtil.findEditor(problem);
    if (problemEditor == null) {
      return;
    }
    ListPopup p = JBPopupFactory.getInstance().createListPopup(new ListPopupStep<PsiFile>() {

      @NotNull
      @Override
      public List<PsiFile> getValues() {
        return Arrays.asList(files);
      }

      @Override
      public boolean isSelectable(PsiFile o) {
        return true;
      }

      @NotNull
      @Override
      public Icon getIconFor(PsiFile o) {
        return ErlangIcons.HEADER;
      }

      @NotNull
      @Override
      public String getTextFor(PsiFile o) {
        // Uses relative path to project root if possible (if not - full path)
        VirtualFile f = o.getVirtualFile();
        VirtualFile projectDir = ProjectUtil.guessProjectDir(project);
        String projectRootRelativePath = projectDir == null ? null : VfsUtilCore.getRelativePath(f, projectDir, INCLUDE_STRING_PATH_SEPARATOR);
        return projectRootRelativePath == null ? f.getPath() : projectRootRelativePath;
      }

      @Nullable
      @Override
      public ListSeparator getSeparatorAbove(PsiFile o) {
        return null;
      }

      @Override
      public int getDefaultOptionIndex() {
        return 0;
      }

      @NotNull
      @Override
      public String getTitle() {
        return "Multiple Files Found";
      }

      @Nullable
      @Override
      public PopupStep<PsiFile> onChosen(PsiFile o, boolean b) {
        CommandProcessor.getInstance().executeCommand(project, () -> ApplicationManager.getApplication().runWriteAction(() -> {
          fixUsingIncludeFile(problem, o);
          renameIncludeString(project, problem, setDirectHrlLink, includeString, includeFileName);
          FileContentUtilCore.reparseFiles(Collections.singletonList(problem.getContainingFile().getVirtualFile()));
        }), "Add Facet Action (Find Include Quick Fix)", "Include File", problemEditor.getDocument());

        return null;
      }

      @Override
      public boolean hasSubstep(PsiFile o) {
        return false;
      }

      @Override
      public void canceled() {
      }

      @Override
      public boolean isMnemonicsNavigationEnabled() {
        return false;
      }

      @Nullable
      @Override
      public MnemonicNavigationFilter<PsiFile> getMnemonicNavigationFilter() {
        return null;
      }

      @Override
      public boolean isSpeedSearchEnabled() {
        return false;
      }

      @Nullable
      @Override
      public SpeedSearchFilter<PsiFile> getSpeedSearchFilter() {
        return null;
      }

      @Override
      public boolean isAutoSelectionEnabled() {
        return false;
      }

      @Nullable
      @Override
      public Runnable getFinalRunnable() {
        return null;
      }
    });
    p.showInBestPositionFor(problemEditor);
  }

  private static void fixUsingIncludeFile(PsiElement problem,
                                          PsiFile includeFile) {
    //Search the module that contains the current(problem) file & fix facets
    Module containedModule = ModuleUtilCore.findModuleForPsiElement(problem);
    if (containedModule == null) return;
    ErlangIncludeDirectoryUtil.markAsIncludeDirectory(containedModule, includeFile.getVirtualFile().getParent());
  }

  /*
   * returns file name from includeString
   * eg:
   * getFileName("pr285_helper/include/internal_communication.hrl")
   *   -> "internal_communications.hrl"
   * getFileName("ecst_events.hrl")
   *   -> "ecst_events.hrl"
   * */
  private static String getFileName(String includeString) {
    int index = includeString.lastIndexOf(INCLUDE_STRING_PATH_SEPARATOR);
    return includeString.substring(index + 1);
  }

  private static PsiFile[] searchFileInsideProject(Project project, String fileName) {
    return FilenameIndex.getFilesByName(project, fileName, GlobalSearchScope.allScope(project));
  }

}
