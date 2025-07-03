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

import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.intention.preview.IntentionPreviewInfo;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.UndoConfirmationPolicy;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectUtil;
import com.intellij.openapi.ui.popup.*;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiAnchor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiEditorUtil;
import com.intellij.util.FileContentUtilCore;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Collections;
import java.util.List;

/**
 * @author mark-dev
 */
public class ErlangFindIncludeQuickFix extends ErlangQuickFixBase {

  /**
   * Data class that holds a PsiAnchor for a file and its pre-calculated display path.
   * This avoids expensive calculations during UI rendering and prevents EDT issues.
   */
  private static class FileItem {
    final PsiAnchor fileAnchor;
    final String displayPath;

    FileItem(PsiAnchor fileAnchor, String displayPath) {
      this.fileAnchor = fileAnchor;
      this.displayPath = displayPath;
    }
  }

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
    Editor editor = PsiEditorUtil.findEditor(problem);
    if (editor == null) return;

    //Looks for a file that is referenced by include string
    String includeString = StringUtil.unquoteString(problem.getText());
    String includeFileName = getFileName(includeString);

    ProgressManager.getInstance().run(
      new Task.Backgroundable(project, editor.getComponent(), "Searching for include file '" + includeFileName + "'...", true, null) {
        @Override
        public void run(@NotNull ProgressIndicator indicator) {
          indicator.setIndeterminate(true);
          List<ErlangFile> files = ApplicationManager.getApplication().runReadAction(
            (Computable<List<ErlangFile>>) () -> {
              // pass indicator into the search
              return ErlangModuleIndex.getFilesByName(project, includeFileName, GlobalSearchScope.allScope(project));
            }
          );

          // Pre-compute all display paths and create FileItem objects
          // This work is done outside the EDT to avoid performance issues
          // during popup rendering
          final List<FileItem> fileItems = files.stream().map(file -> {
                                                  PsiAnchor anchor = PsiAnchor.create(file);
                                                  String displayPath = calcFilePath(file, project);
                                                  return new FileItem(anchor, displayPath);
                                                })
                                                .toList();


          ApplicationManager.getApplication().invokeLater(() -> {
            if (!files.isEmpty()) {
              displayPopupListDialog(project, problem, fileItems, setDirectHrlLink, includeString, includeFileName);
            }
            else {
              // Show editor notification that no files are found
              Editor editor = PsiEditorUtil.findEditor(problem);
              if (editor != null) {
                HintManager.getInstance().showErrorHint(editor, "No files matching '" + includeFileName + "' were found");
              }
            }
          });
        }
      }
    );
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
                                             final List<FileItem> fileItems,
                                             final boolean setDirectHrlLink,
                                             final String includeString,
                                             final String includeFileName
  ) {
    final Editor problemEditor = PsiEditorUtil.findEditor(problem);
    if (problemEditor == null) return;

    ListPopup p = JBPopupFactory.getInstance().createListPopup(new ListPopupStep<FileItem>() {
      @NotNull
      @Override
      public List<FileItem> getValues() {
        return fileItems;
      }

      @Override
      public boolean isSelectable(FileItem item) {
        return true;
      }

      @NotNull
      @Override
      public Icon getIconFor(FileItem item) {
        if (item.displayPath.endsWith("hrl")) {
          return ErlangIcons.HEADER;
        }
        else {
          return ErlangIcons.FILE;
        }
      }

      @NotNull
      @Override
      public String getTextFor(FileItem item) {
        // Return the pre-calculated display path
        return item.displayPath;
      }

      @Nullable
      @Override
      public ListSeparator getSeparatorAbove(FileItem item) {
        return null;
      }

      @Override
      public int getDefaultOptionIndex() {
        return 0;
      }

      @NotNull
      @Override
      public String getTitle() {
        return StringUtil.pluralize("File") + " to Include";
      }

      @Nullable
      @Override
      public PopupStep<FileItem> onChosen(FileItem item, boolean finalChoice) {
        WriteCommandAction.writeCommandAction(project, problem.getContainingFile())
          .withName("Include File Fix")
          .withGroupId("Include File")
          .withUndoConfirmationPolicy(UndoConfirmationPolicy.DEFAULT)
          .run(() -> {
            PsiElement element = item.fileAnchor.retrieve();
            if (element instanceof PsiFile) {
              fixUsingIncludeFile(problem, (PsiFile) element);
              renameIncludeString(project, problem, setDirectHrlLink, includeString, includeFileName);
              FileContentUtilCore.reparseFiles(Collections.singletonList(problem.getContainingFile().getVirtualFile()));
            }
          });

        return null;
      }

      @Override
      public boolean hasSubstep(FileItem item) {
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
      public MnemonicNavigationFilter<FileItem> getMnemonicNavigationFilter() {
        return null;
      }

      @Override
      public boolean isSpeedSearchEnabled() {
        return false;
      }

      @Nullable
      @Override
      public SpeedSearchFilter<FileItem> getSpeedSearchFilter() {
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

  private static @NotNull String calcFilePath(PsiFile file, Project project) {
    if (file == null) return "<invalid file>";

    VirtualFile f = file.getVirtualFile();
    if (f == null) return "<unknown path>";

    VirtualFile projectDir = ProjectUtil.guessProjectDir(project);
    String projectRootRelativePath = projectDir == null ? null : VfsUtilCore.getRelativePath(f, projectDir, INCLUDE_STRING_PATH_SEPARATOR);
    return projectRootRelativePath == null ? f.getPath() : projectRootRelativePath;
  }

  private static void fixUsingIncludeFile(PsiElement problem,
                                          PsiFile includeFile) {
    //Search the module that contains the current(problem) file & fix facets
    Module containedModule = ModuleUtilCore.findModuleForPsiElement(problem);
    if (containedModule == null) return;

    VirtualFile includeFileParent = includeFile.getVirtualFile() != null ?
                                    includeFile.getVirtualFile().getParent() : null;
    if (includeFileParent != null) {
      ErlangIncludeDirectoryUtil.markAsIncludeDirectory(containedModule, includeFileParent);
    }
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
}
