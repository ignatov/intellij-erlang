/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.popup.*;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.FileContentUtilCore;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Arrays;
import java.util.List;

/**
 * @author mark-dev
 */
public class ErlangFindIncludeQuickFix extends ErlangQuickFixBase {
  private static char INCLUDE_STRING_PATH_SEPARATOR = '/';
  /*
   * if true after adding facets include string will be renamed to direct link on hrl file
   * eg: -include("pr285_helper/include/internal_communication.hrl")
   * will be renamed to -include("internal_communication.hrl").
   */
  private boolean setDirectHrlLink;


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
    //Single file found
    if (matchFiles.length == 1) {
      fixUsingIncludeFile(problem, matchFiles[0]);
      renameIncludeString(project, problem, setDirectHrlLink, includeString, includeFileName);
    }
    //Multiple files -- allow user select which file should be imported
    if (matchFiles.length > 1) {
      displayPopupListDialog(project, problem, matchFiles, setDirectHrlLink, includeString, includeFileName);
    }


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
    final Editor problemEditor = PsiUtilBase.findEditor(problem);
    if (problemEditor == null) {
      return;
    }
    ListPopup p = JBPopupFactory.getInstance().createListPopup(new ListPopupStep() {

      @NotNull
      @Override
      public List getValues() {
        return Arrays.asList(files);
      }

      @Override
      public boolean isSelectable(Object o) {
        return true;
      }

      @Nullable
      @Override
      public Icon getIconFor(Object o) {
        return ErlangIcons.HEADER;
      }

      @NotNull
      @Override
      public String getTextFor(Object o) {
        //Uses relative path to project root if possible (if not - full path)
        VirtualFile f = ((PsiFile) o).getVirtualFile();
        String projectRootRelativePath = VfsUtilCore.getRelativePath(f, project.getBaseDir(), INCLUDE_STRING_PATH_SEPARATOR);
        return projectRootRelativePath == null ?
          f.getPath() : projectRootRelativePath;
      }

      @Nullable
      @Override
      public ListSeparator getSeparatorAbove(Object o) {
        return null;
      }

      @Override
      public int getDefaultOptionIndex() {
        return 0;
      }

      @Nullable
      @Override
      public String getTitle() {
        return "Multiple files found";
      }

      @Nullable
      @Override
      public PopupStep onChosen(Object o, boolean b) {
        final PsiFile f = (PsiFile) o;
        CommandProcessor.getInstance().executeCommand(project, new Runnable() {
          @Override
          public void run() {
            ApplicationManager.getApplication().runWriteAction(new Runnable() {
              @Override
              public void run() {
                fixUsingIncludeFile(problem, f);
                renameIncludeString(project, problem, setDirectHrlLink, includeString, includeFileName);
                FileContentUtilCore.reparseFiles(Arrays.asList(problem.getContainingFile().getVirtualFile()));
              }
            });
          }
        }, "add facet action(find include quick fix)", null, problemEditor.getDocument());

        return null;
      }

      @Override
      public boolean hasSubstep(Object o) {
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
      public MnemonicNavigationFilter getMnemonicNavigationFilter() {
        return null;
      }

      @Override
      public boolean isSpeedSearchEnabled() {
        return false;
      }

      @Nullable
      @Override
      public SpeedSearchFilter getSpeedSearchFilter() {
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
                                          final PsiFile includeFile) {
    //Search the module that contains the current(problem) file & fix facets
    final Module containedModule = ModuleUtilCore.findModuleForPsiElement(problem);
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


