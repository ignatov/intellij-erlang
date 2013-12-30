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
import com.intellij.openapi.module.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileFilter;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.ErlangModuleType;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.facet.ErlangFacetConfiguration;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;

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
    return "try find include";
  }

  public void applyFix(@NotNull Project project,
                       @NotNull ProblemDescriptor problemDescriptor) {

    PsiElement problem = problemDescriptor.getPsiElement();
    if (problem == null) return;

    //Looks for a file that is referenced by include string
    String includeString = StringUtil.unquoteString(problem.getText());
    String includeFileName = getFileName(includeString);
    VirtualFile includeFile = searchFileInsideProject(project, includeFileName);
    if (includeFile == null) return;

    //Search the module that contains the current file & fix facets
    Module containedModule = ModuleUtilCore.findModuleForPsiElement(problem);
    if (containedModule == null) return;
    addToModuleFacet(containedModule, includeFile);

    //Rename include string according setDirectHrlLink
    if (setDirectHrlLink && !includeString.equals(includeFileName)) {
      problem.replace(ErlangElementFactory.createStringFromText(project, includeFileName));
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

  private static VirtualFile searchFileInsideProject(Project project, String fileName) {
    RecursiveSearchFileIterator fi = new RecursiveSearchFileIterator(fileName);
    VfsUtilCore.iterateChildrenRecursively(project.getBaseDir(), VirtualFileFilter.ALL, fi);
    return fi.getResult();
  }

  private static void addToModuleFacet(Module module, VirtualFile includeFile) {
    //Force create facet if facet for module not found
    if (ModuleType.get(module) != ErlangModuleType.getInstance()) return;
    ErlangFacet facet = ErlangFacet.getFacet(module);
    if (facet == null) {
      ErlangFacet.createFacet(module);
      facet = ErlangFacet.getFacet(module);
    }

    if (facet != null) {
      ErlangFacetConfiguration configuration = facet.getConfiguration();
      Collection<String> includeString = new ArrayList<String>();
      includeString.add(includeFile.getParent().getCanonicalPath());
      configuration.addIncludePaths(includeString);
    }
  }


  private static class RecursiveSearchFileIterator implements ContentIterator {
    private VirtualFile result;
    private String expectedFilename;

    private RecursiveSearchFileIterator(String expectedFilename) {
      this.expectedFilename = expectedFilename;
    }

    @Override
    public boolean processFile(VirtualFile virtualFile) {
      if (virtualFile.getName().equals(expectedFilename)) {
        result = virtualFile;
        return false;
      }
      return true;
    }

    public VirtualFile getResult() {
      return result;
    }
  }
}


