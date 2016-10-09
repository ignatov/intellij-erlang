/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.refactoring;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.move.moveFilesOrDirectories.MoveFilesOrDirectoriesProcessor;
import com.intellij.testFramework.fixtures.CodeInsightFixtureTestCase;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangMoveHeaderTest extends CodeInsightFixtureTestCase {
  public void testMoveFromIncludeDirToModuleDir() {
    PsiFile includeFile = myFixture.addFileToProject("include/a.hrl", "");
    PsiDirectory includeDir = includeFile.getParent();
    markAsIncludeDirectory(includeDir);

    String moduleText = "-module(a).\n-include(\"a.hrl\").";
    PsiFile moduleFile = myFixture.configureByText("a.erl", moduleText);
    PsiDirectory moduleDir = moduleFile.getParent();

    move(includeFile, moduleDir);

    assertEquals(moduleText, moduleFile.getText());
  }

  public void testMoveFromIncludeDirToIncludeDirChild() {
    PsiFile includeFile = myFixture.addFileToProject("include/a.hrl", "");
    PsiDirectory includeDir = includeFile.getParent();
    markAsIncludeDirectory(includeDir);
    PsiDirectory includeDirChild = createSubdirectory(includeDir, "child");

    PsiFile moduleFile = myFixture.configureByText("a.erl", "-module(a).\n-include(\"a.hrl\").");

    move(includeFile, includeDirChild);

    assertEquals("-module(a).\n-include(\"child/a.hrl\").", moduleFile.getText());
  }

  public void testMoveFromModuleDirToIncludeDir() {
    PsiFile includeFile = myFixture.addFileToProject("a.hrl", "");

    String moduleText = "-module(a).\n-include(\"a.hrl\").";
    PsiFile moduleFile = myFixture.configureByText("a.erl", moduleText);

    PsiDirectory includeDir = createSubdirectory(moduleFile.getParent(), "include");
    markAsIncludeDirectory(includeDir);

    move(includeFile, includeDir);

    assertEquals(moduleText, moduleFile.getText());
  }

  public void testMoveFromModuleDirToChildDir() {
    PsiFile includeFile = myFixture.addFileToProject("a.hrl", "");
    PsiFile moduleFile = myFixture.configureByText("a.erl", "-module(a).\n-include(\"a.hrl\").");
    PsiDirectory childDir = createSubdirectory(moduleFile.getParent(), "child");
    move(includeFile, childDir);
    assertEquals("-module(a).\n-include(\"child/a.hrl\").", moduleFile.getText());
  }

  private void move(@Nullable PsiFile what, @Nullable PsiDirectory where) {
    assertNotNull(what);
    assertNotNull(where);

    Project project = getProject();
    PsiElement[] files = {what};
    new MoveFilesOrDirectoriesProcessor(project, files, where, true, false, false, null, null).run();
  }

  private void markAsIncludeDirectory(@Nullable final PsiDirectory dir) {
    assertNotNull(dir);
    ApplicationManager.getApplication().runWriteAction(() -> ErlangIncludeDirectoryUtil.markAsIncludeDirectory(myModule, dir.getVirtualFile()));
  }

  @NotNull
  private static PsiDirectory createSubdirectory(@Nullable final PsiDirectory dir, @Nullable final String name) {
    assertNotNull(dir);
    assertNotNull(name);
    return ApplicationManager.getApplication().runWriteAction((Computable<PsiDirectory>) () -> dir.createSubdirectory(name));
  }
}
