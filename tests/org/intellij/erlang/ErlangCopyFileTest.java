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

package org.intellij.erlang;

import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.util.ThrowableComputable;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.copy.CopyFilesOrDirectoriesHandler;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.Nullable;

public class ErlangCopyFileTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testSimple() throws Throwable {
    final PsiFile file = myFixture.configureByText("aaa.erl", "-module(aaa).\nfoo() -> ok.");
    final PsiDirectory containingDirectory = file.getContainingDirectory();
    assert containingDirectory != null;
    final String newName = "bbb.erl";
    WriteCommandAction.runWriteCommandAction(myFixture.getProject(), new ThrowableComputable<Object, Throwable>() {
      @Nullable
      @Override
      public Object compute() throws Throwable {
        return CopyFilesOrDirectoriesHandler.copyToDirectory(file, newName, containingDirectory);
      }
    });
    PsiFile copiedFile = containingDirectory.findFile(newName);
    assert copiedFile != null;
    assertEquals(copiedFile.getText(), "-module(bbb).\nfoo() -> ok.");
  }
}
