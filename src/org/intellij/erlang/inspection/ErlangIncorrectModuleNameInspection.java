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

package org.intellij.erlang.inspection;

import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.openapi.application.AccessToken;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class ErlangIncorrectModuleNameInspection extends ErlangInspectionBase {
  @Override
  protected void checkFile(PsiFile file, final ProblemsHolder problemsHolder) {
    if (!(file instanceof ErlangFile)) return;
    file.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitModule(@NotNull ErlangModule o) {
        String ext = FileUtilRt.getExtension(o.getContainingFile().getName());
        String withoutExtension = FileUtil.getNameWithoutExtension(o.getContainingFile().getName());
        String moduleName = StringUtil.replace(o.getName(), "'", "");
        ErlangCompositeElement atom = o.getQAtom();
        if (atom != null && !StringUtil.equals(moduleName, withoutExtension)) {
          problemsHolder.registerProblem(atom, "Module with name '" + moduleName + "' should be declared in a file named '" +
            moduleName + "." + ext + "'.",
            new ErlangRenameModuleFix(o, withoutExtension),
            new ErlangRenameFileFix(o)
          );
        }
      }
    });
  }

  private static class ErlangRenameModuleFix implements LocalQuickFix {
    private final ErlangModule myModule;
    private final String myShouldBeName;

    public ErlangRenameModuleFix(ErlangModule module, String shouldBeName) {
      myModule = module;
      myShouldBeName = shouldBeName;
    }

    @NotNull
    @Override
    public String getName() {
      return "Rename module";
    }

    @NotNull
    @Override
    public String getFamilyName() {
      return "Rename module";
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      final AccessToken token = WriteAction.start();
      String name;
      try {
        ErlangElementFactory.createQAtomFromText(project, myShouldBeName);
        name = myShouldBeName;
      } catch (Exception e) {
        name = "'" + myShouldBeName + "'";
      }
      try {
        myModule.setName(name);
      } finally {
        token.finish();
      }
    }
  }

  private static class ErlangRenameFileFix implements LocalQuickFix {
    private final ErlangModule myModule;
    private final String myExtension;

    public ErlangRenameFileFix(ErlangModule module) {
      myModule = module;
      myExtension = FileUtilRt.getExtension(myModule.getContainingFile().getName());
    }

    @NotNull
    @Override
    public String getName() {
      return "Rename file";
    }

    @NotNull
    @Override
    public String getFamilyName() {
      return "Rename containing file";
    }

    @Override
    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor problemDescriptor) {
      final AccessToken token = WriteAction.start();
      try {
        VirtualFile virtualFile = myModule.getContainingFile().getVirtualFile();
        if (virtualFile != null) {
          virtualFile.rename(problemDescriptor, StringUtil.replace(myModule.getName(), "'", "") + "." + myExtension);
        }
      } catch (IOException e) { //
      } finally {
        token.finish();
      }
    }
  }
}
