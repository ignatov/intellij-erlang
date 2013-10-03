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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Set;

final class ImportedOtpApp {
  private final String myName;
  private final VirtualFile myRoot;
  private final Set<String> myDeps = ContainerUtil.newHashSet();
  private final Set<String> myIncludePaths = ContainerUtil.newHashSet();
  private VirtualFile myIdeaModuleFile;
  private Module myModule;

  public ImportedOtpApp(@NotNull VirtualFile root, @NotNull VirtualFile appConfig) {
    myName = StringUtil.trimEnd(StringUtil.trimEnd(appConfig.getName(), ".src"), ".app");
    myRoot = root;
    addDependenciesFromAppFile(appConfig);
    addInfoFromRebarConfig();
  }

  @NotNull
  public String getName() {
    return myName;
  }

  @NotNull
  public VirtualFile getRoot() {
    return myRoot;
  }

  @NotNull
  public Set<String> getDeps() {
    return myDeps;
  }

  public Set<String> getIncludePaths() {
    return myIncludePaths;
  }

  public void setIdeaModuleFile(@Nullable VirtualFile ideaModuleFile) {
    myIdeaModuleFile = ideaModuleFile;
  }

  @Nullable
  public VirtualFile getIdeaModuleFile() {
    return myIdeaModuleFile;
  }

  public Module getModule() {
    return myModule;
  }

  public void setModule(Module module) {
    myModule = module;
  }

  @Override
  public String toString() {
    return myName + " (" + myRoot + ')';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    ImportedOtpApp that = (ImportedOtpApp) o;

    if (!myName.equals(that.myName)) return false;
    if (!myRoot.equals(that.myRoot)) return false;

    return true;
  }

  @Override
  public int hashCode() {
    int result = myName.hashCode();
    result = 31 * result + myRoot.hashCode();
    return result;
  }

  private void addInfoFromRebarConfig() {
    VirtualFile rebarConfig = myRoot.findChild("rebar.config");
    ErlangFile rebarConfigPsi = rebarConfig != null ? ErlangTermFileUtil.createPsi(rebarConfig) : null;
    if (rebarConfigPsi == null) return;
    addDependenciesFromRebarConfig(rebarConfigPsi);
    addIncludePathsFromRebarConfig(rebarConfigPsi);
  }

  private void addDependenciesFromAppFile(@NotNull VirtualFile appFile) {
    ErlangFile appConfigPsi = ErlangTermFileUtil.createPsi(appFile);
    if (appConfigPsi == null) return;
    List<ErlangTupleExpression> applicationDescriptors =
      ErlangTermFileUtil.findNamedTuples(PsiTreeUtil.getChildrenOfTypeAsList(appConfigPsi, ErlangExpression.class), "application");
    ErlangListExpression appAttributes = PsiTreeUtil.getChildOfType(ContainerUtil.getFirstItem(applicationDescriptors), ErlangListExpression.class);
    processConfigSection(appAttributes, "applications", new Processor<ErlangExpression>() {
      @Override
      public boolean process(ErlangExpression deps) {
        ErlangListExpression dependencyAppsList = deps instanceof ErlangListExpression ? (ErlangListExpression) deps : null;
        if (dependencyAppsList != null) {
          for (ErlangExpression depExpression : dependencyAppsList.getExpressionList()) {
            ErlangQAtom depApp = PsiTreeUtil.getChildOfType(depExpression, ErlangQAtom.class);
            PsiElement appNameAtom = depApp != null ? depApp.getAtom() : null;
            if (appNameAtom != null) {
              myDeps.add(appNameAtom.getText());
            }
          }
        }
        return true;
      }
    });
  }

  private void addDependenciesFromRebarConfig(PsiFile rebarConfig) {
    processConfigSection(rebarConfig, "deps", new Processor<ErlangExpression>() {
      @Override
      public boolean process(ErlangExpression tuplesList) {
        List<ErlangTupleExpression> dependencyTuples = ErlangTermFileUtil.findNamedTuples(tuplesList);
        for (ErlangTupleExpression namedTuple : dependencyTuples) {
          myDeps.add(ErlangTermFileUtil.getNameOfNamedTuple(namedTuple));
        }
        return true;
      }
    });
  }

  private void addIncludePathsFromRebarConfig(PsiFile rebarConfig) {
    processConfigSection(rebarConfig, "erl_opts", new Processor<ErlangExpression>() {
      @Override
      public boolean process(ErlangExpression section) {
        processConfigSection(section, "i", new Processor<ErlangExpression>() {
          @Override
          public boolean process(ErlangExpression includeOptionValue) {
            if (includeOptionValue instanceof ErlangStringLiteral) {
              addIncludePath((ErlangStringLiteral) includeOptionValue);
            }
            else {
              for (ErlangStringLiteral includePath : PsiTreeUtil.findChildrenOfType(includeOptionValue, ErlangStringLiteral.class)) {
                addIncludePath(includePath);
              }
            }
            return true;
          }
        });
        return true;
      }
    });
  }

  private static void processConfigSection(@Nullable PsiElement configRoot, String sectionName, Processor<ErlangExpression> sectionProcessor) {
    List<ErlangTupleExpression> erlOptTuples = ErlangTermFileUtil.findNamedTuples(PsiTreeUtil.getChildrenOfTypeAsList(configRoot, ErlangExpression.class), sectionName);
    for (ErlangTupleExpression erlOptTuple : erlOptTuples) {
      List<ErlangExpression> expressions = erlOptTuple.getExpressionList();
      ErlangExpression optionsList = expressions.size() >= 2 ? expressions.get(1) : null;
      if (optionsList == null) continue;
      sectionProcessor.process(optionsList);
    }
  }

  private void addIncludePath(ErlangStringLiteral includePath) {
    String relativeIncludePath = StringUtil.unquoteString(includePath.getString().getText());
    VirtualFile path = VfsUtil.findRelativeFile(relativeIncludePath, myRoot);
    if (path != null) {
      myIncludePaths.add(path.getPath());
    }
  }
}
