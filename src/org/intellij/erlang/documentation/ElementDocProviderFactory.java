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

package org.intellij.erlang.documentation;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public final class ElementDocProviderFactory {
  private ElementDocProviderFactory() {
  }

  @Nullable
  static ElementDocProvider create(@NotNull PsiElement psiElement) {
    Project project = psiElement.getProject();

    if (psiElement instanceof ErlangModule erlangModule) {
      return createModuleDocProvider(project, erlangModule);
    }
    if (psiElement instanceof ErlangFunction erlangFunction) {
      return createFunctionDocProvider(project, erlangFunction);
    }
    if (psiElement instanceof ErlangTypeDefinition typeDefinition) {
      return createTypedefDocProvider(project, typeDefinition);
    }
    var applyDoc = createApplyDocProvider(psiElement, project);
    if (applyDoc != null) {
      return applyDoc;
    }
    // Allow the ErlangDocumentationProvider to query PSI node type from ErlTypeFactory
    return new ErlangEmptyDocProvider();
  }

  @Nullable
  private static ErlangSdkFunctionDocProvider createApplyDocProvider(@NotNull PsiElement psiElement,
                                                                     Project project) {
    var erlGlobalFunctionCall = PsiTreeUtil.getParentOfType(psiElement, ErlangGlobalFunctionCallExpression.class);

    if (erlGlobalFunctionCall != null) {
      var moduleRef = erlGlobalFunctionCall.getModuleRef();
      var moduleName = moduleRef.getText();
      var erlFunctionCall = erlGlobalFunctionCall.getFunctionCallExpression();
      var functionName = erlFunctionCall.getName();
      int arity = erlFunctionCall.getArgumentList().getExpressionList().size();

      if (ErlangBifTable.isBif(moduleName, functionName, arity)) {
        var tentativeErlangModule = moduleRef.getReference().resolve();

        if (tentativeErlangModule instanceof ErlangModule) {
          var virtualFile = getVirtualFile(tentativeErlangModule);

          if (virtualFile != null) {
            return new ErlangSdkFunctionDocProvider(project, functionName, arity, virtualFile);
          }
        }
      }
    }
    return null;
  }

  @Nullable
  private static ErlangSdkTypeDocProvider createTypedefDocProvider(Project project,
                                                                   @NotNull ErlangTypeDefinition typeDefinition) {
    var virtualFile = getVirtualFile(typeDefinition);
    if (virtualFile == null) return null;

    if (isFileFromErlangSdk(project, virtualFile)) {
      return new ErlangSdkTypeDocProvider(project, virtualFile, typeDefinition.getName());
    }
    return null;
  }

  @Nullable
  private static ElementDocProvider createFunctionDocProvider(Project project,
                                                              @NotNull ErlangFunction erlangFunction) {
    var virtualFile = getVirtualFile(erlangFunction);
    if (virtualFile == null) return null;

    if (isFileFromErlangSdk(project, virtualFile)) {
      return new ErlangSdkFunctionDocProvider(project, erlangFunction.getName(), erlangFunction.getArity(), virtualFile);
    }
    return new ErlangFunctionDocProvider(erlangFunction);
  }

  @Nullable
  private static ElementDocProvider createModuleDocProvider(Project project,
                                                            @NotNull ErlangModule erlangModule) {
    var virtualFile = getVirtualFile(erlangModule);
    if (virtualFile == null) return null;

    if (isFileFromErlangSdk(project, virtualFile)) {
      return new ErlangSdkModuleDocProvider(project, virtualFile);
    }
    return new ErlangModuleDocProvider(erlangModule);
  }

  private static boolean isFileFromErlangSdk(@NotNull Project project, @NotNull VirtualFile virtualFile) {
    var projectRootManager = ProjectRootManager.getInstance(project);
    var projectSdk = projectRootManager.getProjectSdk();

    if (projectSdk == null) return false;

    for (VirtualFile sdkSourceRoot : projectSdk.getRootProvider().getFiles(OrderRootType.SOURCES)) {
      if (virtualFile.getPath().startsWith(sdkSourceRoot.getPath())) return true;
    }
    return false;
  }

  @Nullable
  private static VirtualFile getVirtualFile(@NotNull PsiElement psiElement) {
    var containingFile = psiElement.getContainingFile();
    return containingFile == null ? null : containingFile.getVirtualFile();
  }

}
