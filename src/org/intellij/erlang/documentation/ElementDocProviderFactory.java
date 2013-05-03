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

package org.intellij.erlang.documentation;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

final class ElementDocProviderFactory {

  private ElementDocProviderFactory() {
  }

  @Nullable
  static ElementDocProvider create(@NotNull PsiElement psiElement) {
    final Project project = psiElement.getProject();
    if (psiElement instanceof ErlangModule) {
      final VirtualFile virtualFile = getVirtualFile(psiElement);
      if (virtualFile == null) {
        return null;
      }
      final ErlangModule erlangModule = (ErlangModule) psiElement;
      if (isFileFromErlangSdk(project, virtualFile)) {
        return new ErlangSdkModuleDocProvider(project, virtualFile);
      }
      else {
        return new ErlangModuleDocProvider(erlangModule);
      }
    }
    else if (psiElement instanceof ErlangFunction) {
      final VirtualFile virtualFile = getVirtualFile(psiElement);
      if (virtualFile == null) {
        return null;
      }
      final ErlangFunction erlangFunction = (ErlangFunction) psiElement;
      if (isFileFromErlangSdk(project, virtualFile)) {
        return new ErlangSdkFunctionDocProvider(project, erlangFunction.getName(),
          erlangFunction.getArity(), virtualFile);
      }
      else {
        return new ErlangFunctionDocProvider(erlangFunction);
      }
    }
    else if (psiElement instanceof ErlangTypeDefinition) {
      final VirtualFile virtualFile = getVirtualFile(psiElement);
      if (virtualFile == null) {
        return null;
      }
      final ErlangTypeDefinition typeDefinition = (ErlangTypeDefinition) psiElement;
      if (isFileFromErlangSdk(project, virtualFile)) {
        return new ErlangSdkTypeDocProvider(project, virtualFile, typeDefinition.getName());
      }
      else {
        return null; // TODO implement TypeDocProvider
      }
    }
    else {
      final ErlangGlobalFunctionCallExpression erlGlobalFunctionCall = PsiTreeUtil.getParentOfType(
        psiElement, ErlangGlobalFunctionCallExpression.class);
      if (erlGlobalFunctionCall != null) {
        final ErlangModuleRef moduleRef = erlGlobalFunctionCall.getModuleRef();
        if (moduleRef != null) {
          final String moduleName = moduleRef.getText();
          final ErlangFunctionCallExpression erlFunctionCall = erlGlobalFunctionCall.getFunctionCallExpression();
          final String functionName = erlFunctionCall.getNameIdentifier().getText();
          final int arity = erlFunctionCall.getArgumentList().getExpressionList().size();
          if (ErlangBifTable.isBif(moduleName, functionName, arity)) {
            final PsiReference psiReference = moduleRef.getReference();
            final PsiElement tentativeErlangModule = psiReference != null ? psiReference.resolve() : null;
            if (tentativeErlangModule instanceof ErlangModule) {
              final VirtualFile virtualFile = getVirtualFile(tentativeErlangModule);
              if (virtualFile != null) {
                return new ErlangSdkFunctionDocProvider(project, functionName, arity, virtualFile);
              }
            }
          }
        }
      }
    }
    return null;
  }

  private static boolean isFileFromErlangSdk(@NotNull Project project, @NotNull VirtualFile virtualFile) {
    final ProjectRootManager projectRootManager = ProjectRootManager.getInstance(project);
    final Sdk projectSdk = projectRootManager.getProjectSdk();
    if (projectSdk == null) {
      return false;
    }
    for (VirtualFile sdkSourceRoot : projectSdk.getRootProvider().getFiles(OrderRootType.SOURCES)) {
      if (virtualFile.getPath().startsWith(sdkSourceRoot.getPath())) {
        return true;
      }
    }
    return false;
  }

  @Nullable
  private static VirtualFile getVirtualFile(@NotNull PsiElement psiElement) {
    final PsiFile containingFile = psiElement.getContainingFile();
    return (containingFile == null ? null : containingFile.getVirtualFile());
  }
}
