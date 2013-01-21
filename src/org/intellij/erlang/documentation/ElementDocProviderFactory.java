/*
 * Copyright 2013 Sergey Ignatov
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
        return new SdkModuleDocProvider(erlangModule.getProject(), virtualFile);
      }
      else {
        return new ModuleDocProvider(erlangModule);
      }
    }
    else if (psiElement instanceof ErlangFunction) {
      final VirtualFile virtualFile = getVirtualFile(psiElement);
      if (virtualFile == null) {
        return null;
      }
      final ErlangFunction erlangFunction = (ErlangFunction) psiElement;
      if (isFileFromErlangSdk(project, virtualFile)) {
        return new SdkFunctionDocProvider(erlangFunction.getProject(), erlangFunction.getName(),
          erlangFunction.getArity(), virtualFile);
      }
      else {
        return new FunctionDocProvider(erlangFunction);
      }
    }
    else if (psiElement instanceof ErlangTypeDefinition) {
      final VirtualFile virtualFile = getVirtualFile(psiElement);
      if (virtualFile == null) {
        return null;
      }
      final ErlangTypeDefinition typeDefinition = (ErlangTypeDefinition) psiElement;
      if (isFileFromErlangSdk(project, virtualFile)) {
        return new SdkTypeDocProvider(project, virtualFile, typeDefinition.getName());
      }
      else {
        return null; // TODO implement TypeDocProvider
      }
    }
    else {
      final PsiElement parent = psiElement.getParent();
      if (parent instanceof ErlangFunctionCallExpression) {
        final ErlangFunctionCallExpression unresolvedFunctionCall = (ErlangFunctionCallExpression) parent;
        final String functionName = unresolvedFunctionCall.getNameIdentifier().getText();
        final int functionArity = unresolvedFunctionCall.getArgumentList().getExpressionList().size();
        final PsiElement callExpressionParent = unresolvedFunctionCall.getParent();
        if (callExpressionParent instanceof ErlangGlobalFunctionCallExpression) {
          final ErlangModuleRef moduleRef = ((ErlangGlobalFunctionCallExpression) callExpressionParent).getModuleRef();
          final PsiReference psiReference = (moduleRef != null ? moduleRef.getReference() : null);
          final PsiElement tentativeErlangModule = psiReference != null ? psiReference.resolve() : null;
          if (tentativeErlangModule instanceof ErlangModule) {
            final ErlangModule erlangModule = (ErlangModule) tentativeErlangModule;
            if (ErlangBifTable.isBif(erlangModule.getName(), functionName, functionArity)) {
              final VirtualFile virtualFile = getVirtualFile(erlangModule);
              if (virtualFile != null) {
                return new SdkFunctionDocProvider(erlangModule.getProject(), functionName, functionArity, virtualFile);
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
