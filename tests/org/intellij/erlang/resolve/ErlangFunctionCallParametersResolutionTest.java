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

package org.intellij.erlang.resolve;

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ObjectUtils;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangQAtom;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangFunctionCallParametersResolutionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/resolve/functionCallParameters/" + getTestName(true) + "/";
  }

  protected void doTest(@NotNull Class<? extends PsiElement> psiClass, String expectedModulePath, String... filePaths) {
    myFixture.configureByFiles(filePaths);
    PsiElement focusedElement = myFixture.getFile().findElementAt(myFixture.getEditor().getCaretModel().getOffset());
    focusedElement = PsiTreeUtil.getParentOfType(focusedElement, ErlangQAtom.class);
    assertNotNull(focusedElement);
    PsiReference reference = focusedElement.getReference();
    assertNotNull(reference);
    PsiElement resolvedElement = resolveToPsiElement(reference, psiClass);
    assertNotNull(resolvedElement);
    PsiFile containingFile = resolvedElement.getContainingFile();
    assertNotNull(containingFile);
    VirtualFile virtualFile = containingFile.getVirtualFile();
    assertNotNull(virtualFile);
    String actualModulePath = virtualFile.getPath();
    assertTrue(actualModulePath.endsWith(expectedModulePath));
  }

  @Nullable
  private static PsiElement resolveToPsiElement(@NotNull PsiReference reference,
                                                @NotNull Class<? extends PsiElement> clazz) {
    PsiMultiReference multiReference = ObjectUtils.tryCast(reference, PsiMultiReference.class);
    if (multiReference != null) {
      for (ResolveResult result : multiReference.multiResolve(true)) {
        PsiElement element = ObjectUtils.tryCast(result.getElement(), clazz);
        if (element != null) return element;
      }
    }
    return reference.resolve();
  }

  public void testModuleParameter() {
    doTest(ErlangModule.class, "incl.erl", "test.erl", "incl.erl");
  }

  public void testFunctionParameter() {
    doTest(ErlangFunction.class, "incl.erl", "test.erl", "incl.erl");
  }

}
