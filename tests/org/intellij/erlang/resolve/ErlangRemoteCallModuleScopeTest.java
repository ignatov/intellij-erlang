/*
 * Copyright 2012-2026 Sergey Ignatov
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

import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ModuleRootModificationUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.JavaModuleTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.util.ArrayUtil;
import org.intellij.erlang.module.ErlangModuleType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

// #1690: the module name and the function name of a remote call dup:f() search the same scope,
// so they open the same copy of dup even when an IDE module the caller does not depend on has one
public class ErlangRemoteCallModuleScopeTest extends JavaModuleTestCase {
  private static final String TEST_DATA = "testData/resolve/remoteCallScope/";

  public void testOnlyInModuleNotDependedOn() throws Exception {
    Module caller = createErlangModule("caller");
    createErlangModule("other");
    doTest(caller);
  }

  public void testCopiesInDependencyAndInModuleNotDependedOn() throws Exception {
    Module caller = createErlangModule("caller");
    createErlangModule("other");
    Module dependency = createErlangModule("dependency");
    ModuleRootModificationUtil.addDependency(caller, dependency);
    doTest(caller);
  }

  private void doTest(@NotNull Module caller) {
    VirtualFile root = ArrayUtil.getFirstElement(ModuleRootManager.getInstance(caller).getContentRoots());
    assertNotNull(root);
    VirtualFile callerFile = root.findFileByRelativePath("src/caller.erl");
    assertNotNull(callerFile);
    PsiFile file = PsiManager.getInstance(myProject).findFile(callerFile);
    assertNotNull(file);
    String text = file.getText();
    String modulePath = resolvedPath(file.findReferenceAt(text.indexOf("dup:") + 1));
    String functionPath = resolvedPath(file.findReferenceAt(text.indexOf(":f(") + 1));
    assertEquals("module name resolves to " + modulePath + ", function name to " + functionPath,
                 functionPath, modulePath);
  }

  @Nullable
  private static String resolvedPath(@Nullable PsiReference reference) {
    assertNotNull(reference);
    PsiElement target = reference.resolve();
    PsiFile containingFile = target != null ? target.getContainingFile() : null;
    VirtualFile virtualFile = containingFile != null ? containingFile.getVirtualFile() : null;
    return virtualFile != null ? virtualFile.getPath() : null;
  }

  @NotNull
  private Module createErlangModule(@NotNull String name) throws Exception {
    Module module = createModuleFromTestData(TEST_DATA + getTestName(true) + "/" + name, name,
                                             ErlangModuleType.getInstance(), false);
    VirtualFile contentRoot = ArrayUtil.getFirstElement(ModuleRootManager.getInstance(module).getContentRoots());
    assertNotNull(contentRoot);
    VirtualFile sourceRoot = contentRoot.findChild("src");
    assertNotNull(sourceRoot);
    PsiTestUtil.addSourceRoot(module, sourceRoot, false);
    return module;
  }
}
