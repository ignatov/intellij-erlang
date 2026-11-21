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

package org.intellij.erlang.index;

import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.GlobalSearchScope;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

// #1421: modules with the same name must come in an order that does not depend on the order the index returns them in,
// otherwise resolving a call to such a module is not idempotent
public class ErlangModuleIndexTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testSameNameModulesInKernelAndStdlib() {
    myFixture.addFileToProject("lib/kernel/src/kernel.app.src", "{application, kernel, []}.");
    myFixture.addFileToProject("lib/stdlib/src/stdlib.app.src", "{application, stdlib, []}.");
    VirtualFile inKernel = addModule("lib/kernel/src/dup.erl");
    VirtualFile inStdlib = addModule("lib/stdlib/src/dup.erl");
    doTest(inKernel, inStdlib);
  }

  public void testSameNameModulesWithPathsOfEqualLength() {
    VirtualFile first = addModule("apps/aa/src/dup.erl");
    VirtualFile second = addModule("apps/bb/src/dup.erl");
    doTest(first, second);
  }

  public void testResolveToSameNameModulesCreatedInOrder() {
    addModule("apps/aa/src/dup.erl");
    addModule("apps/bb/src/dup.erl");
    doResolveTest("/src/apps/aa/src/dup.erl");
  }

  public void testResolveToSameNameModulesCreatedInReverseOrder() {
    addModule("apps/bb/src/dup.erl");
    addModule("apps/aa/src/dup.erl");
    doResolveTest("/src/apps/aa/src/dup.erl");
  }

  public void testResolveToSameVersionKernelCopiesCreatedInOrder() {
    addKernelCopy("a");
    addKernelCopy("b");
    doResolveTest("/src/a/kernel-1.0/src/dup.erl");
  }

  public void testResolveToSameVersionKernelCopiesCreatedInReverseOrder() {
    addKernelCopy("b");
    addKernelCopy("a");
    doResolveTest("/src/a/kernel-1.0/src/dup.erl");
  }

  public void testResolveToUnversionedKernelCopiesCreatedInOrder() {
    addKernelCopy("a", "kernel");
    addKernelCopy("b", "kernel");
    doResolveTest("/src/a/kernel/src/dup.erl");
  }

  public void testResolveToUnversionedKernelCopiesCreatedInReverseOrder() {
    addKernelCopy("b", "kernel");
    addKernelCopy("a", "kernel");
    doResolveTest("/src/a/kernel/src/dup.erl");
  }

  private void addKernelCopy(String root) {
    addKernelCopy(root, "kernel-1.0");
  }

  private void addKernelCopy(String root, String appDir) {
    myFixture.addFileToProject(root + "/" + appDir + "/src/kernel.app.src", "{application, kernel, []}.");
    addModule(root + "/" + appDir + "/src/dup.erl");
  }

  private void doResolveTest(String expectedPath) {
    myFixture.configureByText("caller.erl", "-module(caller).\ng() -> dup:<caret>f().\n");
    PsiReference reference = myFixture.getFile().findReferenceAt(myFixture.getCaretOffset());
    assertNotNull(reference);
    PsiElement target = reference.resolve();
    assertNotNull(target);
    assertEquals(expectedPath, target.getContainingFile().getVirtualFile().getPath());
  }

  private VirtualFile addModule(String path) {
    return myFixture.addFileToProject(path, "-module(dup).\n-export([f/0]).\nf() -> ok.\n").getVirtualFile();
  }

  private void doTest(VirtualFile a, VirtualFile b) {
    GlobalSearchScope scope = GlobalSearchScope.allScope(getProject());
    assertEquals(2, ErlangModuleIndex.getVirtualFilesByName(getProject(), "dup", scope).size());

    Comparator<VirtualFile> comparator = createComparator(scope);
    List<VirtualFile> ab = new ArrayList<>(Arrays.asList(a, b));
    List<VirtualFile> ba = new ArrayList<>(Arrays.asList(b, a));
    ab.sort(comparator);
    ba.sort(comparator);
    assertEquals("compare(a, b) = " + comparator.compare(a, b) + ", compare(b, a) = " + comparator.compare(b, a),
                 ab, ba);
  }

  @SuppressWarnings("unchecked")
  private Comparator<VirtualFile> createComparator(GlobalSearchScope scope) {
    try {
      Class<?> comparatorClass = Class.forName(ErlangModuleIndex.class.getName() + "$MyProjectFilesComparator");
      Constructor<?> constructor = comparatorClass.getDeclaredConstructor(ProjectFileIndex.class, GlobalSearchScope.class);
      constructor.setAccessible(true);
      return (Comparator<VirtualFile>) constructor.newInstance(ProjectFileIndex.getInstance(getProject()), scope);
    }
    catch (ReflectiveOperationException e) {
      throw new AssertionError(e);
    }
  }
}
