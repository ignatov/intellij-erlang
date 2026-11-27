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

import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.IndexingTestUtil;
import com.intellij.testFramework.JavaModuleTestCase;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.VfsTestUtil;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangGlobalFunctionCallExpression;
import org.jetbrains.annotations.Nullable;

public class ErlangMultiModuleRemoteCallResolveTest extends JavaModuleTestCase {
  @Override
  protected ModuleType<?> getModuleType() {
    return ErlangModuleType.getInstance();
  }

  // #1688: the function of util:f() goes to the copy of util that the caller's module sees, the one its module name
  // resolves to, not to the first copy of the whole project, which an unrelated IDE module holds
  public void testFunctionInCopyOfCallerModule() {
    VirtualFile root = getTempDir().createVirtualDir();
    VirtualFile aa = VfsTestUtil.createDir(root, "aa");
    VirtualFile zz = VfsTestUtil.createDir(root, "zz");
    VfsTestUtil.createFile(aa, "util.erl", "-module(util).\n-export([g/0]).\ng() -> ok.\n");
    VirtualFile zzUtil = VfsTestUtil.createFile(zz, "util.erl", "-module(util).\n-export([f/0]).\nf() -> ok.\n");
    VirtualFile caller = VfsTestUtil.createFile(zz, "caller.erl", "-module(caller).\n-export([h/0]).\nh() -> util:f().\n");

    Module other = WriteAction.compute(() -> ModuleManager.getInstance(myProject)
      .newModule(root.getPath() + "/other.iml", ErlangModuleType.getInstance().getId()));
    PsiTestUtil.addContentRoot(other, aa);
    PsiTestUtil.addSourceRoot(other, aa);
    PsiTestUtil.addContentRoot(myModule, zz);
    PsiTestUtil.addSourceRoot(myModule, zz);
    IndexingTestUtil.waitUntilIndexesAreReady(myProject);

    PsiFile callerFile = PsiManager.getInstance(myProject).findFile(caller);
    assertNotNull(callerFile);
    ErlangGlobalFunctionCallExpression call = PsiTreeUtil.findChildOfType(callerFile, ErlangGlobalFunctionCallExpression.class);
    assertNotNull(call);
    assertEquals(zzUtil, resolvedFile(call.getModuleRef().getReference()));
    ErlangFunctionCallExpression function = call.getFunctionCallExpression();
    assertEquals(zzUtil, resolvedFile(function.getReference()));
  }

  @Nullable
  private static VirtualFile resolvedFile(@Nullable PsiReference reference) {
    PsiElement target = reference != null ? reference.resolve() : null;
    PsiFile file = target != null ? target.getContainingFile() : null;
    return file != null ? file.getVirtualFile() : null;
  }
}
