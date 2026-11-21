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

package org.intellij.erlang.stubs;

import com.intellij.openapi.vfs.VirtualFileFilter;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.PsiManagerEx;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.stubs.StubElement;
import org.intellij.erlang.psi.ErlangBehaviour;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.impl.ErlangBehaviourImpl;
import org.intellij.erlang.stubs.types.ErlangFileElementType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangFileStubTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // Up to version 10 an unterminated '-if(' swallowed the rest of the file, so stubs cached by
  // older versions lack the functions below it. The version must change to drop those stubs.
  public void testUnterminatedIfAttribute() {
    PsiFile file = myFixture.configureByText("p4.erl", "-module(p4).\n-if(\nf() -> ok.\ng() -> ok.\n");
    int functions = 0;
    for (StubElement<?> stub : ((PsiFileImpl) file).calcStubTree().getPlainList()) {
      if (stub.getPsi() instanceof ErlangFunction) functions++;
    }
    assertEquals(2, functions);
    assertTrue(ErlangFileElementType.INSTANCE.getStubVersion() > 10);
  }

  // The behaviour name is stored in the stub, so reading it must not load the tree of the file.
  public void testBehaviourNameComesFromStub() {
    ErlangFile file = (ErlangFile) myFixture.addFileToProject("gs.erl", "-module(gs).\n-behaviour(gen_server).\n");
    PsiFileImpl fileImpl = (PsiFileImpl) file;
    assertNotNull(fileImpl.getStub());
    assertFalse(fileImpl.isContentsLoaded());

    PsiManagerEx.getInstanceEx(getProject()).setAssertOnFileLoadingFilter(VirtualFileFilter.ALL, getTestRootDisposable());
    List<ErlangBehaviour> behaviours = file.getBehaviours();
    assertEquals(1, behaviours.size());
    assertEquals("gen_server", behaviours.get(0).getName());
    assertFalse(fileImpl.isContentsLoaded());
  }

  // The name from the stub must equal the name from the tree.
  public void testBehaviourNameFromStubEqualsNameFromTree() {
    doTestBehaviourNameFromStubEqualsNameFromTree("q.erl", "-module(q).\n-behaviour('gen_server').\n", "gen_server");
    doTestBehaviourNameFromStubEqualsNameFromTree("us.erl", "-module(us).\n-behavior(supervisor).\n", "supervisor");
    doTestBehaviourNameFromStubEqualsNameFromTree("none.erl", "-module(none).\n-behaviour().\n", "");
  }

  private void doTestBehaviourNameFromStubEqualsNameFromTree(String fileName, String text, String expected) {
    ErlangFile file = (ErlangFile) myFixture.addFileToProject(fileName, text);
    PsiFileImpl fileImpl = (PsiFileImpl) file;
    assertNotNull(fileImpl.getStub());
    List<ErlangBehaviour> fromStub = file.getBehaviours();
    assertEquals(1, fromStub.size());
    String stubName = fromStub.get(0).getName();
    assertFalse(fileImpl.isContentsLoaded());

    assertNotNull(fileImpl.getNode());
    List<ErlangBehaviour> fromTree = file.getBehaviours();
    assertEquals(1, fromTree.size());
    assertNull(((ErlangBehaviourImpl) fromTree.get(0)).getStub());
    assertEquals(expected, fromTree.get(0).getName());
    assertEquals(fromTree.get(0).getName(), stubName);
  }
}
