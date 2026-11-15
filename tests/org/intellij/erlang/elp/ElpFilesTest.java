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

package org.intellij.erlang.elp;

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ElpFilesTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testModulesAndHeaders() {
    assertTrue(ElpFiles.isElpFile(file("src/a.erl")));
    assertTrue(ElpFiles.isElpFile(file("include/a.hrl")));
  }

  public void testRebarConfigIsNotForElp() {
    VirtualFile rebarConfig = file("rebar.config");
    assertInstanceOf(rebarConfig.getFileType(), ErlangFileType.class);
    assertFalse(ElpFiles.isElpFile(rebarConfig));
  }

  public void testAppSrcIsNotForElp() {
    VirtualFile appSrc = file("src/x.app.src");
    assertInstanceOf(appSrc.getFileType(), ErlangFileType.class);
    assertFalse(ElpFiles.isElpFile(appSrc));
  }

  public void testOtherErlangFilesAreNotForElp() {
    assertFalse(ElpFiles.isElpFile(file("x.escript")));
    assertFalse(ElpFiles.isElpFile(file("config/sys.config")));
    assertFalse(ElpFiles.isElpFile(file("ebin/x.app")));
    assertFalse(ElpFiles.isElpFile(file("x.erl.orig")));
    assertFalse(ElpFiles.isElpFileName("erl"));
  }

  public void testDirectoryIsNotForElp() {
    VirtualFile dir = file("src/a.erl").getParent();
    assertFalse(ElpFiles.isElpFile(dir));
  }

  public void testWorkspaceRootIsTheRootThatHoldsTheFile() {
    VirtualFile module = file("src/a.erl");
    VirtualFile root = ReadAction.compute(() -> ElpFiles.findWorkspaceRoot(getProject(), module));
    assertNotNull(root);
    assertContainsElements(java.util.Arrays.asList(ProjectRootManager.getInstance(getProject()).getContentRoots()), root);
    assertTrue(VfsUtilCore.isAncestor(root, module, true));
  }

  private VirtualFile file(String path) {
    return myFixture.addFileToProject(path, "").getVirtualFile();
  }
}
