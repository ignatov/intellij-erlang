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

import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;

public class ElpServedRootsTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ElpServedRoots myRoots;
  private ElpSettings.ElpSettingsState myOldState;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myRoots = ElpServedRoots.getInstance();
    myRoots.setRootsForTests(List.of(), getTestRootDisposable());
    ElpSettings.ElpSettingsState old = ElpSettings.getInstance().getState();
    myOldState = new ElpSettings.ElpSettingsState();
    myOldState.path = old.path;
    myOldState.source = old.source;
    myOldState.mode = old.mode;
    myOldState.eqwalizer = old.eqwalizer;
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ElpSettings.getInstance().loadState(myOldState);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testPathPrefixOfAWholeDirectoryName() {
    assertTrue(myRoots.add("/work/shop/"));
    assertFalse("already served", myRoots.add("/work/shop"));
    assertEquals(List.of("/work/shop"), myRoots.getRoots());
    assertTrue(myRoots.isUnderServedRoot("/work/shop"));
    assertTrue(myRoots.isUnderServedRoot("/work/shop/src/shop.erl"));
    assertFalse("a sibling with the same prefix", myRoots.isUnderServedRoot("/work/shop2/src/a.erl"));
    assertFalse(myRoots.isUnderServedRoot("/work"));
    assertFalse("OTP sources outside the root", myRoots.isUnderServedRoot("/usr/lib/erlang/lib/stdlib/src/lists.erl"));
    assertTrue("a Windows path", myRoots.add("C:\\work\\app"));
    assertTrue(myRoots.isUnderServedRoot("C:/work/app/src/a.erl"));
  }

  public void testAddRemoveClear() {
    assertTrue(myRoots.isEmpty());
    myRoots.add("/a");
    myRoots.add("/b");
    assertTrue(myRoots.remove("/a/"));
    assertFalse(myRoots.remove("/a"));
    assertFalse(myRoots.isUnderServedRoot("/a/x.erl"));
    assertTrue(myRoots.isUnderServedRoot("/b/x.erl"));
    assertTrue(myRoots.clear());
    assertFalse(myRoots.clear());
    assertTrue(myRoots.isEmpty());
  }

  public void testStoredAndLoaded() {
    myRoots.add("/work/shop");
    ElpServedRoots.ServedRootsState state = myRoots.getState();
    assertEquals(List.of("/work/shop"), state.roots);
    myRoots.clear();
    state.roots.add("/work/shop/");
    state.roots.add("");
    myRoots.loadState(state);
    assertEquals("normalized, without duplicates or blanks", List.of("/work/shop"), myRoots.getRoots());
  }

  /** The query needs no read action: an indexer asks it on any thread. */
  public void testQueryOnAPooledThreadWithoutReadAction() throws Exception {
    myRoots.add("/work/shop");
    assertTrue(CompletableFuture.supplyAsync(() -> myRoots.isUnderServedRoot("/work/shop/src/a.erl")).get(10, TimeUnit.SECONDS));
  }

  /** Switching back to the Plugin position, in the settings or by "Switch back to the plugin", gives every root back. */
  public void testSwitchingToThePluginPositionClearsTheRegistry() {
    ElpSettings.getInstance().update(ElpServerSource.CUSTOM, "/opt/elp/bin/elp", ElpMode.ELP, false);
    myRoots.add("/work/shop");
    ElpSettings.getInstance().update(ElpServerSource.CUSTOM, "/opt/elp/bin/other-elp", ElpMode.ELP, false);
    assertFalse("another path in the ELP position keeps the roots", myRoots.isEmpty());
    ElpSettings.getInstance().setMode(ElpMode.PLUGIN);
    assertTrue(myRoots.isEmpty());
  }
}
