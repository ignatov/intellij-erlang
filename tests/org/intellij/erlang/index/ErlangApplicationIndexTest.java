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

import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.search.GlobalSearchScope;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangApplicationIndexTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testHighestVersionWins() {
    doTest("foo-1.10", "foo-1.9", "foo-1.10");
  }

  public void testHighestVersionWinsWhenLastComponentGainsDigit() {
    doTest("foo-10.2.7.10", "foo-10.2.7.9", "foo-10.2.7.10");
  }

  public void testHighestVersionWinsWithSameNumberOfDigits() {
    doTest("foo-1.2.4", "foo-1.2.3", "foo-1.2.4");
  }

  public void testLongerVersionWins() {
    doTest("foo-8.5.4.1", "foo-8.5.4", "foo-8.5.4.1");
  }

  public void testNoVersionWins() {
    doTest("foo", "foo-1.9", "foo-1.10", "foo");
  }

  private void doTest(String expectedDirName, String... dirNames) {
    for (String dirName : dirNames) {
      myFixture.addFileToProject("lib/" + dirName + "/ebin/foo.app", "{application, foo, []}.");
    }
    VirtualFile appDir = ErlangApplicationIndex.getApplicationDirectoryByName("foo", GlobalSearchScope.allScope(getProject()));
    assertNotNull(appDir);
    assertEquals(expectedDirName, appDir.getName());
  }
}
