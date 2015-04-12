/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.sdk;

import junit.framework.TestCase;

public class ErlangSdkReleaseTest extends TestCase {
  public void testNeedCompletion1() { assertTrue(ErlangSdkRelease.V_R15B02.needBifCompletion("empty")); }
  public void testNeedCompletion2() { assertFalse(ErlangSdkRelease.V_R16A.needBifCompletion("empty")); }
  public void testNeedCompletion3() { assertFalse(ErlangSdkRelease.V_R16B.needBifCompletion("empty")); }
  public void testNeedCompletion4() { assertTrue(ErlangSdkRelease.V_R15B02.needBifCompletion("lager")); }
  public void testNeedCompletion5() { assertTrue(ErlangSdkRelease.V_R16A.needBifCompletion("lager")); }
  public void testNeedCompletion6() { assertTrue(ErlangSdkRelease.V_R16B.needBifCompletion("lager")); }

  public void testIsNewerThan() {
    assertTrue(ErlangSdkRelease.V_17_0.isNewerThan(ErlangSdkRelease.V_R16A));
    assertFalse(ErlangSdkRelease.V_R16A.isNewerThan(ErlangSdkRelease.V_17_0));
    assertFalse(ErlangSdkRelease.V_17_0.isNewerThan(ErlangSdkRelease.V_17_0));
  }
}
