/*
 * Copyright 2012-2013 Sergey Ignatov
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
  public void testNeedCompletion1() throws Exception { assertTrue(ErlangSdkRelease.R15B03.needBifCompletion("empty")); }
  public void testNeedCompletion2() throws Exception { assertFalse(ErlangSdkRelease.R16A.needBifCompletion("empty")); }
  public void testNeedCompletion3() throws Exception { assertFalse(ErlangSdkRelease.R16B.needBifCompletion("empty")); }
  public void testNeedCompletion4() throws Exception { assertTrue(ErlangSdkRelease.R15B03.needBifCompletion("lager")); }
  public void testNeedCompletion5() throws Exception { assertTrue(ErlangSdkRelease.R16A.needBifCompletion("lager")); }
  public void testNeedCompletion6() throws Exception { assertTrue(ErlangSdkRelease.R16B.needBifCompletion("lager")); }
}
