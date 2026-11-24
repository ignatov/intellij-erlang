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

package org.intellij.erlang.quickfixes;

import org.intellij.erlang.inspection.ErlangUnusedTypeInspection;
import org.intellij.erlang.intention.ErlangExportTypeIntention;

public class ErlangExportTypeFixTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    //noinspection unchecked
    myFixture.enableInspections(ErlangUnusedTypeInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/export_type/";
  }

  public void testEmpty()    { doTest("Export type"); }
  public void testWithout()  { doTest("Export type"); }
  public void testCommon()   { doTest("Export type"); }
  public void testDelete()   { doTest("Remove type"); }

  public void testIntentionNotOfferedForExportedType() {
    myFixture.configureByFile("exportedType.erl");
    assertFalse(new ErlangExportTypeIntention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
    assertEmpty(myFixture.filterAvailableIntentions("Export type"));
  }

  public void testIntentionOfferedForTypeExportedWithOtherArity() {
    myFixture.configureByText("a.erl", "-export_type([t/1]).\n-type t<caret>() :: atom().\n-type t(A) :: A.");
    assertTrue(new ErlangExportTypeIntention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
  }

  public void testIntentionNotOfferedForTypeInSecondExportTypeAttribute() {
    myFixture.configureByText("a.erl", "-export_type([u/0]).\n-export_type([t/0]).\n-type t<caret>() :: atom().\n-type u() :: atom().");
    assertFalse(new ErlangExportTypeIntention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
  }

  public void testIntentionNotOfferedForExportedQuotedType() {
    myFixture.configureByText("a.erl", "-export_type(['my-type'/0]).\n-type 'my-<caret>type'() :: atom().");
    assertFalse(new ErlangExportTypeIntention().isAvailable(getProject(), myFixture.getEditor(), myFixture.getFile()));
  }
}
