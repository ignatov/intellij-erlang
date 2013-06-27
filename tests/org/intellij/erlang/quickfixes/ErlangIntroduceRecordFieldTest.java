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

package org.intellij.erlang.quickfixes;

import org.intellij.erlang.inspection.ErlangUnresolvedRecordFieldInspection;

/**
 * @author savenko
 */
public class ErlangIntroduceRecordFieldTest extends ErlangQuickFixTestBase {

  @SuppressWarnings("unchecked")
  @Override
  protected void setUp() throws Exception {
    super.setUp();

    myFixture.enableInspections(ErlangUnresolvedRecordFieldInspection.class);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/introduce_record_field";
  }

  public void testSimple() throws Throwable           { doIntroduceRecordFieldTest(); }
  public void testWithOtherFields() throws Throwable  { doIntroduceRecordFieldTest(); }

  private void doIntroduceRecordFieldTest() { doTest("Introduce record field"); }
}
