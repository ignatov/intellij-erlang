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

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.openapi.application.WriteAction;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangUnresolvedRecordFieldInspection;

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

  public void testSimple()           { doIntroduceRecordFieldTest(); }
  public void testWithOtherFields()  { doIntroduceRecordFieldTest(); }

  public void testRecordInHeaderPreview() {
    myFixture.configureByFiles("recordInHeader.erl", "recordInHeader.hrl");
    VirtualFile header = myFixture.findFileInTempDir("recordInHeader.hrl");
    IntentionAction action = ContainerUtil.getFirstItem(myFixture.filterAvailableIntentions("Introduce record field"));
    assertNotNull(action);
    myFixture.getIntentionPreviewText(action);
    assertEquals("-record(rec, {old_field}).\n", getDocumentText(header));
  }

  public void testRecordInReadOnlyHeader() throws Exception {
    myFixture.configureByFiles("recordInReadOnlyHeader.erl", "recordInReadOnlyHeader.hrl");
    VirtualFile header = myFixture.findFileInTempDir("recordInReadOnlyHeader.hrl");
    WriteAction.runAndWait(() -> header.setWritable(false));
    assertFalse(header.isWritable());
    launchIntention("Introduce record field");
    assertEquals("-record(rec, {old_field}).\n", getDocumentText(header));
  }

  private static String getDocumentText(VirtualFile file) {
    return FileDocumentManager.getInstance().getDocument(file).getText();
  }

  private void doIntroduceRecordFieldTest() { doTest("Introduce record field"); }
}
