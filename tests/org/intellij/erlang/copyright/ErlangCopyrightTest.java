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

package org.intellij.erlang.copyright;

import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.options.Configurable;
import com.intellij.psi.PsiFile;
import com.maddyhome.idea.copyright.CopyrightProfile;
import com.maddyhome.idea.copyright.psi.UpdateCopyright;
import com.maddyhome.idea.copyright.psi.UpdateCopyrightFactory;
import com.maddyhome.idea.copyright.ui.FileTypeCopyrightConfigurableFactory;
import com.maddyhome.idea.copyright.ui.TemplateCommentPanel;
import com.maddyhome.idea.copyright.util.FileTypeUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class ErlangCopyrightTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String NOTICE = "Copyright test notice";

  // Settings | Editor | Copyright | Formatting builds one entry per supported file type; #951
  public void testFormattingEntriesHaveDistinctNames() {
    TemplateCommentPanel mainPanel = new TemplateCommentPanel(null, null, getProject());
    List<String> names = new ArrayList<>();
    for (FileType fileType : FileTypeUtil.getSupportedTypes()) {
      if (!(fileType instanceof LanguageFileType languageFileType) ||
          !languageFileType.getLanguage().isKindOf(ErlangLanguage.INSTANCE)) {
        continue;
      }
      Configurable configurable = FileTypeCopyrightConfigurableFactory.createFileTypeConfigurable(getProject(), fileType, mainPanel);
      names.add(configurable.getDisplayName());
      configurable.disposeUIResources();
    }
    mainPanel.disposeUIResources();
    assertContainsElements(names, "Erlang");
    assertEquals("Formatting entries of the Erlang file types: " + names, names.size(), new HashSet<>(names).size());
  }

  public void testModule()  { doUpdateTest("foo.erl", ErlangFileType.MODULE, "-module(foo).\n"); }
  public void testHeader()  { doUpdateTest("foo.hrl", ErlangFileType.HEADER, "-define(FOO, 1).\n"); }
  public void testAppSrc()  { doUpdateTest("foo.app.src", ErlangFileType.APP, "{application, foo, []}.\n"); }
  public void testApp()     { doUpdateTest("foo.app", ErlangFileType.APP, "{application, foo, []}.\n"); }
  public void testConfig()  { doUpdateTest("sys.config", ErlangFileType.TERMS, "[{foo, []}].\n"); }

  private void doUpdateTest(String fileName, FileType expectedType, String text) {
    PsiFile file = myFixture.configureByText(fileName, text);
    assertSame(expectedType, file.getVirtualFile().getFileType());
    assertTrue(FileTypeUtil.isSupportedFile(file));

    CopyrightProfile profile = new CopyrightProfile("test");
    profile.setNotice(NOTICE);
    UpdateCopyright update = UpdateCopyrightFactory.createUpdateCopyright(getProject(), getModule(), file, profile);
    assertNotNull("No copyright updater for " + fileName, update);
    update.prepare();
    try {
      update.complete();
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }

    String result = myFixture.getEditor().getDocument().getText();
    assertTrue(result, result.startsWith("%%"));
    assertTrue(result, result.contains(NOTICE));
    assertTrue(result, result.endsWith(text));
  }
}
