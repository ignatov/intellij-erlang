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
import com.intellij.openapi.options.Configurable;
import com.maddyhome.idea.copyright.ui.FileTypeCopyrightConfigurableFactory;
import com.maddyhome.idea.copyright.ui.TemplateCommentPanel;
import com.maddyhome.idea.copyright.util.FileTypeUtil;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class ErlangCopyrightFormattingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // Settings | Editor | Copyright | Formatting has one entry per supported file type; #951
  public void testErlangFileTypesHaveDistinctEntryNames() {
    TemplateCommentPanel mainPanel = new TemplateCommentPanel(null, null, getProject());
    List<String> names = new ArrayList<>();
    for (FileType fileType : FileTypeUtil.getSupportedTypes()) {
      if (!(fileType instanceof ErlangFileType)) continue;
      Configurable configurable = FileTypeCopyrightConfigurableFactory.createFileTypeConfigurable(getProject(), fileType, mainPanel);
      names.add(configurable.getDisplayName());
      configurable.disposeUIResources();
    }
    mainPanel.disposeUIResources();
    assertEquals("Formatting entries of the Erlang file types: " + names, ErlangFileType.TYPES.size(), names.size());
    assertEquals("Formatting entries of the Erlang file types: " + names, names.size(), new HashSet<>(names).size());
  }
}
