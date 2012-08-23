/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.formatter.settings;

import com.intellij.application.options.TabbedLanguageCodeStylePanel;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import org.intellij.erlang.ErlangLanguage;

/**
 * @author ignatov
 */
public class ErlangCodeStyleMainPanel extends TabbedLanguageCodeStylePanel {
  protected ErlangCodeStyleMainPanel(CodeStyleSettings currentSettings, CodeStyleSettings settings) {
    super(ErlangLanguage.INSTANCE, currentSettings, settings);
  }
}
