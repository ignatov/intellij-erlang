/*
* Copyright 2012 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
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

import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;

public class ErlangCodeStyleSettings extends CustomCodeStyleSettings {
  public boolean ALIGN_MULTILINE_BLOCK = false;
  public boolean ALIGN_FUNCTION_CLAUSES = false;
  public boolean ALIGN_GUARDS = false;
  public boolean INDENT_RELATIVE = true;
  public boolean NEW_LINE_BEFORE_COMMA = false;
  public boolean SPACE_AROUND_SEND = true;
  public boolean SPACE_AROUND_ARROW = true;
  public boolean SPACE_AROUND_LEFT_ARROW = true;
  public boolean SPACE_AROUND_EQ_IN_RECORDS = true;
  public boolean SPACE_AROUND_OR_IN_LISTS = true;

  public int EXPRESSION_IN_CLAUSE_WRAP = CommonCodeStyleSettings.WRAP_AS_NEEDED;  

  protected ErlangCodeStyleSettings(CodeStyleSettings container) {
    super("ErlangCodeStyleSettings", container);
  }
}
