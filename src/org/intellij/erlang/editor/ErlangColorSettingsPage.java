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

package org.intellij.erlang.editor;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import gnu.trove.THashMap;
import org.intellij.erlang.ErlangIcons;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.Map;

import static org.intellij.erlang.editor.ErlangSyntaxHighlighter.*;

/**
 * @author ignatov
 */
public class ErlangColorSettingsPage implements ColorSettingsPage {
  private static final AttributesDescriptor[] ATTRS;

  static {
    ATTRS = new AttributesDescriptor[]{
      new AttributesDescriptor("Illegal character", ILLEGAL),
      new AttributesDescriptor("Comment", COMMENT),
      new AttributesDescriptor("String", STRING),
      new AttributesDescriptor("Number", NUMBER),
      new AttributesDescriptor("Module attributes", MODULE_ATTRIBUTE),
      new AttributesDescriptor("Keyword", KEYWORD),
      new AttributesDescriptor("Parenthesis", PARENTHS),
      new AttributesDescriptor("Braces", BRACES),
      new AttributesDescriptor("Brackets", BRACKETS),
      new AttributesDescriptor("Atom", ATOM),
      new AttributesDescriptor("Macro", MACRO),
      new AttributesDescriptor("Variable", VARIABLES),
      new AttributesDescriptor("Records", RECORDS),
      new AttributesDescriptor("Operation sign", OP_SIGN),
      new AttributesDescriptor("Edoc tag", DOC_COMMENT_TAG),
      new AttributesDescriptor("Function", FUNCTION),
      new AttributesDescriptor("Type definition", TYPE_DEFINITION),
      new AttributesDescriptor("Type", TYPE),
      new AttributesDescriptor("Specification", SPECIFICATION)
    };
  }

  @NotNull
  public String getDisplayName() {
    return "Erlang";
  }

  public Icon getIcon() {
    return ErlangIcons.FILE;
  }

  @NotNull
  public AttributesDescriptor[] getAttributeDescriptors() {
    return ATTRS;
  }

  @NotNull
  public ColorDescriptor[] getColorDescriptors() {
    return ColorDescriptor.EMPTY_ARRAY;
  }

  @NotNull
  public SyntaxHighlighter getHighlighter() {
    return new ErlangSyntaxHighlighter();
  }

  @NotNull
  public String getDemoText() {
    return "%%% Module fact documentation\n" +
      "-<m_att>module</m_att>(fact).\n" +
      "-<m_att>export</m_att>([fac/1]).\n" +
      "\n" +
      "-<m_att>record</m_att>(state, {id, name}).\n" +
      "\n" +
      "-<m_att>define</m_att>(<m>MACRO</m>, macro_value).\n" +
      "\n" +
      "<td>-type</td> <t>in</t>() :: ok | hello .\n" +
      "<td>-type</td> <t>out</t>() :: ok | {error, <t>term</t>()}.\n" +
      "\n" +
      "%% Factorial implementation\n" +
      "%% <d>@doc</d> Documentation\n" +
      "<f>fac</f>(0) -> 1;\n" +
      "<f>fac</f>(N) when N > 0, is_integer(N) -> N * fac(N-1).\n" +
      "\n" +
      "<f>string_sample</f>(A) -> \"string\n" +
      "  second line\".\n" +
      "\n" +
      "<f>update_state</f>(State) -> State#<r>state</r>{id=10}.\n" +
      "\n" +
      "<s>-spec</s> simple(<t>in</t>())-> <t>out</t>(). \n"  +
      "<f>simple</f>(<a>ok</a>) -> <a>ok</a>.\n" +
      "\n" +
      "<f>use_macro</f>() -> ?<m>MACRO</m>."
      ;
  }

  public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
    @NonNls
    final Map<String, TextAttributesKey> map = new THashMap<String, TextAttributesKey>();
    map.put("a", ATOM);
    map.put("d", DOC_COMMENT_TAG);
    map.put("k", KEYWORD);
    map.put("m_att", MODULE_ATTRIBUTE);
    map.put("m", MACRO);
    map.put("r", RECORDS);
    map.put("f", FUNCTION);
    map.put("td", TYPE_DEFINITION);
    map.put("t", TYPE);
    map.put("s", SPECIFICATION);

    return map;
  }
}