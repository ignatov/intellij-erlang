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
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangIcons;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.Map;

import static org.intellij.erlang.editor.ErlangSyntaxHighlighter.*;

public class ErlangColorSettingsPage implements ColorSettingsPage {
  private static final AttributesDescriptor[] ATTRS = new AttributesDescriptor[]{
    new AttributesDescriptor("Illegal character", ILLEGAL),
    new AttributesDescriptor("Comment", COMMENT),
    new AttributesDescriptor("String", STRING),
    new AttributesDescriptor("Number", NUMBER),
    new AttributesDescriptor("Keyword", KEYWORD),
    new AttributesDescriptor("Parenthesis", PARENTHESES),
    new AttributesDescriptor("Braces", BRACES),
    new AttributesDescriptor("Brackets", BRACKETS),
    new AttributesDescriptor("Atom", ATOM),
    new AttributesDescriptor("Macro", MACRO),
    new AttributesDescriptor("Variable", VARIABLES),
    new AttributesDescriptor("Records", RECORDS),
    new AttributesDescriptor("Operation sign", OP_SIGN),
    new AttributesDescriptor("Edoc tag", DOC_TAG),
    new AttributesDescriptor("Function", FUNCTION),
    new AttributesDescriptor("Type", TYPE),
    new AttributesDescriptor("Built-in type", BUILT_IN_TYPE),
    new AttributesDescriptor("Attribute", ATTRIBUTE),
    new AttributesDescriptor("Function call", FUNCTION_CALL),
    new AttributesDescriptor("Module reference", MODULE_REF),
    new AttributesDescriptor("Guards", GUARD),
    new AttributesDescriptor("Callbacks", CALLBACK),
    new AttributesDescriptor("Specifications", SPEC),
  };

  private static Map<String, TextAttributesKey> ATTRIBUTES_KEY_MAP = ContainerUtil.newHashMap();

  static {
    ATTRIBUTES_KEY_MAP.put("a", ATOM);
    ATTRIBUTES_KEY_MAP.put("d", DOC_TAG);
    ATTRIBUTES_KEY_MAP.put("k", KEYWORD);
    ATTRIBUTES_KEY_MAP.put("m", MACRO);
    ATTRIBUTES_KEY_MAP.put("r", RECORDS);
    ATTRIBUTES_KEY_MAP.put("f", FUNCTION);
    ATTRIBUTES_KEY_MAP.put("t", TYPE);
    ATTRIBUTES_KEY_MAP.put("bt", BUILT_IN_TYPE);
    ATTRIBUTES_KEY_MAP.put("m_att", ATTRIBUTE);
    ATTRIBUTES_KEY_MAP.put("c", FUNCTION_CALL);
    ATTRIBUTES_KEY_MAP.put("mr", MODULE_REF);
    ATTRIBUTES_KEY_MAP.put("g", GUARD);
    ATTRIBUTES_KEY_MAP.put("s", SPEC);
    ATTRIBUTES_KEY_MAP.put("cb", CALLBACK);
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
      "<m_att>-module</m_att>(fact).\n" +
      "<m_att>-export</m_att>([<f>fac</f>/1]).\n" +
      "\n" +
      "<m_att>-record</m_att>(<r>state</r>, {id, name}).\n" +
      "\n" +
      "<m_att>-define</m_att>(<m>MACRO</m>, macro_value).\n" +
      "\n" +
      "<m_att>-type</m_att> <t>in</t>() :: ok | hello .\n" +
      "<m_att>-type</m_att> <t>out</t>() :: ok | {error, <bt>term</bt>()}.\n" +
      "\n" +
      "%% Factorial implementation\n" +
      "%% <d>@doc</d> Documentation\n" +
      "<f>fac</f>(0) -> 1;\n" +
      "<f>fac</f>(N) when N > 0, <g>is_integer</g>(N) -> N * <c>fac</c>(N-1).\n" +
      "\n" +
      "<f>string_sample</f>(A) -> \"string\n" +
      "  second line\".\n" +
      "\n" +
      "<f>update_state</f>(State) -> State#<r>state</r>{id=10}.\n" +
      "\n" +
      "<m_att>-spec</m_att> <s>simple</s>(<t>in</t>())-> <t>out</t>(). \n"  +
      "<f>simple</f>(<a>ok</a>) -> <a>ok</a>.\n" +
      "\n" +
      "<f>use_macro</f>() -> <mr>io</mr>:<c>format</c>(?<m>MACRO</m>).\n" +
      "\n" +
      "<m_att>-callback</m_att> <cb>start_service</cb>() -> {<a>ok</a>, <bt>pid</bt>()}."
      ;
  }

  @NotNull
  public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
    return ATTRIBUTES_KEY_MAP;
  }
}