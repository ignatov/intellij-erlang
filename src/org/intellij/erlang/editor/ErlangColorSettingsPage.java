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
      new AttributesDescriptor("Keyword", KEYWORD),
      new AttributesDescriptor("Parenthesis", PARENTHS),
      new AttributesDescriptor("Braces", BRACES),
      new AttributesDescriptor("Brackets", BRACKETS),
      new AttributesDescriptor("Variable", VARIABLES),
      new AttributesDescriptor("Records", RECORDS),
      new AttributesDescriptor("Operation sign", OP_SIGN),
      new AttributesDescriptor("Edoc tag", DOC_COMMENT_TAG),
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
      "-<k>module</k>(fact).\n" +
      "-<k>export</k>([fac/1]).\n" +
      "\n" +
      "-<k>record</k>(state, {id, name}).\n" +
      "\n" +
      "%% Factorial implementation\n" +
      "%% <d>@doc</d> Documentation\n" +
      "fac(0) -> 1;\n" +
      "fac(N) when N > 0, is_integer(N) -> N * fac(N-1).\n" +
      "\n" +
      "string_sample(A) -> \"string\n" +
      "  second line\".\n" +
      "\n" +
      "update_state(State) -> State#<r>state</r>{id=10}."
      ;
  }

  public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
    @NonNls
    final Map<String, TextAttributesKey> map = new THashMap<String, TextAttributesKey>();
    map.put("k", KEYWORD);
    map.put("d", DOC_COMMENT_TAG);
    map.put("r", RECORDS);
    return map;
  }
}