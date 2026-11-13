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

package org.intellij.erlang.documentation;

import junit.framework.TestCase;

public class ErlangSdkDocProviderBaseTest extends TestCase {
  public void testHttpProtocols() {
    assertTrue(ErlangSdkDocProviderBase.isHttpProtocol("http"));
    assertTrue(ErlangSdkDocProviderBase.isHttpProtocol("https"));
    assertFalse(ErlangSdkDocProviderBase.isHttpProtocol("file"));
  }

  public void testOfficialDocumentationUrls() {
    assertTrue(ErlangSdkDocProviderBase.isOfficialErlangDocumentationUrl(
      "http://www.erlang.org/documentation/doc-17.1"));
    assertTrue(ErlangSdkDocProviderBase.isOfficialErlangDocumentationUrl(
      "https://www.erlang.org/docs/29"));
    assertFalse(ErlangSdkDocProviderBase.isOfficialErlangDocumentationUrl(
      "https://docs.example.com/erlang"));
  }

  public void testApplicationNames() {
    assertEquals("stdlib", ErlangSdkDocProviderBase.getApplicationName("stdlib-8.1"));
    assertEquals("erts", ErlangSdkDocProviderBase.getApplicationName("erts-17.1"));
    assertEquals("syntax_tools", ErlangSdkDocProviderBase.getApplicationName("syntax_tools-3.2"));
  }

  public void testEmptyLink() {
    assertEquals("", ErlangSdkDocProviderBase.convertLink("", "lists"));
  }

  public void testModernLinks() {
    assertEquals("psi_element://lists#foreach/2",
                 ErlangSdkDocProviderBase.convertLink("#foreach/2", "lists"));
    assertEquals("psi_element://lists#t:filename/0",
                 ErlangSdkDocProviderBase.convertLink("#t:filename/0", "lists"));
    assertEquals("psi_element://file#open/2",
                 ErlangSdkDocProviderBase.convertLink("../../apps/kernel/file.html#open/2", "lists"));
    assertEquals("psi_element://unicode#t:chardata/0",
                 ErlangSdkDocProviderBase.convertLink("unicode.html#t:chardata/0", "io"));
  }

  public void testAbsoluteLinksAreKept() {
    assertEquals("https://github.com/erlang/otp/blob/master/lib/stdlib/doc/lists.html#foreach",
                 ErlangSdkDocProviderBase.convertLink("https://github.com/erlang/otp/blob/master/lib/stdlib/doc/lists.html#foreach", "lists"));
  }

  public void testLocalAnchor() {
    assertEquals("psi_element://lists#member-2",
                 ErlangSdkDocProviderBase.convertLink("#member-2", "lists"));
  }
}
