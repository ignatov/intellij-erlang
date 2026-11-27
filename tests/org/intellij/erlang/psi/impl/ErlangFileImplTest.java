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

package org.intellij.erlang.psi.impl;

import com.intellij.psi.PsiReferenceService;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangMacrosDefinition;
import org.intellij.erlang.psi.ErlangRecordDefinition;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangFileImplTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testModuleHasNoRules() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("r.erl", "-module(r).\n-export([f/1]).\nf(X) -> X.\n");
    assertEquals(1, file.getFunctions().size());
    assertEmpty(file.getRules());
  }

  // erlc accepts both: the definitions of the two branches, and macros of the same name with different arities.
  public void testMacrosByNameIsTheFirstDefinition() {
    ErlangFile file = (ErlangFile)myFixture.configureByText(
      "m.erl",
      "-module(m).\n-ifdef(debug).\n-define(A, 1).\n-else.\n-define(A, 2).\n-endif.\n" +
      "-define(F(X), X).\n-define(F, 0).\n-define('Q', q).\n");
    ErlangMacrosDefinition a = file.getMacros("A");
    assertNotNull(a);
    assertEquals("-define(A, 1).", a.getText());
    ErlangMacrosDefinition f = file.getMacros("F");
    assertNotNull(f);
    assertEquals("-define(F(X), X).", f.getText());
    ErlangMacrosDefinition q = file.getMacros("Q");
    assertNotNull(q);
    assertEquals("-define('Q', q).", q.getText());
    assertNull(file.getMacros("B"));
  }

  // erlc accepts the record of each branch and the types of the same name with different arities.
  public void testRecordTypeAndImportByName() {
    ErlangFile file = (ErlangFile)myFixture.configureByText(
      "rt.erl",
      "-module(rt).\n-ifdef(debug).\n-record(r, {a}).\n-else.\n-record(r, {b}).\n-endif.\n" +
      "-type t() :: a.\n-type t(X) :: X.\n-import(lists, [map/2]).\n-export_type([t/0, t/1]).\n" +
      "-export([f/1]).\nf(#r{} = R) -> map(fun(X) -> X end, [R]).\n");
    ErlangRecordDefinition r = file.getRecord("r");
    assertNotNull(r);
    assertEquals("-record(r, {a}).", r.getText());
    assertNull(file.getRecord("q"));
    ErlangTypeDefinition t = file.getType("t");
    assertNotNull(t);
    assertEquals("-type t() :: a.", t.getText());
    ErlangTypeDefinition t1 = file.getType("t", 1);
    assertNotNull(t1);
    assertEquals("-type t(X) :: X.", t1.getText());
    assertNull(file.getType("t", 2));
    assertNotNull(file.getImportedFunction("map", 2));
    assertNull(file.getImportedFunction("map", 3));
    assertNull(file.getImportedFunction("foldl", 3));
  }

  public void testNoAutoImportAll() {
    ErlangFile file = (ErlangFile)myFixture.configureByText("n.erl", "-module(n).\n-compile(no_auto_import).\n");
    assertTrue(file.isNoAutoImportAll());
    assertTrue(file.isNoAutoImport("length", 1));
    assertFalse(file.isExportedAll());
  }

  // erlc suppresses the auto-import of the {Name, Arity} pairs only: here size/1 and abs/1, and not length/1, hd/1
  // or tl/1, whose local definitions it reports as ambiguous calls.
  public void testNoAutoImportOnlyOfNameArityPairs() {
    ErlangFile file = (ErlangFile)myFixture.configureByText(
      "s.erl",
      "-module(s).\n" +
      "-compile([{no_auto_import, [{size, 1}, {\"length\", 1}, {hd, x}, {tl, 1, 2}]}, {inline, [f/0]}, {no_auto_import, [abs/1]}]).\n" +
      "-compile({a, b, c}).\n-compile(debug_info).\n-export([f/0]).\nf() -> ok.\n");
    assertFalse(file.isNoAutoImportAll());
    assertTrue(file.isNoAutoImport("size", 1));
    assertTrue(file.isNoAutoImport("abs", 1));
    assertFalse(file.isNoAutoImport("size", 2));
    assertFalse(file.isNoAutoImport("length", 1));
    assertFalse(file.isNoAutoImport("hd", 1));
    assertFalse(file.isNoAutoImport("tl", 1));
    assertFalse(file.isNoAutoImport("inline", 1));
    assertFalse(file.isNoAutoImport("f", 0));
  }

  public void testShouldAskParentForReferences() {
    ErlangFileImpl file = (ErlangFileImpl)myFixture.configureByText("p.erl", "-module(p).\n");
    assertTrue(file.shouldAskParentForReferences(new PsiReferenceService.Hints()));
  }
}
