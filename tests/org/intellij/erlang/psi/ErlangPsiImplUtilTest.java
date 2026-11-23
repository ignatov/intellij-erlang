/*
 * Copyright 2012-2014 Sergey Ignatov
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

package org.intellij.erlang.psi;

import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ProcessingContext;
import org.intellij.erlang.ErlangStringLiteralEscaper;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.psi.impl.ErlangFunctionReferenceImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.List;

public class ErlangPsiImplUtilTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void testStandaloneAtomReferenceRename() {
    myFixture.configureByText("a.erl", "f() -> fo<caret>o.");
    ErlangQAtom atom = getElementAtCaret(ErlangQAtom.class);
    PsiReference reference = atom.getReference();
    assertNotNull(reference);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { reference.handleElementRename("Bar"); });
    myFixture.checkResult("f() -> 'Bar'.");
  }

  public void testRecordFieldInRecordTypeHasReferenceOnItsName() {
    myFixture.configureByText("a.erl", "-record(r, {a, b}).\n-type t() :: #r{<caret>a :: integer()}.");
    ErlangFieldType fieldType = getElementAtCaret(ErlangFieldType.class);
    PsiReference reference = fieldType.getReference();
    assertNotNull(reference);
    assertSame(fieldType, reference.getElement());
    assertEquals("a", reference.getRangeInElement().substring(fieldType.getText()));
  }

  public void testRecordFieldInRecordTypeResolvesToRecordField() {
    myFixture.configureByText("a.erl", "-record(r, {a, b}).\n-type t() :: #r{<caret>a :: integer()}.");
    PsiElement resolved = myFixture.getReferenceAtCaretPositionWithAssertion().resolve();
    assertInstanceOf(resolved, ErlangTypedExpr.class);
    assertEquals("a", ((ErlangTypedExpr) resolved).getName());
  }

  public void testRecordFieldResolvesThroughMacroRecordName() {
    myFixture.configureByText("a.erl", "-record(rec, {a, b}).\n-define(R, #rec).\nf() -> ?R{<caret>b = 1}.");
    PsiElement resolved = myFixture.getReferenceAtCaretPositionWithAssertion().resolve();
    assertInstanceOf(resolved, ErlangTypedExpr.class);
    assertEquals("b", ((ErlangTypedExpr) resolved).getName());
  }

  public void testUnknownRecordFieldThroughMacroRecordNameIsUnresolved() {
    myFixture.configureByText("a.erl", "-record(rec, {a, b}).\n-define(R, #rec).\nf() -> ?R{<caret>c = 1}.");
    assertNull(myFixture.getReferenceAtCaretPositionWithAssertion().resolve());
  }

  public void testIncludeStringCannotBeBoundToNonFile() {
    myFixture.addFileToProject("header.hrl", "-define(HEADER, 1).\n");
    myFixture.configureByText("a.erl", "-include(\"hea<caret>der.hrl\").\n");
    ErlangIncludeString includeString = getElementAtCaret(ErlangIncludeString.class);
    PsiReference reference = includeString.getReference();
    assertNotNull(reference);
    assertInstanceOf(reference.resolve(), ErlangFile.class);
    try {
      WriteCommandAction.runWriteCommandAction(getProject(), () -> { reference.bindToElement(includeString); });
      fail("IncorrectOperationException expected");
    }
    catch (IncorrectOperationException e) {
      assertEquals("Unexpected element", e.getMessage());
    }
    myFixture.checkResult("-include(\"header.hrl\").\n");
  }

  public void testVariableInLeftPartOfMaybeMatch() {
    myFixture.configureByText("a.erl", "f() -> maybe {ok, <caret>A} ?= g(), A end.\ng() -> {ok, 1}.");
    ErlangQVar declaration = getElementAtCaret(ErlangQVar.class);
    assertTrue(ErlangPsiImplUtil.inLeftPartOfAssignment(declaration));
    List<ErlangQVar> vars = List.copyOf(PsiTreeUtil.findChildrenOfType(myFixture.getFile(), ErlangQVar.class));
    assertEquals(2, vars.size());
    assertFalse(ErlangPsiImplUtil.inLeftPartOfAssignment(vars.get(1)));
  }

  public void testNoLookupElementsOutsideErlangFiles() {
    PsiFile text = myFixture.configureByText("a.txt", "foo");
    assertEmpty(ErlangPsiImplUtil.getFunctionLookupElements(text, false, null));
    assertEmpty(ErlangPsiImplUtil.getMacrosLookupElements(text));
    assertEmpty(ErlangPsiImplUtil.getRecordLookupElements(text));
    assertEmpty(ErlangPsiImplUtil.getTypeLookupElements(text, true, false, false));
  }

  public void testNoFunctionLookupElementsInApplicationFile() {
    PsiFile app = myFixture.configureByText("a.app", "{application, a, []}.");
    assertInstanceOf(app, ErlangFile.class);
    assertEmpty(ErlangPsiImplUtil.getFunctionLookupElements(app, false, null));
  }

  public void testRenameAtomWithoutAtom() {
    assertFalse(ErlangPsiImplUtil.renameAtom(null, "foo"));
    assertFalse(ErlangPsiImplUtil.renameQAtom(null, "foo"));
  }

  public void testSpecificationNameIdentifierAndSetName() {
    myFixture.configureByText("a.erl", "-spec fo<caret>o() -> ok.\nfoo() -> ok.");
    ErlangSpecification spec = getElementAtCaret(ErlangSpecification.class);
    PsiElement nameIdentifier = spec.getNameIdentifier();
    assertEquals("foo", nameIdentifier.getText());
    assertEquals(6, nameIdentifier.getTextOffset());
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { spec.setName("bar"); });
    myFixture.checkResult("-spec bar() -> ok.\nfoo() -> ok.");
  }

  public void testTypeDefinitionTextOffset() {
    myFixture.configureByText("a.erl", "-type my_<caret>type() :: ok.");
    ErlangTypeDefinition type = getElementAtCaret(ErlangTypeDefinition.class);
    assertEquals(6, type.getTextOffset());
    assertEquals("my_type", type.getNameIdentifier().getText());
  }

  public void testModuleSetNameRenamesFile() {
    myFixture.configureByText("old_name.erl", "-module(old<caret>_name).\n");
    ErlangModule module = getElementAtCaret(ErlangModule.class);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { module.setName("new_name"); });
    assertEquals("new_name.erl", myFixture.getFile().getName());
    myFixture.checkResult("-module(new_name).\n");
  }

  public void testMacroSetNameQuotesName() {
    myFixture.configureByText("a.erl", "-define(FO<caret>O, 1).\n");
    ErlangMacrosDefinition macro = getElementAtCaret(ErlangMacrosDefinition.class);
    WriteCommandAction.runWriteCommandAction(getProject(), () -> { macro.setName("1abc"); });
    myFixture.checkResult("-define('1abc', 1).\n");
  }

  public void testFunctionPresentation() {
    myFixture.configureByText("a.erl", "'my f<caret>un'(A, B) -> {A, B}.");
    ErlangFunction function = getElementAtCaret(ErlangFunction.class);
    ItemPresentation presentation = function.getPresentation();
    assertEquals("'my fun'/2", presentation.getPresentableText());
    assertEquals("a.erl", presentation.getLocationString());
    assertSame(ErlangIcons.FUNCTION, presentation.getIcon(false));
    assertSame(ErlangIcons.FUNCTION, function.getIcon(0));
  }

  public void testStringLiteralUpdateTextAndEscaper() {
    myFixture.configureByText("a.erl", "f() -> \"a<caret>bc\".");
    ErlangStringLiteral literal = getElementAtCaret(ErlangStringLiteral.class);
    ErlangStringLiteralEscaper escaper = literal.createLiteralTextEscaper();
    StringBuilder decoded = new StringBuilder();
    assertTrue(escaper.decode(TextRange.create(1, 4), decoded));
    assertEquals("abc", decoded.toString());
    assertTrue(escaper.isOneLine());
    ErlangStringLiteral updated = WriteCommandAction.writeCommandAction(getProject())
      .compute(() -> literal.updateText("\"xyz\""));
    assertEquals("\"xyz\"", updated.getText());
    myFixture.checkResult("f() -> \"xyz\".");
  }

  public void testNotParenthesizedExpression() {
    myFixture.configureByText("a.erl", "f() -> ((1)).");
    ErlangParenthesizedExpression outer = PsiTreeUtil.findChildOfType(myFixture.getFile(), ErlangParenthesizedExpression.class);
    assertNotNull(outer);
    assertEquals("((1))", outer.getText());
    ErlangExpression expression = ErlangPsiImplUtil.getNotParenthesizedExpression(outer);
    assertInstanceOf(expression, ErlangMaxExpression.class);
    assertEquals("1", expression.getText());
  }

  public void testFunctionCallPrecedence() {
    myFixture.configureByText("a.erl", "f() -> <caret>g(1) + 2.\ng(X) -> X.");
    ErlangFunctionCallExpression call = getElementAtCaret(ErlangFunctionCallExpression.class);
    assertEquals(11, ErlangPsiImplUtil.getExpressionPrecedence(call));
    assertEquals(7, ErlangPsiImplUtil.getExpressionPrecedence(call.getParent()));
  }

  public void testFunctionReferenceOutsideCallHasNoArity() {
    myFixture.configureByText("a.erl", "f() -> fo<caret>o.");
    ErlangFunctionReferenceImpl reference = ErlangPsiImplUtil.createFunctionReference(getElementAtCaret(ErlangQAtom.class));
    assertEquals("foo", reference.getName());
    assertEquals(-1, reference.getArity());
  }

  public void testIsWhitespaceOrComment() {
    myFixture.configureByText("a.erl", "% comment\nf() -> ok.");
    PsiComment comment = PsiTreeUtil.findChildOfType(myFixture.getFile(), PsiComment.class);
    assertNotNull(comment);
    assertTrue(ErlangPsiImplUtil.isWhitespaceOrComment(comment));
    PsiElement ok = myFixture.getFile().findElementAt(myFixture.getFile().getText().indexOf("ok"));
    assertNotNull(ok);
    assertFalse(ErlangPsiImplUtil.isWhitespaceOrComment(ok));
  }

  public void testNotFromPreviousFunction() {
    myFixture.configureByText("a.erl", "f() -> ok.\n-spec g() -> ok.\ng() -> ok.\nh() -> ok.");
    ErlangFile file = (ErlangFile) myFixture.getFile();
    ErlangSpecification spec = file.getSpecifications().getFirst();
    assertTrue(ErlangPsiImplUtil.notFromPreviousFunction(spec, null));
    assertTrue(ErlangPsiImplUtil.notFromPreviousFunction(spec, file.getFunction("f", 0)));
    assertFalse(ErlangPsiImplUtil.notFromPreviousFunction(spec, file.getFunction("h", 0)));
  }

  public void testAtomNameWithUnescapedQuote() {
    assertNull(ErlangPsiImplUtil.toAtomName("a'b"));
    assertFalse(ErlangPsiImplUtil.needQuotation("a'b"));
  }

  public void testAtomNameTooLongToMatchIsUnsure() {
    String name = "A" + "a".repeat(1_000_000);
    assertNull(ErlangPsiImplUtil.toAtomName(name));
    assertFalse(ErlangPsiImplUtil.needQuotation(name));
  }

  public void testTypeFromIncludeResolves() {
    myFixture.addFileToProject("types.hrl", "-type inc_t() :: ok.\n");
    myFixture.configureByText("a.erl", "-include(\"types.hrl\").\n-spec f() -> inc_<caret>t().\nf() -> ok.");
    PsiElement resolved = myFixture.getReferenceAtCaretPositionWithAssertion().resolve();
    assertInstanceOf(resolved, ErlangTypeDefinition.class);
    assertEquals("types.hrl", resolved.getContainingFile().getName());
  }

  public void testFunctionCallArgumentPatternsAcceptByDefault() {
    myFixture.configureByText("a.erl", "f() -> g(<caret>foo, 1).\ng(A, B) -> {A, B}.");
    ErlangQAtom atom = getElementAtCaret(ErlangQAtom.class);
    ErlangPsiImplUtil.ErlangFunctionCallArgumentBase<PsiElement> anyArgument =
      new ErlangPsiImplUtil.ErlangFunctionCallArgumentBase<>("anyArgument") {};
    ErlangPsiImplUtil.ErlangFunctionCallQAtomArgument atomArgument =
      new ErlangPsiImplUtil.ErlangFunctionCallQAtomArgument("atomArgument") {};
    assertTrue(anyArgument.accepts(atom, new ProcessingContext()));
    assertTrue(atomArgument.accepts(atom, new ProcessingContext()));
    ErlangQAtom function = PsiTreeUtil.findChildOfType(myFixture.getFile(), ErlangQAtom.class);
    assertNotNull(function);
    assertEquals("f", function.getText());
    assertFalse(anyArgument.accepts(function, new ProcessingContext()));
    assertFalse(atomArgument.accepts(function, new ProcessingContext()));
  }

  public void testAtomArgumentIsNotModuleWhenGuardConstrainsOtherVariable() {
    myFixture.configureByText("a.erl",
                              "-spec call(M, F) -> M when F :: module().\n" +
                              "call(M, _) -> M.\n" +
                              "f() -> call(lis<caret>ts, reverse).");
    ErlangQAtom atom = getElementAtCaret(ErlangQAtom.class);
    assertFalse(new ErlangPsiImplUtil.ErlangFunctionCallModuleArgument().accepts(atom, new ProcessingContext()));
  }

  public void testAtomArgumentIsModuleWhenGuardConstrainsItsVariable() {
    myFixture.configureByText("a.erl",
                              "-spec call(M, F) -> ok when M :: module(), F :: atom().\n" +
                              "call(_, _) -> ok.\n" +
                              "f() -> call(lis<caret>ts, reverse).");
    ErlangQAtom atom = getElementAtCaret(ErlangQAtom.class);
    assertTrue(new ErlangPsiImplUtil.ErlangFunctionCallModuleArgument().accepts(atom, new ProcessingContext()));
  }
}
