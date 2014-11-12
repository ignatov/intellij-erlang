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

package org.intellij.erlang.debugger;

import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.debugger.xdebug.ErlangSourcePosition;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunClause;
import org.intellij.erlang.psi.ErlangFunExpression;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangSourcePositionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MODULE_NAME = "erlang-source-position";
  private ErlangFile myErlangFile;

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    myFixture.configureByFile(MODULE_NAME + ".erl");
    myErlangFile = (ErlangFile) myFixture.getFile();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/debugger/";
  }

  public void testFunctionSourcePositionConstructor() throws Exception {
    ErlangSourcePosition sourcePosition = ErlangSourcePosition.create(getProject(), MODULE_NAME, "function", 0);
    ErlangFunction function = myErlangFile.getFunction("function", 0);

    assertNotNull(sourcePosition);
    assertNotNull(function);
    assertEquals(MODULE_NAME, sourcePosition.getErlangModuleName());
    assertEquals(myErlangFile.getVirtualFile(), sourcePosition.getFile());
    assertEquals(function.getTextOffset(), sourcePosition.getSourcePosition().getOffset());
    assertEquals(0, sourcePosition.getFunctionArity());
    assertEquals(-1, sourcePosition.getFunExpressionArity());
  }

  public void testFunExpressionSourcePositionConstructor() throws Exception {
    ErlangSourcePosition sourcePosition =
      ErlangSourcePosition.create(getProject(), MODULE_NAME, "-function_with_fun_expression/0-fun-0-", 0);
    ErlangFunction function = myErlangFile.getFunction("function_with_fun_expression", 0);
    ErlangFunExpression funExpression = PsiTreeUtil.findChildOfType(function, ErlangFunExpression.class, true);
    ErlangFunClause funExpressionClause = PsiTreeUtil.findChildOfType(funExpression, ErlangFunClause.class, true);

    assertNotNull(sourcePosition);
    assertNotNull(function);
    assertNotNull(funExpression);
    assertNotNull(funExpressionClause);

    assertEquals(MODULE_NAME, sourcePosition.getErlangModuleName());
    assertEquals(myErlangFile.getVirtualFile(), sourcePosition.getFile());
    assertEquals(0, sourcePosition.getFunctionArity());
    assertEquals(funExpressionClause.getArgumentDefinitionList().getArgumentDefinitionList().size(), sourcePosition.getFunExpressionArity());
    assertEquals(funExpression.getTextOffset(), sourcePosition.getSourcePosition().getOffset());
  }
}
