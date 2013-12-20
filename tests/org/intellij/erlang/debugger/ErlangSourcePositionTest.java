package org.intellij.erlang.debugger;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.impl.XSourcePositionImpl;
import org.intellij.erlang.debugger.xdebug.ErlangSourcePosition;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

public class ErlangSourcePositionTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ErlangFile myErlangFile;

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    myFixture.configureByFile("erlang-source-position.erl");
    myErlangFile = (ErlangFile) myFixture.getFile();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/debugger/";
  }

  public void testFunctionSourcePositionConstructor() throws Exception {
    ErlangSourcePosition sourcePosition = new ErlangSourcePosition(myErlangFile, "function", 0);
    ErlangFunction function = myErlangFile.getFunction("function", 0);
    assertNotNull(function);
    assertEquals(function, sourcePosition.getFunction());
    int functionLineNumber = StringUtil.offsetToLineNumber(myErlangFile.getText(), function.getTextOffset());
    assertEquals(functionLineNumber, sourcePosition.getLine());
    assertNull(sourcePosition.getFunExpression());
  }

  public void testFunExpressionSourcePositionConstructor() throws Exception {
    ErlangSourcePosition sourcePosition = new ErlangSourcePosition(myErlangFile, "-function_with_fun_expression/0-fun-0-", 0);
    ErlangFunction function = myErlangFile.getFunction("function_with_fun_expression", 0);
    assertNotNull(function);
    assertEquals(function, sourcePosition.getFunction());
    assertNotNull(sourcePosition.getFunExpression());
    assertTrue(PsiTreeUtil.isAncestor(function, sourcePosition.getFunExpression(), true));
    int funExpressionLineNumber = StringUtil.offsetToLineNumber(myErlangFile.getText(), sourcePosition.getFunExpression().getTextOffset());
    assertEquals(funExpressionLineNumber, sourcePosition.getLine());
  }

  public void testXSourcePositionConstructor() throws Exception {
    XSourcePosition xSourcePosition = XSourcePositionImpl.create(myErlangFile.getVirtualFile(), 3);
    assertNotNull(xSourcePosition);
    ErlangSourcePosition sourcePosition = new ErlangSourcePosition(getProject(), xSourcePosition);
    assertEquals(xSourcePosition.getLine(), sourcePosition.getLine());
    assertEquals(myErlangFile, sourcePosition.getErlangFile());
  }
}
