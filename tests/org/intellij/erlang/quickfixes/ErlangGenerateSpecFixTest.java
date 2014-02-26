package org.intellij.erlang.quickfixes;

import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.util.containers.ContainerUtil;

import java.util.List;

public class ErlangGenerateSpecFixTest extends ErlangQuickFixTestBase {
  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/quickfixes/generate_spec/";
  }

  private void doGenerateSpecTest() { doTest(ErlangGenerateSpecFix.NAME); }

  public void testNoArgs() throws Throwable { doGenerateSpecTest(); }
  public void testWithArgs() throws Throwable { doGenerateSpecTest(); }

  public void testNoActionIfSpecExists() throws Throwable {
    final String testName = getTestName(true);
    myFixture.configureByFile(testName + ".erl");
    List<IntentionAction> availableIntentions = myFixture.filterAvailableIntentions(ErlangGenerateSpecFix.NAME);
    IntentionAction action = ContainerUtil.getFirstItem(availableIntentions);
    assertNull("Generate spec fix should not be available if spec already exists.", action);
  }
}
