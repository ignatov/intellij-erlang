package org.intellij.erlang.resolve;

public class ErlangIncludeResolveTest extends ErlangIncludeResolveTestBase {
  public void testRelativeToDirectParent() throws Exception {
    doTest("testmodule.erl", "testinclude.hrl");
  }

  public void testRelativeToDirectParentWithDots() throws Exception {
    doTest("src/testmodule.erl", "include/testinclude.hrl");
  }
}
