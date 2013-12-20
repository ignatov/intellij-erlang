package org.intellij.erlang.resolve;

import com.intellij.util.PlatformUtilsCore;

public class ErlangIncludeResolveTest extends ErlangIncludeResolveTestBase {
  public ErlangIncludeResolveTest() {
    super(PlatformUtilsCore.COMMUNITY_PREFIX);
  }

  public void testRelativeToDirectParent() throws Exception {
    doTest("testmodule.erl", "testinclude.hrl");
  }

  public void testRelativeToDirectParentWithDots() throws Exception {
    doTest("src/testmodule.erl", "include/testinclude.hrl");
  }
}
