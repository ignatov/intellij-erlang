package org.intellij.erlang.highlighting;

public class TmpPerfTest extends ErlangHighlightingTestBase {
  public void testPerf() throws Exception {
    for (String kind : new String[]{"match", "case"}) {
      StringBuilder sb = new StringBuilder("-export([foo/0]).\nfoo() ->\n  X = 1,\n");
      for (int i = 0; i < 1500; i++) sb.append(kind.equals("match") ? "  X = 1,\n" : "  case X = bar() of _ -> X end,\n");
      sb.append("  X.\nbar() -> 1.\n");
      myFixture.configureByText("a" + kind + ".erl", sb.toString());
      setUpInspections(myFixture);
      long t = System.nanoTime();
      myFixture.doHighlighting();
      long ms = (System.nanoTime() - t) / 1000000;
      java.nio.file.Files.writeString(java.nio.file.Path.of("/tmp/perf-" + kind), ms + "\n", java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND);
    }
  }
}
