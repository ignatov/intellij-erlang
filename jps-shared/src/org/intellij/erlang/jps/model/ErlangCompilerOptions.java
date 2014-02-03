package org.intellij.erlang.jps.model;

import com.intellij.util.xmlb.annotations.Tag;

public class ErlangCompilerOptions {
  public ErlangCompilerOptions() {
  }

  public ErlangCompilerOptions(ErlangCompilerOptions options) {
    myUseRebarCompiler = options.myUseRebarCompiler;
  }

  @Tag("useRebarCompiler")
  public boolean myUseRebarCompiler = false;

  @Tag("useDebugInfo")
  public boolean myAddDebugInfoEnabled = true;
}
