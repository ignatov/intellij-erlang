package org.intellij.erlang.highlighting;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.inspection.ErlangIoFormatInspection;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

import java.util.List;

public class Erlang27HighlightingTest extends ErlangHighlightingTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R27/", ErlangSdkRelease.V_27_0);
      }
    };
  }

  public void testErlang27SyntaxNoError() {
    enableErlang27SyntaxInspection();
    doTest();
  }

  public void testIoFormatSigils() {
    myFixture.addFileToProject("io.erl", "-module(io).\n-export([format/2]).\nformat(_, _) -> ok.\n");
    myFixture.addFileToProject("io_lib.erl", "-module(io_lib).\n-export([format/2]).\nformat(_, _) -> [].\n");
    doTest();
  }

  // claim 4 (#1127), confirmed: ~s#~# and ~S#~# get no io:format warning
  public void _testIoFormatSigilWithHashDelimiter() {
    myFixture.addFileToProject("io.erl", "-module(io).\n-export([format/2]).\nformat(_, _) -> ok.\n");
    myFixture.configureByText("a.erl", """
      -module(a).
      -export([p/0, s/0, b/0]).
      p() -> io:format("~", [1]).
      s() -> io:format(~s#~#, [1]).
      b() -> io:format(~S#~#, [1]).
      """);
    myFixture.enableInspections(ErlangIoFormatInspection.class);
    List<HighlightInfo> infos = myFixture.doHighlighting(HighlightSeverity.WARNING);
    List<String> withoutWarning = ContainerUtil.filter(List.of("p()", "s()", "b()"), function -> {
      int start = myFixture.getFile().getText().indexOf(function + " ->");
      int end = myFixture.getFile().getText().indexOf('\n', start);
      return !ContainerUtil.exists(infos, info -> info.getStartOffset() >= start && info.getEndOffset() <= end);
    });
    assertEmpty("functions without an io:format warning: " + withoutWarning + ", warnings: " + infos, withoutWarning);
  }

  public void testErlang29SyntaxError() {
    enableErlang29SyntaxInspection();
    doTest();
  }

  public void testErlang29SyntaxSingleValuedComprehensions() {
    enableErlang29SyntaxInspection();
    doTest();
  }

  public void testAutoImportByRelease27() {
    addErlangModuleWithFunctionsAutoImportedSince28();
    doTest();
  }
}
