package org.intellij.erlang.console;

import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.impl.ConsoleViewImpl;
import com.intellij.execution.impl.EditorHyperlinkSupport;
import com.intellij.execution.ui.ConsoleViewContentType;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.File;

public class ConsoleLinkProbeTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  public void setUp() throws Exception {
    super.setUp();
    FileUtil.copyDir(new File("testData/rebar/sampleProject"), new File(getProject().getBasePath()));
    setUpProjectSdk();
  }

  public void testErlangConsole() { probe(new ErlangConsoleView(getProject())); }
  public void testPlainConsole() { probe((ConsoleViewImpl)TextConsoleBuilderFactory.getInstance().createBuilder(getProject()).getConsole()); }

  private void probe(ConsoleViewImpl console) {
    Disposer.register(getTestRootDisposable(), console);
    console.getComponent();
    ErlangConsoleUtil.attachFilters(getProject(), console);
    String[] lines = {
      "src/a_module.erl:12: syntax error\n",
      "src/a_module.erl:12:5: syntax error\n",
      "** exception error: bad argument in function a_module:f/0 (src/a_module.erl, line 12)\n",
      "     in call from a_module:f/0 (src/a_module.erl:12)\n",
      "[{file,\"src/a_module.erl\"},{line,12}]\n",
      "{file,\"src/a_module.erl\"},{line,12}\n",
      "       location: {a_module,f,0,[{file,\"src/a_module.erl\"},{location,{12,5}}]}\n",
    };
    for (String l : lines) console.print(l, ConsoleViewContentType.NORMAL_OUTPUT);
    console.flushDeferredText();
    console.waitAllRequests();
    EditorHyperlinkSupport links = console.getHyperlinks();
    links.waitForPendingFilters(10000);
    com.intellij.testFramework.PlatformTestUtil.dispatchAllEventsInIdeEventQueue();
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < lines.length; i++) {
      sb.append(getName()).append(" line ").append(i).append(" links=").append(links.findAllHyperlinksOnLine(i).size())
        .append(" : ").append(lines[i]);
    }
    System.out.println("PROBE\n" + sb + "\nTEXT[" + console.getEditor().getDocument().getText() + "]\nALL=" + links.getAllHyperlinks(0, console.getEditor().getDocument().getTextLength()).size() + " editorClass=" + console.getEditor());
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/", ErlangSdkRelease.V_R15B02);
      }
    };
  }
}
