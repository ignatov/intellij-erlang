package org.intellij.erlang.eunit;

import com.intellij.execution.Executor;
import com.intellij.execution.executors.DefaultRunExecutor;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.openapi.util.text.StringUtil;
import jetbrains.buildServer.messages.serviceMessages.*;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.FileReader;

/**
 * @author savenko
 */
public class ErlangUnitTestEventsConverterTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ErlangUnitTestEventsConverter myTestEventsConverter;
  private LoggingServiceMessageVisitor myServiceMessageVisitor;

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();

    Executor executor = new DefaultRunExecutor();
    ErlangUnitRunConfiguration runConfig = new ErlangUnitRunConfiguration(myFixture.getProject(), "", ErlangUnitRunConfigurationType.getInstance());
    ErlangUnitConsoleProperties consoleProperties = new ErlangUnitConsoleProperties(new RuntimeConfigurationProducer.DelegatingRuntimeConfiguration<ErlangUnitRunConfiguration>(runConfig), executor);
    myTestEventsConverter = new ErlangUnitTestEventsConverter("", consoleProperties);
    myServiceMessageVisitor = new LoggingServiceMessageVisitor();
  }

  @Override
  protected String getTestDataPath() {
    return "testData/eunit/console";
  }

  private void doTest() throws Exception {
    String inputDataFilename = getTestName(true) + ".txt";
    BufferedReader inputReader = new BufferedReader(new FileReader(getTestDataPath() + "/" + inputDataFilename));
    try {
      String lineSeparator = System.getProperty("line.separator");
      String line;
      while ((line = inputReader.readLine()) != null) {
        myTestEventsConverter.processServiceMessages(line + lineSeparator, ProcessOutputTypes.STDOUT, myServiceMessageVisitor);
      }
    } finally {
      inputReader.close();
    }
    assertSameLinesWithFile(getTestDataPath() + "/" + getTestName(true) + "-expected.txt", myServiceMessageVisitor.getLog());
  }

  public void testEunitSingleFunctionTestFailed()     throws Exception { doTest(); }
  public void testEunitSingleFunctionTestOk()         throws Exception { doTest(); }
  public void testEunitMultipleFunctionsTestFailed()  throws Exception { doTest(); }
  public void testEunitMultipleFunctionsTestOk()      throws Exception { doTest(); }

  public void testEunitMultipleModulesTestFailed()    throws Exception { doTest(); }
  public void testEunitMultipleModulesTestOk()        throws Exception { doTest(); }

  public void testRebarSingleFunctionTestFailed()     throws Exception { doTest(); }
  public void testRebarSingleFunctionTestOk()         throws Exception { doTest(); }
  public void testRebarMultipleFunctionsTestFailed()  throws Exception { doTest(); }
  public void testRebarMultipleFunctionsTestOk()      throws Exception { doTest(); }

  public void testRebarSingleSuiteTestFailed()        throws Exception { doTest(); }
  public void testRebarSingleSuiteTestOk()            throws Exception { doTest(); }
  public void testRebarMultipleSuitesTestFailed()     throws Exception { doTest(); }
  public void testRebarMultipleSuitesTestOk()         throws Exception { doTest(); }

  public void testRebarEmptyModule()                  throws Exception { doTest(); }
  public void testRebarNestedModules()                throws Exception { doTest(); }
  public void testRebarMultilineTestOutput()          throws Exception { doTest(); }

  private static class LoggingServiceMessageVisitor implements ServiceMessageVisitor {
    private static final String MY_INDENT = "  ";
    private final StringBuilder myLog = new StringBuilder();
    private String myIndent = "";

    public String getLog() {
      return myLog.toString();
    }

    private void increaseIndent() {
      myIndent += MY_INDENT;
    }

    private void decreaseIndent() {
      myIndent = StringUtil.trimEnd(myIndent, MY_INDENT);
    }

    private void append(String event, String eventDetails) {
      String delimiter = StringUtil.isEmpty(eventDetails) || Character.isWhitespace(eventDetails.charAt(0)) ? ":" : ": ";
      myLog.append(myIndent).append(event).append(delimiter).append(eventDetails).append('\n');
    }

    @Override
    public void visitTestSuiteStarted(@NotNull TestSuiteStarted testSuiteStarted) {
      append("started_suite", testSuiteStarted.getSuiteName());
      increaseIndent();
    }

    @Override
    public void visitTestSuiteFinished(@NotNull TestSuiteFinished testSuiteFinished) {
      decreaseIndent();
      append("finished_suite", testSuiteFinished.getSuiteName());
    }

    @Override
    public void visitTestStarted(@NotNull TestStarted testStarted) {
      append("started_test", testStarted.getTestName());
      increaseIndent();
    }

    @Override
    public void visitTestFinished(@NotNull TestFinished testFinished) {
      decreaseIndent();
      append("finished_test", testFinished.getTestName());
    }

    @Override
    public void visitTestIgnored(@NotNull TestIgnored testIgnored) {
      append("ignored_test", testIgnored.getTestName());
    }

    @Override
    public void visitTestStdOut(@NotNull TestStdOut testStdOut) {
      append("test_output", testStdOut.getStdOut());
    }

    @Override
    public void visitTestStdErr(@NotNull TestStdErr testStdErr) {
      append("failed_test_message", testStdErr.getStdErr());
    }

    @Override
    public void visitTestFailed(@NotNull TestFailed testFailed) {
      append("failed_test", testFailed.getTestName());
    }

    @Override
    public void visitPublishArtifacts(@NotNull PublishArtifacts publishArtifacts) {
    }

    @Override
    public void visitProgressMessage(@NotNull ProgressMessage progressMessage) {
    }

    @Override
    public void visitProgressStart(@NotNull ProgressStart progressStart) {
    }

    @Override
    public void visitProgressFinish(@NotNull ProgressFinish progressFinish) {
    }

    @Override
    public void visitBuildStatus(@NotNull BuildStatus buildStatus) {
    }

    @Override
    public void visitBuildNumber(@NotNull BuildNumber buildNumber) {
    }

    @Override
    public void visitBuildStatisticValue(@NotNull BuildStatisticValue buildStatisticValue) {
    }

    @Override
    public void visitMessageWithStatus(@NotNull Message message) {
    }

    @Override
    public void visitBlockOpened(@NotNull BlockOpened blockOpened) {
    }

    @Override
    public void visitBlockClosed(@NotNull BlockClosed blockClosed) {
    }

    @Override
    public void visitCompilationStarted(@NotNull CompilationStarted compilationStarted) {
    }

    @Override
    public void visitCompilationFinished(@NotNull CompilationFinished compilationFinished) {
    }

    @Override
    public void visitServiceMessage(@NotNull ServiceMessage serviceMessage) {
    }
  }
}
