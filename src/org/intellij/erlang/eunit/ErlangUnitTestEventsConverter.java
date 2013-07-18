package org.intellij.erlang.eunit;

import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.ServiceMessageBuilder;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.Stack;
import jetbrains.buildServer.messages.serviceMessages.ServiceMessageVisitor;
import org.jetbrains.annotations.NotNull;

import java.text.ParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author savenko
 */
public class ErlangUnitTestEventsConverter extends OutputToGeneralTestEventsConverter {
  private static final String EOL = System.getProperty("line.separator");
  private static final Pattern MODULE = Pattern.compile("^(\\s*)module \'(\\w+)\'" + EOL + "$");
  private static final Pattern DONE = Pattern.compile("^\\s*\\[done in \\d+(?:\\.\\d+)? s\\]" + EOL + "$");
  private static final Pattern TEST = Pattern.compile("^(\\s*)(\\w+)(?::\\d+)?: (\\w+)(?: ?\\(.*\\))?\\.{3}");
  private static final Pattern TEST_RESULT = Pattern.compile("^\\s*(?:(?:\\*(failed)\\*)|(?:(?:\\[\\d+(?:\\.\\d+)? s\\] )?(ok)))" + EOL + "$");
  private static final Pattern END = Pattern.compile("^=+" + EOL + "$");

  private StringBuilder myOutput = new StringBuilder();
  private Stack<ErlangTestModuleInfo> myActiveTestSuites = new Stack<ErlangTestModuleInfo>();
  private ErlangTestFunctionInfo myActiveTest = null;

  private Key myOutputType = null;
  private ServiceMessageVisitor myVisitor = null;

  public ErlangUnitTestEventsConverter(@NotNull String testFrameworkName, @NotNull TestConsoleProperties consoleProperties) {
    super(testFrameworkName, consoleProperties);
  }

  @Override
  public boolean processServiceMessages(String text, Key outputType, ServiceMessageVisitor visitor) throws ParseException {
    Matcher m;

    myOutputType = outputType;
    myVisitor = visitor;

    if ((m = MODULE.matcher(text)).matches()) {
      String moduleName = m.group(2);
      int indentationLevel = m.group(1).length();
      return startTestSuite(moduleName, indentationLevel);
    }
    else if (DONE.matcher(text).matches()) {
      return finishActiveSuite();
    } else if (END.matcher(text).matches()) {
      return finishAllActiveSuites();
    }
    else if ((m = TEST.matcher(text)).find()) {
      String moduleName = m.group(2);
      String testName = m.group(3);
      int indentationLevel = m.group(1).length();
      int matchEnd = m.end();
      return startTest(indentationLevel, moduleName, testName) && processServiceMessages(text.substring(matchEnd), myOutputType, myVisitor);
    }
    else if (myActiveTest != null) {
      if (myActiveTest.isFailed() && StringUtil.isEmptyOrSpaces(text)) {
        return finishActiveTest();
      }
      else if ((m = TEST_RESULT.matcher(text)).matches()) {
        String failed = m.group(1);
        String ok = m.group(2);
        return (ok != null && failed == null) || failActiveTest();
      }
      myOutput.append(text);
    }

    return true;
  }

  private boolean startTestSuite(String moduleName, int indentationLevel) throws ParseException {
    boolean result = finishActiveTest();
    boolean skipSuite = false;
    if (!myActiveTestSuites.isEmpty()) {
      ErlangTestModuleInfo activeSuite = myActiveTestSuites.peek();
      if (activeSuite.getIndentationLevel() >= indentationLevel) {
        result = finishActiveSuite();
      } else {
        skipSuite = moduleName.equals(activeSuite.getModuleName() + "_tests");
      }
    }
    myActiveTestSuites.push(new ErlangTestModuleInfo(moduleName, indentationLevel, skipSuite));
    if (!skipSuite) {
      result &= processSuper(setLocation(ServiceMessageBuilder.testSuiteStarted(moduleName), moduleName));
    }
    return result;
  }

  private boolean finishActiveSuite() throws ParseException {
    boolean result = finishActiveTest();
    ErlangTestModuleInfo suite = myActiveTestSuites.tryPop();
    if (suite != null && !suite.isSkipped()) {
      String moduleName = suite.getModuleName();
      return result & processSuper(setLocation(ServiceMessageBuilder.testSuiteFinished(moduleName), moduleName));
    }
    return result;
  }

  private boolean finishAllActiveSuites() throws ParseException {
    boolean result = true;
    while (!myActiveTestSuites.isEmpty()) {
      result &= finishActiveSuite();
    }
    return result;
  }

  private boolean startTest(int indentationLevel, String module, String test) throws ParseException {
    boolean result = finishActiveTest();
    ErlangTestModuleInfo activeSuite = myActiveTestSuites.isEmpty() ? null : myActiveTestSuites.peek();
    if (activeSuite == null || !activeSuite.getModuleName().equals(module)) {
      result &= startTestSuite(module, indentationLevel);
    }
    myActiveTest = new ErlangTestFunctionInfo(test, myActiveTestSuites.peek());
    return result & processSuper(setLocation(ServiceMessageBuilder.testStarted(test), module, test));
  }

  private boolean failActiveTest() throws ParseException {
    if (myActiveTest == null) return true;
    myActiveTest.setFailed(true);
    return processSuper(ServiceMessageBuilder.testFailed(myActiveTest.getFunctionName()).addAttribute("message", ""));
  }

  private boolean finishActiveTest() throws ParseException {
    if (myActiveTest == null) return true;
    String test = myActiveTest.getFunctionName();
    String module = myActiveTest.getModuleInfo().getModuleName();
    boolean result = true;
    if (myOutput.length() != 0) {
      ServiceMessageBuilder builder = myActiveTest.isFailed() ? ServiceMessageBuilder.testStdErr(test) : ServiceMessageBuilder.testStdOut(test);
      result = processSuper(setOutput(builder, myOutput.toString()));
      myOutput.setLength(0);
    }
    myActiveTest = null;
    return result & processSuper(setLocation(ServiceMessageBuilder.testFinished(test), module));
  }

  private boolean processSuper(ServiceMessageBuilder messageBuilder) throws ParseException {
    return super.processServiceMessages(messageBuilder.toString(), myOutputType, myVisitor);
  }

  private static ServiceMessageBuilder setOutput(ServiceMessageBuilder builder, String output) {
    return builder.addAttribute("out", output);
  }

  private static ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module, String test) {
    return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module + ":" + test);
  }

  private static ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module) {
    return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module);
  }

  private static class ErlangTestModuleInfo {
    private final String myModuleName;
    private final int myIndentationLevel;
    private final boolean mySkipped;

    public ErlangTestModuleInfo(String name, int indentationLevel, boolean skipped) {
      myModuleName = name;
      myIndentationLevel = indentationLevel;
      mySkipped = skipped;
    }

    public String getModuleName() {
      return myModuleName;
    }

    public int getIndentationLevel() {
      return myIndentationLevel;
    }

    private boolean isSkipped() {
      return mySkipped;
    }
  }

  private static class ErlangTestFunctionInfo {
    private final String myFunctionName;
    private final ErlangTestModuleInfo myModuleInfo;
    private boolean myFailed = false;

    private ErlangTestFunctionInfo(String functionName, ErlangTestModuleInfo moduleInfo) {
      myFunctionName = functionName;
      myModuleInfo = moduleInfo;
    }

    private String getFunctionName() {
      return myFunctionName;
    }

    private ErlangTestModuleInfo getModuleInfo() {
      return myModuleInfo;
    }

    private boolean isFailed() {
      return myFailed;
    }

    private void setFailed(boolean failed) {
      myFailed = failed;
    }
  }
}
