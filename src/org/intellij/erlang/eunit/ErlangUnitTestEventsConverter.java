package org.intellij.erlang.eunit;

import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.ServiceMessageBuilder;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import jetbrains.buildServer.messages.serviceMessages.ServiceMessageVisitor;

import java.text.ParseException;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.execution.testframework.sm.ServiceMessageBuilder.*;

/**
* @author savenko
*/
class ErlangUnitTestEventsConverter extends OutputToGeneralTestEventsConverter {

  String myCurrentModule;
  String myCurrentTest;
  String myCurrentFailedTest;
  Key myCurrentOutputType;
  ServiceMessageVisitor myCurrentVisitor;
  boolean myFailed;
  String myStdOut;
  Set<String> myFailedTests;

  public ErlangUnitTestEventsConverter(String testFrameworkName, TestConsoleProperties consoleProperties) {
    super(testFrameworkName, consoleProperties);
    myCurrentModule = "";
    myCurrentTest = "";
    myCurrentFailedTest = "";
    myCurrentOutputType = null;
    myCurrentVisitor = null;
    myFailed = false;
    myStdOut = "";
    myFailedTests = new HashSet<String>();
  }

  @Override
  public boolean processServiceMessages(String text, Key outputType, ServiceMessageVisitor visitor) throws ParseException {
    Matcher m;

    Pattern OK = Pattern.compile("(?:  )?(\\w+):?\\d*: (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
    Pattern OK_ONE_TEST = Pattern.compile("(\\w+):?\\d*: (\\w+) .*\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
    Pattern FAILED = Pattern.compile("(?:  )?(\\w+):?\\d*: (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");
    Pattern FAILED_ONE_TEST = Pattern.compile("(\\w+):?\\d*: (\\w+).*\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");
    Pattern MODULE = Pattern.compile("(?:  )?module \'(\\w+)\'" + System.getProperty("line.separator"));

    myCurrentOutputType = outputType;
    myCurrentVisitor = visitor;

    if ((m = MODULE.matcher(text)).matches()) {
      String module = m.group(1);
      return startTestSuite(module);
    }
    else if (StringUtil.startsWith(text, "  [done")) {
      return finishTestSuite();
    }
    else if ((m = OK.matcher(text)).find() || (m = OK_ONE_TEST.matcher(text)).find()) {
      String module = m.group(1);
      String test = m.group(2);
      return startTest(test, module) && finishTest();
    }
    else if ((m = FAILED.matcher(text)).find() || (m = FAILED_ONE_TEST.matcher(text)).find() || text.trim().equals("undefined")) {
      myFailed = true;
      myStdOut = "";
      if (text.trim().equals("undefined")) {
        myCurrentFailedTest = "undefined";
        return startTest("undefined", myCurrentModule);
      }
      else {
        String module = m.group(1);
        String test = m.group(2);
        myCurrentFailedTest = test;
        return startTest(test, module);
      }
    }
    else if (text.startsWith("ERROR:") && !myFailedTests.contains(myCurrentFailedTest)) {
      myStdOut += text;
      return failTest() && finishTest(myCurrentFailedTest);
    }
    else if (myFailed) {
      if (StringUtil.isEmptyOrSpaces(text)) {
        myFailed = false;
        myFailedTests.add(myCurrentFailedTest);
        return failTest() && finishTest(myCurrentFailedTest);
      }
    }
    else if (text.startsWith("=======================================================")){
      return finishTestSuite();
    }
    myStdOut += text;
    return true;
  }

  private boolean failTest() throws ParseException {
    return super.processServiceMessages(testFailed(myCurrentFailedTest).addAttribute("message", myStdOut).toString(), myCurrentOutputType, myCurrentVisitor);
  }

  private boolean startTest(String test, String module) throws ParseException {
    boolean result = true;
    if (!myCurrentModule.equals(module)) {
      if (!myCurrentModule.isEmpty()) {
        result = finishTestSuite();
      }
      result &= startTestSuite(module);
    }
    myCurrentTest = test;
    ServiceMessageBuilder serviceMessageBuilder = setLocation(testStarted(test), module, test);
    return result && super.processServiceMessages(serviceMessageBuilder.toString(), myCurrentOutputType, myCurrentVisitor);
  }

  private boolean finishTest() throws ParseException {
    return finishTest(myCurrentTest);
  }

  private boolean finishTest(String test) throws ParseException {
    return super.processServiceMessages(testFinished(test).toString(), myCurrentOutputType, myCurrentVisitor);
  }

  private boolean startTestSuite(String module) throws ParseException {
    myCurrentModule = module;
    ServiceMessageBuilder builder = setLocation(ServiceMessageBuilder.testSuiteStarted(module), module);
    return super.processServiceMessages(builder.toString(), myCurrentOutputType, myCurrentVisitor);
  }

  private boolean finishTestSuite() throws ParseException {
    if (myCurrentModule.isEmpty()) return true;

    boolean result = super.processServiceMessages(testSuiteFinished(myCurrentModule).toString(), myCurrentOutputType, myCurrentVisitor);
    myCurrentModule = "";
    return result;
  }

  private static ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module, String test) {
    return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module + ":" + test);
  }

  private static ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module) {
    return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module);
  }
}