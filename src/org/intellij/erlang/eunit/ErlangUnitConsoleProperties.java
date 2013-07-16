/*
 * Copyright 2012-2013 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.eunit;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RuntimeConfiguration;
import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.SMCustomMessagesParsing;
import com.intellij.execution.testframework.sm.ServiceMessageBuilder;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.execution.testframework.sm.runner.SMTRunnerConsoleProperties;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import jetbrains.buildServer.messages.serviceMessages.ServiceMessageVisitor;
import org.jetbrains.annotations.NotNull;

import java.text.ParseException;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.execution.testframework.sm.ServiceMessageBuilder.*;

/**
 * @author ignatov
 */
public class ErlangUnitConsoleProperties extends SMTRunnerConsoleProperties implements SMCustomMessagesParsing {

  private RuntimeConfiguration myConfig;

  public ErlangUnitConsoleProperties(final RuntimeConfiguration config, final Executor executor) {
    super(config, "Erlang", executor);
    myConfig = config;
  }

  @Override
  public RuntimeConfiguration getConfiguration() {
    return myConfig;
  }

  @Override
  public OutputToGeneralTestEventsConverter createTestEventsConverter(@NotNull final String testFrameworkName, @NotNull final TestConsoleProperties consoleProperties) {
    return new OutputToGeneralTestEventsConverter(testFrameworkName, consoleProperties) {

      String myCurrentModule = "";
      String myCurrentTest = "";
      String myCurrentFailedTest = "";
      Key myCurrentOutputType = null;
      ServiceMessageVisitor myCurrentVisitor = null;
      boolean myFailed = false;
      boolean mySingleFunctionModuleTestFailed = false;
      String myStdOut = "";
      Set<String> myFailedTests = new HashSet<String>();

      @Override
      protected boolean processServiceMessages(String text, Key outputType, ServiceMessageVisitor visitor) throws ParseException {
        Matcher m;

        Pattern OK = Pattern.compile("(?:  )?(\\w+):?\\d*: (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
        Pattern OK_ONE_TEST = Pattern.compile("(\\w+): (\\w+) .*\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
        Pattern FAILED = Pattern.compile("(?:  )?(\\w+):?\\d*: (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");
        Pattern FAILED_ONE_TEST = Pattern.compile("(\\w+): (\\w+).*\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");
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
          String test = m.group(2);
          if (!isTestingModule()) {
            String module = m.group(1);
            return startTestSuite(module) && startTest(test) && finishTest() && finishTestSuite();
          }
          return startTest(test) && finishTest();
        }
        else if ((m = FAILED.matcher(text)).find() || (m = FAILED_ONE_TEST.matcher(text)).find() || text.trim().equals("undefined")) {
          myFailed = true;
          myStdOut = "";
          if (text.trim().equals("undefined")) {
            myCurrentFailedTest = "undefined";
            return startTest("undefined");
          }
          else {
            String test = m.group(2);
            myCurrentFailedTest = test;
            if (!isTestingModule()) {
              String module = m.group(1);
              mySingleFunctionModuleTestFailed = true;
              return startTestSuite(module) && startTest(test);
            }
            return startTest(test);
          }
        }
        else if (text.startsWith("ERROR:") && !myFailedTests.contains(myCurrentFailedTest)) {
          myStdOut += text;
          boolean result = failTest() && finishTest(myCurrentFailedTest);
          if (mySingleFunctionModuleTestFailed) {
            mySingleFunctionModuleTestFailed = false;
            result &= finishTestSuite();
          }
          return result;
        }
        else if (myFailed) {
          if (StringUtil.isEmptyOrSpaces(text)) {
            myFailed = false;
            myFailedTests.add(myCurrentFailedTest);
            boolean result = failTest() && finishTest(myCurrentFailedTest);
            if (mySingleFunctionModuleTestFailed) {
              mySingleFunctionModuleTestFailed = false;
              result &= finishTestSuite();
            }
            return result;
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

      private boolean isTestingModule() {
        return !myCurrentModule.isEmpty();
      }

      private boolean startTest(String test) throws ParseException {
        myCurrentTest = test;
        ServiceMessageBuilder serviceMessageBuilder = setLocation(testStarted(test), myCurrentModule, test);
        return super.processServiceMessages(serviceMessageBuilder.toString(), myCurrentOutputType, myCurrentVisitor);
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
        boolean result = super.processServiceMessages(testSuiteFinished(myCurrentModule).toString(), myCurrentOutputType, myCurrentVisitor);
        myCurrentModule = "";
        return result;
      }

      private ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module, String test) {
        return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module + ":" + test);
      }
      
      private ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module) {
        return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module);
      }
    };
  }
}