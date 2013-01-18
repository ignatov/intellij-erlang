package org.intellij.erlang.eunit;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RuntimeConfiguration;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
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

  private final boolean myEunit;
  private RuntimeConfiguration myConfig;

  public ErlangUnitConsoleProperties(final RuntimeConfiguration config, final Executor executor, boolean isEunit) {
    super(config, "Erlang", executor);
    myEunit = isEunit;
    myConfig = new RuntimeConfigurationProducer.DelegatingRuntimeConfiguration<RuntimeConfiguration>(config);
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
      boolean myFailed = false;
      String myStdOut = "";
      Set<String> myFailedTests = new HashSet<String>();

      @Override
      protected boolean processServiceMessages(String text, Key outputType, ServiceMessageVisitor visitor) throws ParseException {
        Matcher m;

        Pattern OK = Pattern.compile("  (\\w+):?\\d*: (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
        Pattern OK_ONE_TEST = Pattern.compile("(\\w+): (\\w+) .*\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
        Pattern FAILED = Pattern.compile("  (\\w+):?\\d*: (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");
        Pattern FAILED_ONE_TEST = Pattern.compile("(\\w+): (\\w+).*\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");

        if (StringUtil.startsWith(text, (myEunit ? "" : "  ") + "module")) {
          String module = StringUtil.unquoteString(StringUtil.getWordsIn(text).get(1));
          myCurrentModule = module;
          ServiceMessageBuilder builder = setLocation(ServiceMessageBuilder.testSuiteStarted(module), module);
          return super.processServiceMessages(builder.toString(), outputType, visitor);
        }
        else if (StringUtil.startsWith(text, "  [done")) {
          return super.processServiceMessages(testSuiteFinished(myCurrentModule).toString(), outputType, visitor);
        }
        else if ((m = OK.matcher(text)).find() || (m = OK_ONE_TEST.matcher(text)).find()) {
          String module = m.group(1);
          String test = m.group(2);
          ServiceMessageBuilder serviceMessageBuilder = setLocation(testStarted(test), module, test);
          return super.processServiceMessages(serviceMessageBuilder.toString(), outputType, visitor)
            && super.processServiceMessages(testFinished(test).toString(), outputType, visitor);
        }
        else if ((m = FAILED.matcher(text)).find() || (m = FAILED_ONE_TEST.matcher(text)).find() || text.trim().equals("undefined")) {
          boolean matches = FAILED.matcher(text).find() || FAILED_ONE_TEST.matcher(text).find();
          String module = matches ? m.group(1) : myCurrentModule;
          String test = matches ? m.group(2) : "undefined";
          myFailed = true;
          myStdOut = "";
          myCurrentTest = test;
          ServiceMessageBuilder serviceMessageBuilder = setLocation(testStarted(test), module, test);
          return super.processServiceMessages(serviceMessageBuilder.toString(), outputType, visitor);
        }
        else if (text.startsWith("ERROR:") && !myFailedTests.contains(myCurrentTest)) {
          myStdOut += text;
          return super.processServiceMessages(testFailed(myCurrentTest).addAttribute("message", myStdOut).toString(), outputType, visitor)
            && super.processServiceMessages(testFinished(myCurrentTest).toString(), outputType, visitor);
        }
        else if (myFailed) {
          if (!StringUtil.isEmptyOrSpaces(text)) {
            myStdOut += text;
          }
          else {
            myFailed = false;
            myFailedTests.add(myCurrentTest);
            return super.processServiceMessages(testFailed(myCurrentTest).addAttribute("message", myStdOut).toString(), outputType, visitor)
              && super.processServiceMessages(testFinished(myCurrentTest).toString(), outputType, visitor);
          }
        }
        else if (text.startsWith("=======================================================")){
          return super.processServiceMessages(testSuiteFinished(myCurrentModule).toString(), outputType, visitor);
        }
        myStdOut += text;
        return true;
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