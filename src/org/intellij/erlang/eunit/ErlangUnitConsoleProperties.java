package org.intellij.erlang.eunit;

import com.intellij.execution.Executor;
import com.intellij.execution.configurations.RuntimeConfiguration;
import com.intellij.execution.junit.RuntimeConfigurationProducer;
import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.SMCustomMessagesParsing;
import com.intellij.execution.testframework.sm.ServiceMessageBuilder;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.config.Storage;
import jetbrains.buildServer.messages.serviceMessages.ServiceMessageVisitor;
import org.jetbrains.annotations.NotNull;

import java.text.ParseException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.execution.testframework.sm.ServiceMessageBuilder.*;

/**
 * @author ignatov
 */
public class ErlangUnitConsoleProperties extends TestConsoleProperties implements SMCustomMessagesParsing {

  private RuntimeConfiguration myConfig;

  public ErlangUnitConsoleProperties(final ErlangUnitRunConfiguration config, final Executor executor) {
    super(new Storage.PropertiesComponentStorage("ErlangUnitSupport.", PropertiesComponent.getInstance()), config.getProject(), executor);
    myConfig = new RuntimeConfigurationProducer.DelegatingRuntimeConfiguration<ErlangUnitRunConfiguration>(config);
  }

  @Override
  public RuntimeConfiguration getConfiguration() {
    return myConfig;
  }

  @Override
  public OutputToGeneralTestEventsConverter createTestEventsConverter(@NotNull final String testFrameworkName, @NotNull final TestConsoleProperties consoleProperties) {
    return new OutputToGeneralTestEventsConverter(testFrameworkName, consoleProperties) {

      String myCurrentModule;
      String myCurrentTest;
      boolean myFailed;
      String myStdOut;

      @Override
      protected boolean processServiceMessages(String text, Key outputType, ServiceMessageVisitor visitor) throws ParseException {
        Matcher m;

        Pattern OK = Pattern.compile("  (\\w+): (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?ok");
        Pattern FAILED = Pattern.compile("  (\\w+): (\\w+)\\.\\.\\.(\\[\\d*\\.\\d+ s] )?\\*failed\\*");

        if (StringUtil.startsWith(text, "module")) {
          String module = StringUtil.unquoteString(StringUtil.getWordsIn(text).get(1));
          myCurrentModule = module;
          return super.processServiceMessages(ServiceMessageBuilder.testSuiteStarted(module).toString(), outputType, visitor);
        }
        else if (StringUtil.startsWith(text, "  [done")) {
          return super.processServiceMessages(testSuiteFinished(myCurrentModule).toString(), outputType, visitor);
        }
        else if ((m = OK.matcher(text)).find()) {
          String module = m.group(1);
          String test = m.group(2);
          ServiceMessageBuilder serviceMessageBuilder = setLocation(testStarted(test), module, test);
          return super.processServiceMessages(serviceMessageBuilder.toString(), outputType, visitor)
            && super.processServiceMessages(testFinished(test).toString(), outputType, visitor);
        }
        else if ((m = FAILED.matcher(text)).find()) {
          String module = m.group(1);
          String test = m.group(2);
          myFailed = true;
          myStdOut = "";
          myCurrentTest = test;
          ServiceMessageBuilder serviceMessageBuilder = setLocation(testStarted(test), module, test);
          return super.processServiceMessages(serviceMessageBuilder.toString(), outputType, visitor);
        }
        else if (myFailed) {
          if (!StringUtil.isEmptyOrSpaces(text)) {
            myStdOut += text;
          }
          else {
            myFailed = false;
            return super.processServiceMessages(testFailed(myCurrentTest).addAttribute("message", myStdOut).toString(), outputType, visitor)
              && super.processServiceMessages(testFinished(myCurrentTest).toString(), outputType, visitor);
          }
        }
        return true;
      }

      private ServiceMessageBuilder setLocation(ServiceMessageBuilder builder, String module, String test) {
        return builder.addAttribute("locationHint", ErlangUnitRunConfigurationType.PROTOCOL + "://" + module + ":" + test);
      }
    };
  }
}