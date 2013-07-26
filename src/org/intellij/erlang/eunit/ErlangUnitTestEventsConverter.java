package org.intellij.erlang.eunit;

import com.intellij.execution.testframework.TestConsoleProperties;
import com.intellij.execution.testframework.sm.runner.OutputToGeneralTestEventsConverter;
import com.intellij.openapi.util.Key;
import jetbrains.buildServer.messages.serviceMessages.ServiceMessageVisitor;
import org.jetbrains.annotations.NotNull;

import java.text.ParseException;

/**
 * @author savenko
 */
public class ErlangUnitTestEventsConverter extends OutputToGeneralTestEventsConverter {
  public ErlangUnitTestEventsConverter(@NotNull String testFrameworkName, @NotNull TestConsoleProperties consoleProperties) {
    super(testFrameworkName, consoleProperties);
  }

  // This is done to be able to test this function (super.processServiceMessages has protected modifier)
  @Override
  public boolean processServiceMessages(String text, Key outputType, ServiceMessageVisitor visitor) throws ParseException {
    return super.processServiceMessages(text, outputType, visitor);
  }
}
