/*
 * Copyright 2012-2014 Sergey Ignatov
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
package org.intellij.erlang.jps;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.Function;
import com.intellij.util.SmartList;
import org.jetbrains.jps.incremental.MessageHandler;
import org.jetbrains.jps.incremental.messages.BuildMessage;

import java.util.List;

import static org.junit.Assert.assertTrue;

public class BuildResult implements MessageHandler {
  private final List<BuildMessage> myErrorMessages;
  private final List<BuildMessage> myWarnMessages;
  private final List<BuildMessage> myInfoMessages;

  public BuildResult() {
    myErrorMessages = new SmartList<>();
    myWarnMessages = new SmartList<>();
    myInfoMessages = new SmartList<>();
  }

  @Override
  public void processMessage(BuildMessage msg) {
    if (msg.getKind() == BuildMessage.Kind.ERROR) {
      myErrorMessages.add(msg);
    }
    else if (msg.getKind() == BuildMessage.Kind.WARNING) {
      myWarnMessages.add(msg);
    }
    else {
      myInfoMessages.add(msg);
    }
  }

  private boolean isSuccessful() {
    return myErrorMessages.isEmpty();
  }

  public void assertSuccessful() {
    Function<BuildMessage, String> toStringFunction = Object::toString;
    assertTrue("Build failed. \nErrors:\n" + StringUtil.join(myErrorMessages, toStringFunction, "\n") +
               "\nInfo messages:\n" + StringUtil.join(myInfoMessages, toStringFunction, "\n"), isSuccessful());
  }
}
