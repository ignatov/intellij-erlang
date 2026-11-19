/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.debugger.xdebug.xvalue;

import com.ericsson.otp.erlang.*;
import com.intellij.xdebugger.frame.XFullValueEvaluator;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ErlangXValueTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  // #1309: the Variables view, a watch and a value hint all call computePresentation, a watch even on the debugger
  // server thread. A value that OtpErlangObject.toString() cannot print must be marked, not throw StackOverflowError.
  private static final int DEPTH = 1_000_000;

  public void testTooDeeplyNestedTupleIsMarked() {
    OtpErlangObject value = new OtpErlangAtom("nil");
    for (int i = 0; i < DEPTH; i++) {
      value = new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangLong(1), value});
    }
    assertMarkedAsTooDeep(value);
  }

  public void testTooDeeplyNestedListIsMarked() {
    OtpErlangObject value = new OtpErlangList();
    for (int i = 0; i < DEPTH; i++) {
      value = new OtpErlangList(value);
    }
    assertMarkedAsTooDeep(value);
  }

  public void testTooDeeplyNestedMapIsMarked() {
    OtpErlangObject value = new OtpErlangAtom("nil");
    for (int i = 0; i < DEPTH; i++) {
      value = new OtpErlangMap(new OtpErlangObject[]{new OtpErlangLong(1)}, new OtpErlangObject[]{value});
    }
    assertMarkedAsTooDeep(value);
  }

  private static void assertMarkedAsTooDeep(OtpErlangObject value) {
    RecordingNode node = new RecordingNode();
    ErlangXValueFactory.create(value).computePresentation(node, XValuePlace.TREE);
    assertEquals("<nested too deeply to show>", node.myValue);
    assertTrue("the value can still be expanded", node.myHasChildren);
  }

  private static class RecordingNode implements XValueNode {
    private String myValue;
    private boolean myHasChildren;

    @Override
    public void setPresentation(@Nullable Icon icon, @Nullable String type, @NotNull String value, boolean hasChildren) {
      myValue = value;
      myHasChildren = hasChildren;
    }

    @Override
    public void setPresentation(@Nullable Icon icon, @NotNull XValuePresentation presentation, boolean hasChildren) {
      fail("unexpected custom presentation");
    }

    @Override
    public void setFullValueEvaluator(@NotNull XFullValueEvaluator fullValueEvaluator) {
    }
  }
}
