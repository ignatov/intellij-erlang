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

package org.intellij.erlang.debugger.xdebug.xvalue;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.*;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

class ErlangXValueBase<T extends OtpErlangObject> extends XValue {
  private final T myValue;
  private final int myChildrenCount;
  private int myNextChildIdxToCompute;

  public ErlangXValueBase(T value) {
    this(value, 0);
  }

  public ErlangXValueBase(T value, int childrenCount) {
    myValue = value;
    myChildrenCount = childrenCount;
  }

  protected T getValue() {
    return myValue;
  }

  @Override
  public void computeChildren(@NotNull XCompositeNode node) {
    int nextToLastChildIdx = Math.min(myNextChildIdxToCompute + XCompositeNode.MAX_CHILDREN_TO_SHOW, myChildrenCount);
    XValueChildrenList children = new XValueChildrenList(nextToLastChildIdx - myNextChildIdxToCompute);
    for (int i = myNextChildIdxToCompute; i < nextToLastChildIdx; i++) {
      computeChild(children, i);
    }
    myNextChildIdxToCompute = nextToLastChildIdx;
    boolean computedAllChildren = myNextChildIdxToCompute == myChildrenCount;
    if (!computedAllChildren) {
      node.tooManyChildren(myChildrenCount - myNextChildIdxToCompute);
    }
    node.addChildren(children, computedAllChildren);
  }

  @Override
  public final void computePresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
    XValuePresentation presentation = getPresentation(node, place);
    if (presentation != null) {
      node.setPresentation(AllIcons.Debugger.Value, presentation, hasChildren());
    }
    else {
      String repr = getStringRepr();
      if (repr.length() > XValueNode.MAX_VALUE_LENGTH) {
        node.setFullValueEvaluator(new ImmediateFullValueEvaluator(repr));
        repr = repr.substring(0, XValueNode.MAX_VALUE_LENGTH - 3) + "...";
      }
      node.setPresentation(AllIcons.Debugger.Value, getType(), repr, hasChildren());
    }
  }

  @Override
  public boolean canNavigateToSource() {
    return false;
  }

  protected void computeChild(XValueChildrenList children, int childIdx) {
  }

  @Nullable
  protected XValuePresentation getPresentation(@NotNull XValueNode node, @NotNull XValuePlace place) {
    return null;
  }

  @Nullable
  protected String getType() {
    return null;
  }

  @NotNull
  protected String getStringRepr() {
    return myValue.toString();
  }

  private boolean hasChildren() {
    return myChildrenCount != 0;
  }

  protected static void addIndexedChild(XValueChildrenList childrenList, long numericChild, int childIdx) {
    addIndexedChild(childrenList, new OtpErlangLong(numericChild), childIdx);
  }

  protected static void addIndexedChild(XValueChildrenList childrenList, OtpErlangObject child, int childIdx) {
    addIndexedChild(childrenList, ErlangXValueFactory.create(child), childIdx);
  }

  protected static void addIndexedChild(XValueChildrenList childrenList, XValue child, int childIdx) {
    addNamedChild(childrenList, child, "[" + (childIdx + 1) + "]");
  }

  protected static void addNamedChild(XValueChildrenList childrenList, long numericChild, String name) {
    addNamedChild(childrenList, new OtpErlangLong(numericChild), name);
  }

  protected static void addNamedChild(XValueChildrenList childrenList, String atomicChild, String name) {
    addNamedChild(childrenList, new OtpErlangAtom(atomicChild), name);
  }

  protected static void addNamedChild(XValueChildrenList childrenList, OtpErlangObject child, String name) {
    addNamedChild(childrenList, ErlangXValueFactory.create(child), name);
  }

  protected static void addNamedChild(XValueChildrenList childrenList, XValue child, String name) {
    childrenList.add(name, child);
  }
}
