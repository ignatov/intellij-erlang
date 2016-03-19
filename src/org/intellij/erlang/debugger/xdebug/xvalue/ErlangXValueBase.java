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
import com.intellij.xdebugger.frame.*;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.intellij.erlang.icons.ErlangIcons;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

class ErlangXValueBase<T extends OtpErlangObject> extends XValue {
  private final T myValue;
  private final int myChildrenCount;
  private int myNextChildIdxToCompute;

  protected ErlangXValueBase(T value) {
    this(value, 0);
  }

  protected ErlangXValueBase(T value, int childrenCount) {
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
      node.setPresentation(getIcon(), presentation, hasChildren());
    }
    else {
      String repr = getStringRepr();
      if (repr.length() > XValueNode.MAX_VALUE_LENGTH) {
        node.setFullValueEvaluator(new ImmediateFullValueEvaluator(repr));
        repr = repr.substring(0, XValueNode.MAX_VALUE_LENGTH - 3) + "...";
      }
      node.setPresentation(getIcon(), getType(), repr, hasChildren());
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

  protected Icon getIcon() {
    return ErlangIcons.DEBUGGER_VALUE;
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

  private static void addNamedChild(XValueChildrenList childrenList, XValue child, String name) {
    childrenList.add(name, child);
  }
}

class ErlangPrimitiveXValueBase<T extends OtpErlangObject> extends ErlangXValueBase<T> {
  public ErlangPrimitiveXValueBase(T value) {
    super(value);
  }

  @Override
  protected Icon getIcon() {
    return ErlangIcons.DEBUGGER_PRIMITIVE_VALUE;
  }
}

class ErlangArrayXValueBase<T extends OtpErlangObject> extends ErlangXValueBase<T> {
  protected ErlangArrayXValueBase(T value, int childrenCount) {
    super(value, childrenCount);
  }

  @Override
  protected Icon getIcon() {
    return ErlangIcons.DEBUGGER_ARRAY;
  }
}