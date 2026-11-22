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

package org.intellij.erlang.psi;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayDeque;
import java.util.Deque;

public class ErlangRecursiveVisitor extends ErlangVisitor {
  // the elements left to visit, top first; non-null while a walk is running, so that deep trees do not overflow the stack
  private Deque<PsiElement> myPending;

  @Override
  public void visitCompositeElement(@NotNull ErlangCompositeElement o) {
    visitChildren(o);
  }

  @Override
  public void visitFile(PsiFile file) {
    visitChildren(file);
  }

  private void visitChildren(@NotNull PsiElement o) {
    boolean outermost = myPending == null;
    if (outermost) myPending = new ArrayDeque<>();
    try {
      PsiElement[] children = o.getChildren();
      for (int i = children.length - 1; i >= 0; i--) {
        if (children[i] instanceof ErlangCompositeElement) {
          myPending.push(children[i]);
        }
      }
      if (outermost) {
        while (!myPending.isEmpty()) {
          myPending.pop().accept(this);
        }
      }
    }
    finally {
      if (outermost) myPending = null;
    }
  }
}
