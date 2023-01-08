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

package org.intellij.erlang.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangNamedElement;
import org.intellij.erlang.types.ErlType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public abstract class ErlangNamedElementImpl extends ErlangCompositeElementImpl implements ErlangCompositeElement, ErlangNamedElement {
  public ErlangNamedElementImpl(ASTNode node) {
    super(node);
  }

  @Override
  public PsiElement setName(@NonNls @NotNull String s) throws IncorrectOperationException {
    return this;
  }

  @Override
  public ErlType synthesizeType() {
    return ErlType.FLOAT_TYPE;
  }
}
