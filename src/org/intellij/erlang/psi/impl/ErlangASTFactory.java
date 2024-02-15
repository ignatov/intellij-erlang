/*
 * Copyright 2012-2015 Sergey Ignatov
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

import com.intellij.lang.DefaultASTFactoryImpl;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.PsiCommentImpl;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class ErlangASTFactory extends DefaultASTFactoryImpl {
  @Override
  public @NotNull LeafElement createComment(@NotNull IElementType type, @NotNull CharSequence text) {
    return new ErlangCommentImpl(type, text);
  }

  public static class ErlangCommentImpl extends PsiCommentImpl {
    public ErlangCommentImpl(IElementType type, CharSequence text) {
      super(type, text);
    }
  }
}
