/*
 * Copyright 2012-2023 Sergey Ignatov
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

package org.intellij.erlang.types;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.util.DocumentUtil;
import org.jetbrains.annotations.Nullable;

public class ErlTypeError implements ErlType {
  private final String description;
  private final @Nullable PsiElement element;

  public ErlTypeError(String description, @Nullable PsiElement element) {
    this.description = description;
    this.element = element;
  }

  @Override
  public Kind getKind() {
    return Kind.ERROR;
  }

  @Override
  public boolean isSubtypeOf(ErlType other) {
    return false;
  }

  @Override
  public String toDefinitionString() {
    return this.toReferenceString();
  }

  @Override
  public String toReferenceString() {
    if (this.element != null) {
      TextRange textRange = this.element.getTextRange();
      return "%% Type error: %s at %d".formatted(this.description, textRange.getStartOffset());
    } else {
      return "%% Type error: %s".formatted(this.description);
    }
  }

  @Override
  public String toString() {
    return this.toReferenceString();
  }
}
