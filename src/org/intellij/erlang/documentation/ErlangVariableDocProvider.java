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

package org.intellij.erlang.documentation;

import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Renders empty text (this is to allow documentation popups on variables and other stuff).
 * The type is appended to this empty text in ErlangDocumentationProvider.
 *
 * @see ErlangDocumentationProvider
 */
final class ErlangEmptyDocProvider implements ElementDocProvider {
  ErlangEmptyDocProvider() {
  }

  @Nullable
  @Override
  public List<String> getExternalDocUrls() {
    return null;
  }

  @Override
  public String getDocText() {
    return "";
  }
}
