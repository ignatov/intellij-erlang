/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang;

import com.intellij.codeInsight.completion.BasicInsertHandler;
import com.intellij.codeInsight.completion.InsertionContext;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;

/**
 * @author ignatov
 */
public class SingleCharInsertHandler extends BasicInsertHandler<LookupElement> {
  private char myChar;

  public SingleCharInsertHandler(char aChar) {
    myChar = aChar;
  }

  @Override
  public void handleInsert(InsertionContext context, LookupElement item) {
    if (context.getCompletionChar() != myChar) {
      final Editor editor = context.getEditor();
      final Document document = editor.getDocument();
      context.commitDocument();
      int tailOffset = context.getTailOffset();
      document.insertString(tailOffset, String.valueOf(myChar));
      editor.getCaretModel().moveToOffset(tailOffset + 1);
    }
  }
}