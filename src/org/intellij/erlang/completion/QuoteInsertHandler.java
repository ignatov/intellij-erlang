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

package org.intellij.erlang.completion;

import com.intellij.codeInsight.AutoPopupController;
import com.intellij.codeInsight.completion.InsertionContext;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.jetbrains.annotations.NotNull;

public class QuoteInsertHandler extends SingleCharInsertHandler {
  public static final String QUOTA = "'";
  private final String myName;

  public QuoteInsertHandler(@NotNull String name) {
    super(':');
    myName = name;
  }

  @Override
  public void handleInsert(@NotNull InsertionContext context, LookupElement item) {
    Project project = context.getProject();
    if (needQuotation(project, myName)) {
      Editor editor = context.getEditor();
      Document document = editor.getDocument();
      context.commitDocument();
      int tailOffset = context.getTailOffset();
      int startOffset = context.getStartOffset();

      document.insertString(startOffset, QUOTA);
      document.insertString(tailOffset + 1, QUOTA);
      editor.getCaretModel().moveToOffset(tailOffset + 2);
      context.setTailOffset(tailOffset + 2);
    }
    if (needColon()) {
      AutoPopupController.getInstance(project).autoPopupMemberLookup(context.getEditor(), null);
      super.handleInsert(context, item);
    }
  }

  protected boolean needColon() {
    return false;
  }

  private static boolean needQuotation(@NotNull Project project, @NotNull String name) {
    try {
      ErlangElementFactory.createQAtomFromText(project, name);
      return false;
    } catch (Exception ignored) {
    }
    return true;
  }
  
  public static class ModuleInsertHandler extends QuoteInsertHandler {
    private final boolean myWithColon;

    public ModuleInsertHandler(@NotNull String name, boolean withColon) {
      super(name);
      myWithColon = withColon;
    }

    @Override
    protected boolean needColon() {
      return myWithColon;
    }
  }
}
