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
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class QuoteInsertHandler extends SingleCharInsertHandler {
  public static final String QUOTA = "'";
  private final String myName;

  public QuoteInsertHandler(@NotNull String name) {
    super(':');
    myName = name;
  }

  @Override
  public void handleInsert(@NotNull InsertionContext context, LookupElement item) {
    process(myName, null, context);
    if (needColon()) {
      AutoPopupController.getInstance(context.getProject()).autoPopupMemberLookup(context.getEditor(), null);
      super.handleInsert(context, item);
    }
  }

  public static void process(@NotNull String name, @Nullable String moduleName, @NotNull InsertionContext context) {
    if (moduleName != null && ErlangPsiImplUtil.needQuotation(moduleName)) {
      int startOffset = context.getStartOffset();
      insertQuotesIfAbsent(context, startOffset, startOffset + moduleName.length());
    }
    if (ErlangPsiImplUtil.needQuotation(name)) {
      int tailOffset = context.getTailOffset();
      int startOffset = moduleName == null ? context.getStartOffset() : tailOffset - name.length();
      tailOffset += insertQuotesIfAbsent(context, startOffset, tailOffset);
      context.getEditor().getCaretModel().moveToOffset(tailOffset);
      context.setTailOffset(tailOffset);
    }
  }

  private static int insertQuotesIfAbsent(@NotNull InsertionContext context, int startOffset, int tailOffset) {
    int insertedQuotesCount = 0;
    Document document = context.getEditor().getDocument();
    CharSequence fileText = document.getCharsSequence();

    if (!alreadyHasQuote(fileText, startOffset - 1)) {
      context.commitDocument();
      document.insertString(startOffset, QUOTA);
      insertedQuotesCount++;
      tailOffset++;
    }
    if (!alreadyHasQuote(fileText, tailOffset)) {
      context.commitDocument();
      document.insertString(tailOffset, QUOTA);
      insertedQuotesCount++;
    }
    return insertedQuotesCount;
  }

  private static boolean alreadyHasQuote(@NotNull CharSequence sequence, int position) {
    if (sequence.length() <= position) return false;
    return QUOTA.equals(String.valueOf(sequence.charAt(position)));
  }

  protected boolean needColon() {
    return false;
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
