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

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.completion.util.ParenthesesInsertHandler;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.impl.source.tree.TreeUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.text.CaseInsensitiveStringHashingStrategy;
import gnu.trove.THashSet;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.ErlangColonQualifiedExpression;
import org.intellij.erlang.psi.ErlangExport;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangStringLiteral;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.StandardPatterns.instanceOf;

public class ErlangKeywordsCompletionContributor extends CompletionContributor implements DumbAware {
  public static final THashSet<String> KEYWORDS_WITH_PARENTHESIS = ContainerUtil.newTroveSet(CaseInsensitiveStringHashingStrategy.INSTANCE,
    "include", "include_lib", "module", "export", "export_type", "import", "define", "record", "behaviour", "behavior"
  );

  public ErlangKeywordsCompletionContributor() {
    extend(CompletionType.BASIC, psiElement().inFile(instanceOf(ErlangFile.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters,
                                    ProcessingContext context,
                                    @NotNull CompletionResultSet result) {
        PsiFile file = parameters.getOriginalFile();
        if (ErlangParserUtil.isApplicationConfigFileType(file)) return;
        PsiElement position = parameters.getPosition();
        //noinspection unchecked
        if (PsiTreeUtil.getParentOfType(position, ErlangExport.class, ErlangColonQualifiedExpression.class, ErlangStringLiteral.class, PsiComment.class) != null) return;
        for (String keyword : suggestKeywords(position)) {
          result.addElement(createKeywordLookupElement(keyword));
        }
      }
    });
  }

  @NotNull
  static Collection<String> suggestKeywords(@NotNull PsiElement position) {
    TextRange posRange = position.getTextRange();
    ErlangFile posFile = (ErlangFile) position.getContainingFile();
    TextRange range = new TextRange(0, posRange.getStartOffset());
    String text = range.isEmpty() ? CompletionInitializationContext.DUMMY_IDENTIFIER : range.substring(posFile.getText());

    PsiFile file = PsiFileFactory.getInstance(posFile.getProject()).createFileFromText("a.erl", ErlangLanguage.INSTANCE, text, true, false);
    int completionOffset = posRange.getStartOffset() - range.getStartOffset();
    ErlangParserUtil.CompletionState state = new ErlangParserUtil.CompletionState(completionOffset) {
      @Override
      public String convertItem(Object o) {
        if (o instanceof IElementType && ErlangLexer.KEYWORDS.contains((IElementType) o)) return o.toString();
        return o instanceof String ? (String) o : null;
      }
    };
    //noinspection StaticFieldReferencedViaSubclass
    file.putUserData(ErlangParserUtil.COMPLETION_STATE_KEY, state);
    TreeUtil.ensureParsed(file.getNode());
    return state.items;
  }

  @NotNull
  static LookupElement createKeywordLookupElement(@NotNull String keyword) {
    boolean needHandler = KEYWORDS_WITH_PARENTHESIS.contains(keyword);
    boolean needQuotas = "include".equalsIgnoreCase(keyword) || "include_lib".equalsIgnoreCase(keyword);
    boolean needBrackets = "export".equalsIgnoreCase(keyword) || "export_type".equalsIgnoreCase(keyword);
    return PrioritizedLookupElement.withPriority(LookupElementBuilder.create(keyword)
      .withInsertHandler(needHandler ? new ErlangKeywordInsertHandler(needQuotas, needBrackets) : null)
      .withTailText(needHandler ? "()" : null)
      .bold(), ErlangCompletionContributor.KEYWORD_PRIORITY);
  }

  private static class ErlangKeywordInsertHandler extends ParenthesesInsertHandler<LookupElement> {
    private final boolean myNeedQuotas;
    private final boolean myInsertBrackets;

    public ErlangKeywordInsertHandler(boolean needQuotas, boolean insertBrackets) {
      myNeedQuotas = needQuotas;
      myInsertBrackets = insertBrackets;
    }

    @Override
    protected boolean placeCaretInsideParentheses(InsertionContext context, LookupElement item) {
      return true;
    }

    @Override
    public void handleInsert(@NotNull InsertionContext context, LookupElement item) {
      super.handleInsert(context, item);
      Editor editor = context.getEditor();

      Document document = editor.getDocument();
      if (!document.getText().substring(context.getTailOffset()).startsWith(".")) {
        document.insertString(context.getTailOffset(), ".");
      }

      if (insertQuotas()) doInsert(editor, document, "\"", "\"");
      if (insertBrackets()) doInsert(editor, document, "[", "]");
    }

    private static void doInsert(@NotNull Editor editor, @NotNull Document document, @NotNull String open, @NotNull String closed) {
      int offset = editor.getCaretModel().getOffset();
      document.insertString(offset, open);
      document.insertString(offset + 1, closed);
      editor.getCaretModel().moveToOffset(offset + 1);
    }

    private boolean insertBrackets() {
      return myInsertBrackets;
    }

    private boolean insertQuotas() {
      return myNeedQuotas;
    }
  }
}
