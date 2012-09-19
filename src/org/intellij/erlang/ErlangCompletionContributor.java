/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.source.tree.TreeUtil;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.parser.GeneratedParserUtilBase;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangFileImpl;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.StandardPatterns.instanceOf;

/**
 * @author ignatov
 */
public class ErlangCompletionContributor extends CompletionContributor {
  @Override
  public void beforeCompletion(@NotNull CompletionInitializationContext context) {
    PsiFile file = context.getFile();
    if (ErlangParserUtil.isApplicationConfigFileType(file)) return;
    PsiElement elementAt = file.findElementAt(context.getStartOffset());
    PsiElement parent = elementAt == null ? null : elementAt.getParent();
    ErlangExport export = PsiTreeUtil.getPrevSiblingOfType(parent, ErlangExport.class);
    ErlangRecordTuple recordTuple = PsiTreeUtil.getPrevSiblingOfType(parent, ErlangRecordTuple.class);
    if (parent instanceof ErlangExport || export != null
      || parent instanceof ErlangRecordTuple || recordTuple != null || parent instanceof ErlangRecordField) {
      context.setDummyIdentifier("a");
    }
  }

  public ErlangCompletionContributor() {
    extend(CompletionType.BASIC, psiElement().inFile(instanceOf(ErlangFileImpl.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
        // add completion for records on #<caret>
        PsiElement position = parameters.getPosition();
        PsiFile file = position.getContainingFile();
        if (ErlangParserUtil.isApplicationConfigFileType(file)) return;

        PsiElement parent = position.getParent().getParent();
        PsiElement originalPosition = parameters.getOriginalPosition();
        PsiElement originalParent = originalPosition != null ? originalPosition.getParent() : null;

        if (originalParent instanceof ErlangRecordExpression) {
          result.addAllElements(ErlangPsiImplUtil.getRecordLookupElements(file));
        }
        else if (originalParent instanceof ErlangExportFunctions) {
          result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, true, null));
        }
        else {
          ErlangColonQualifiedExpression colonQualified = PsiTreeUtil.getParentOfType(position, ErlangColonQualifiedExpression.class);
          if (colonQualified != null) {
            result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, false, colonQualified));
          }
          else if (originalParent instanceof ErlangRecordFields || parent instanceof ErlangRecordField || parent instanceof ErlangRecordFields) {
            List<ErlangTypedExpr> fields = ErlangPsiImplUtil.getRecordFields(parent);
            ASTNode prev = FormatterUtil.getPreviousNonWhitespaceSibling(originalPosition != null ? originalPosition.getNode() : null);
            boolean isDotQualified = StringUtil.equals(prev != null ? prev.getText() : "", ".");
            final SingleCharInsertHandler insertHandler = isDotQualified ? null : new SingleCharInsertHandler('=');

            result.addAllElements(ContainerUtil.map(fields, new Function<ErlangTypedExpr, LookupElement>() {
              @Override
              public LookupElement fun(ErlangTypedExpr a) {
                return LookupElementBuilder.create(a.getName())
                  .setIcon(ErlangIcons.FIELD)
                  .setInsertHandler(insertHandler);
              }
            }));
            return;
          }
          else if (PsiTreeUtil.getParentOfType(position, ErlangExport.class) == null) {
            for (String keyword : suggestKeywords(position)) {
              result.addElement(LookupElementBuilder.create(keyword).setBold());
            }
            if (PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class) != null) {
              suggestModules(result, position);
            }
          }
          if (colonQualified == null && parent instanceof ErlangExpression && ErlangPsiImplUtil.inFunction(position)) {
            result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, false, null));
          }
        }
      }
    });
  }

  private static void suggestModules(CompletionResultSet result, PsiElement position) {
    Project project = position.getProject();
    Collection<VirtualFile> files = FilenameIndex.getAllFilesByExt(project, "erl", GlobalSearchScope.projectScope(project));

    List<VirtualFile> standardModules = ContainerUtil.filter(FilenameIndex.getAllFilesByExt(project, "erl", GlobalSearchScope.allScope(project)),
      new Condition<VirtualFile>() {
        @Override
        public boolean value(VirtualFile virtualFile) {
          String canonicalPath = virtualFile.getCanonicalPath();
          canonicalPath = FileUtil.toSystemIndependentName(canonicalPath != null ? canonicalPath : "");
          String kernelRegExp = ".*/lib/kernel[\\-\\d\\.]+/src/.*\\.erl";
          String stdlibRegExp = ".*/lib/stdlib[\\-\\d\\.]+/src/.*\\.erl";
          return canonicalPath.matches(kernelRegExp) || canonicalPath.matches(stdlibRegExp);
        }
      });// todo: module with libs scope

    //noinspection unchecked
    for (VirtualFile file : ContainerUtil.concat(files, standardModules)) {
      if (file.getFileType() == ErlangFileType.INSTANCE) {
        result.addElement(LookupElementBuilder.create(file.getNameWithoutExtension()).
          setIcon(ErlangIcons.MODULE).
          setInsertHandler(new SingleCharInsertHandler(':')));
      }
    }
  }

  private static Collection<String> suggestKeywords(PsiElement position) {
    TextRange posRange = position.getTextRange();
    ErlangFile posFile = (ErlangFile) position.getContainingFile();
    final TextRange range = new TextRange(0, posRange.getStartOffset());
    final String text = range.isEmpty() ? CompletionInitializationContext.DUMMY_IDENTIFIER : range.substring(posFile.getText());

    PsiFile file = PsiFileFactory.getInstance(posFile.getProject()).createFileFromText("a.erl", ErlangLanguage.INSTANCE, text, true, false);
    int completionOffset = posRange.getStartOffset() - range.getStartOffset();
    GeneratedParserUtilBase.CompletionState state = new GeneratedParserUtilBase.CompletionState(completionOffset) {
      @Override
      public String convertItem(Object o) {
        if (o instanceof IElementType && ErlangLexer.KEYWORDS.contains((IElementType) o)) return o.toString();
        return o instanceof String ? (String) o : null;
      }
    };
    file.putUserData(GeneratedParserUtilBase.COMPLETION_STATE_KEY, state);
    TreeUtil.ensureParsed(file.getNode());
    return state.items;
  }
}
