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
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
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
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.List;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.StandardPatterns.instanceOf;

/**
 * @author ignatov
 */
public class ErlangCompletionContributor extends CompletionContributor {
  public static final int MODULE_PRIORITY = 10;
  public static final int KEYWORD_PRIORITY = -10;
  public static final int MODULE_FUNCTIONS_PRIORITY = -4;
  public static final int BIF_PRIORITY = -5;

  @Override
  public void beforeCompletion(@NotNull CompletionInitializationContext context) {
    PsiFile file = context.getFile();
    if (ErlangParserUtil.isApplicationConfigFileType(file)) return;
    int startOffset = context.getStartOffset();
    PsiElement elementAt = file.findElementAt(startOffset);
    PsiElement parent = elementAt == null ? null : elementAt.getParent();
    ErlangExport export = PsiTreeUtil.getPrevSiblingOfType(parent, ErlangExport.class);
    ErlangExportTypeAttribute exportType = PsiTreeUtil.getParentOfType(elementAt, ErlangExportTypeAttribute.class);
    ErlangRecordTuple recordTuple = PsiTreeUtil.getPrevSiblingOfType(parent, ErlangRecordTuple.class);
    PsiElement previousByOffset = startOffset > 0 ? file.findElementAt(startOffset - 1) : null;
    //noinspection unchecked
    ErlangCompositeElement typeParent = PsiTreeUtil.getParentOfType(elementAt, ErlangTypeSig.class, ErlangTypedRecordFields.class, ErlangTypeDefinition.class);
    if (parent instanceof ErlangExport || parent instanceof ErlangExportFunctions || exportType != null || export != null || prevIsRadix(elementAt)
      || (previousByOffset != null && previousByOffset.getNode().getElementType() == ErlangTypes.ERL_RADIX)
      || (previousByOffset != null && previousByOffset.getParent() instanceof ErlangRecordField)
      || parent instanceof ErlangRecordTuple || recordTuple != null || parent instanceof ErlangRecordField
      || typeParent != null) {
      context.setDummyIdentifier("a");
    }
  }

  public ErlangCompletionContributor() {
    extend(CompletionType.BASIC, psiElement().inFile(instanceOf(ErlangFileImpl.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        PsiFile file = position.getContainingFile();
        if (ErlangParserUtil.isApplicationConfigFileType(file)) return;

        PsiElement parent = position.getParent().getParent();
        PsiElement originalPosition = parameters.getOriginalPosition();
        PsiElement originalParent = originalPosition != null ? originalPosition.getParent() : null;

        if (parent instanceof ErlangType) {
          result.addAllElements(ErlangPsiImplUtil.getTypeLookupElements(file, true, false));
        }

        if (originalParent instanceof ErlangRecordExpression || prevIsRadix(originalPosition) || prevIsRadix(parent)) {
          result.addAllElements(ErlangPsiImplUtil.getRecordLookupElements(file));
        }
        else if (originalParent instanceof ErlangExportFunctions) {
          result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, true, null));
        }
        else {
          ErlangColonQualifiedExpression colonQualified = PsiTreeUtil.getParentOfType(position, ErlangColonQualifiedExpression.class);
          if (colonQualified != null && PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class) != null) {
            result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, false, colonQualified));
          }
          else if (originalParent instanceof ErlangRecordFields || parent instanceof ErlangRecordField || parent instanceof ErlangRecordFields) {
            Pair<List<ErlangTypedExpr>,List<ErlangQAtom>> recordFields = ErlangPsiImplUtil.getRecordFields(parent);
            result.addAllElements(ContainerUtil.map(recordFields.first, new Function<ErlangTypedExpr, LookupElement>() {
              @Override
              public LookupElement fun(ErlangTypedExpr a) {
                return LookupElementBuilder.create(a.getName()).withIcon(ErlangIcons.FIELD);
              }
            }));
            result.addAllElements(ContainerUtil.map(recordFields.second, new Function<ErlangQAtom, LookupElement>() {
              @Override
              public LookupElement fun(ErlangQAtom a) {
                return LookupElementBuilder.create(a.getText()).withIcon(ErlangIcons.FIELD);
              }
            }));
            return;
          }
          else if (parent instanceof ErlangMacros) {
            return;
          }
          else if (PsiTreeUtil.getParentOfType(position, ErlangExport.class) == null) {
            for (String keyword : suggestKeywords(position)) {
              result.addElement(PrioritizedLookupElement.withPriority(LookupElementBuilder.create(keyword).bold(), KEYWORD_PRIORITY));
            }
            int invocationCount = parameters.getInvocationCount();
            boolean moduleCompletion = invocationCount > 0 && invocationCount % 2 == 0;
            //noinspection unchecked
            if (PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class, ErlangFunTypeSigs.class, ErlangTypeRef.class) != null && moduleCompletion) {
              suggestModules(result, position);
            }
            else {
              String shortcut = getActionShortcut(IdeActions.ACTION_CODE_COMPLETION);
              CompletionService.getCompletionService().setAdvertisementText(shortcut + " to activate module name completion");
            }
          }
          if (colonQualified == null && parent instanceof ErlangExpression && ErlangPsiImplUtil.inFunction(position)) {
            result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, false, null));
          }
        }
      }
    });
  }

  private static boolean prevIsRadix(@Nullable PsiElement psiElement) {
    PsiElement prevSibling = psiElement != null ? psiElement.getPrevSibling() : null;
    ASTNode prevSiblingNode = prevSibling != null ? prevSibling.getNode() : null;
    return (prevSiblingNode != null ? prevSiblingNode.getElementType() : null) == ErlangTypes.ERL_RADIX;
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
      if (file.getFileType() == ErlangFileType.MODULE) {
        result.addElement(
          PrioritizedLookupElement.withPriority(
            LookupElementBuilder.create(file.getNameWithoutExtension())
              .withIcon(ErlangIcons.MODULE)
              .withInsertHandler(new SingleCharInsertHandler(':')),
            MODULE_PRIORITY));
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
