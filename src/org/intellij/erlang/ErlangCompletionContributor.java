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

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.impl.source.tree.TreeUtil;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.PathUtil;
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

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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
    if (parent instanceof ErlangExport || parent instanceof ErlangExportFunctions
      || parent instanceof ErlangImportDirective || parent instanceof ErlangImportFunctions
      || exportType != null || export != null || prevIsRadix(elementAt)
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

        boolean inConsole = ErlangParserUtil.isConsole(file);
        PsiElement parent = position.getParent().getParent();
        PsiElement originalPosition = parameters.getOriginalPosition();
        PsiElement originalParent = originalPosition != null ? originalPosition.getParent() : null;

        if (originalParent instanceof ErlangIncludeString && originalPosition instanceof LeafPsiElement &&
            ErlangTypes.ERL_STRING == ((LeafPsiElement) originalPosition).getElementType()) {
          String includeText = new TextRange(((LeafPsiElement) originalPosition).getStartOffset() + 1, parameters.getOffset()).substring(file.getText());
          if (parent instanceof ErlangInclude) {
            result.addAllElements(getModulePathLookupElements(file, includeText));
          }
          else if (parent instanceof ErlangIncludeLib) {
            result.addAllElements(getLibPathLookupElements(file, includeText));
          }
        }

        if (originalParent instanceof ErlangStringLiteral || originalPosition instanceof PsiComment) return;

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
          if (colonQualified != null && (PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class) != null || inConsole)) {
            ErlangQAtom qAtom = ErlangPsiImplUtil.getQAtom(colonQualified);
            result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, false, qAtom));
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
            boolean inside = PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class, ErlangFunTypeSigs.class, ErlangTypeRef.class) != null;
            if ((inside || inConsole) && moduleCompletion) {
              suggestModules(result, position, true);
            }
            else {
              //noinspection unchecked
              if (PsiTreeUtil.getParentOfType(position, ErlangImportDirective.class, ErlangImportFunctions.class) instanceof ErlangImportDirective) {
                suggestModules(result, position, false);
              }
              else {
                String shortcut = getActionShortcut(IdeActions.ACTION_CODE_COMPLETION);
                CompletionService.getCompletionService().setAdvertisementText(shortcut + " to activate module name completion");
              }
            }
          }
          if (colonQualified == null && parent instanceof ErlangExpression && ErlangPsiImplUtil.inFunction(position)) {
            result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, false, null));
          }
        }
      }
    });
  }

  private static List<LookupElement> getLibPathLookupElements(PsiFile file, String includeText) {
    if (FileUtil.isAbsolute(includeText)) return Collections.emptyList();

    List<LookupElement> result = new ArrayList<LookupElement>();
    String[] split = includeText.split("/");

    if (split.length != 0) {
      String appName = split[0];
      String slash = includeText.endsWith("/") ? "/" : "";
      String libRelativePath = split.length > 1 ? StringUtil.join(split, 1, split.length, "/") + slash: "";
      List<VirtualFile> appDirs = split.length == 1 && libRelativePath.isEmpty() ?
        ErlangApplicationIndex.getAllApplicationDirectories(file.getProject(), GlobalSearchScope.allScope(file.getProject())) :
        ContainerUtil.createMaybeSingletonList(ErlangApplicationIndex.getApplicationDirectoryByName(appName, GlobalSearchScope.allScope(file.getProject())));
      List<VirtualFile> matchingFiles = new ArrayList<VirtualFile>();

      for (final VirtualFile appRoot : appDirs) {
        final String appFullName = appRoot != null ? appRoot.getName() : null;
        final String appShortName = appFullName != null ? getAppShortName(appFullName) : null;

        if (appRoot == null) continue;

        if (split.length == 1 && !includeText.endsWith("/")) {
          result.add(getDefaultPathLookupElementBuilder(appRoot, appRoot, appShortName)
            .withPresentableText(appShortName + "/")
            .withTypeText("in " + appFullName, true));
          continue;
        }

        addMatchingFiles(appRoot, libRelativePath, matchingFiles);
        result.addAll(ContainerUtil.map(matchingFiles, new Function<VirtualFile, LookupElement>() {
          @Override
          public LookupElement fun(VirtualFile f) {
            return getDefaultPathLookupElementBuilder(f, appRoot, appShortName).withTypeText("in " + appFullName, true);
          }
        }));
        matchingFiles.clear();
      }
    }

    return result;
  }

  private static String getAppShortName(String appFullName) {
    int dashIdx = appFullName.indexOf('-');
    return dashIdx != -1 ? appFullName.substring(0, dashIdx) : appFullName;
  }

  private static List<LookupElement> getModulePathLookupElements(PsiFile file, String includeText) {
    VirtualFile virtualFile = file.getOriginalFile().getVirtualFile();
    final VirtualFile parentFile = virtualFile != null ? virtualFile.getParent() : null;
    List<LookupElement> result = new ArrayList<LookupElement>();

    if (FileUtil.isAbsolute(includeText)) return result;

    //search in this module's directory
    if (parentFile != null) {
      List<VirtualFile> relativeToParent = new ArrayList<VirtualFile>();
      addMatchingFiles(parentFile, includeText, relativeToParent);
      result.addAll(ContainerUtil.map(relativeToParent, new Function<VirtualFile, LookupElement>() {
        @Override
        public LookupElement fun(VirtualFile virtualFile) {
          return getDefaultPathLookupElementBuilder(virtualFile, parentFile, null);
        }
      }));
    }

    //search in include directories
    List<VirtualFile> relativeToIncludeDirs = new ArrayList<VirtualFile>();
    for (VirtualFile moduleRoot : ProjectRootManager.getInstance(file.getProject()).getContentRootsFromAllModules()) {
      final VirtualFile includeDir = VfsUtil.findRelativeFile("include/", moduleRoot);
      if (includeDir != null && includeDir.isDirectory()) {
        addMatchingFiles(includeDir, includeText, relativeToIncludeDirs);
        result.addAll(ContainerUtil.map(relativeToIncludeDirs, new Function<VirtualFile, LookupElement>() {
          @Override
          public LookupElement fun(VirtualFile virtualFile) {
            return getDefaultPathLookupElementBuilder(virtualFile, includeDir, null);
          }
        }));
      }
      relativeToIncludeDirs.clear();
    }

    return result;
  }

  private static LookupElementBuilder getDefaultPathLookupElementBuilder(VirtualFile lookedUpFile, VirtualFile lookUpRoot, @Nullable String appName) {
    String slash = lookedUpFile.isDirectory() ? "/" : "";
    Icon icon = lookedUpFile.isDirectory() ? ErlangIcons.MODULE : ErlangFileType.getIconForFile(lookedUpFile.getName());
    String relativePath = VfsUtilCore.getRelativePath(lookedUpFile, lookUpRoot, '/') + slash;
    if (appName != null)
      relativePath = appName + (relativePath.startsWith("/") ? "" : "/") + relativePath;

    return LookupElementBuilder.create(relativePath)
                               .withPresentableText(lookedUpFile.getName() + slash)
                               .withIcon(icon)
                               .withInsertHandler(new RunCompletionInsertHandler());
  }

  private static void addMatchingFiles(VirtualFile searchRoot, String includeText, List<VirtualFile> result) {
    String[] split = includeText.split("/");

    if (split.length != 0) {
      int joinEndIndex = includeText.endsWith("/") ? split.length : split.length - 1;
      String childPrefix = joinEndIndex == split.length ? "" : split[split.length - 1];
      String directoryPath = PathUtil.getLocalPath(searchRoot) + "/" + StringUtil.join(split, 0, joinEndIndex, "/");
      VirtualFile directory = LocalFileSystem.getInstance().findFileByPath(directoryPath);
      VirtualFile[] children = directory != null ? directory.getChildren() : null;

      if (children == null) return;

      for (VirtualFile child : children) {
        ErlangFileType childType = ErlangFileType.getFileType(child.getName());
        if (child.getName().startsWith(childPrefix) &&
            (child.isDirectory() || childType == ErlangFileType.HEADER || childType == ErlangFileType.MODULE)) {
          result.add(child);
        }
      }
    }
  }

  private static boolean prevIsRadix(@Nullable PsiElement psiElement) {
    PsiElement prevSibling = psiElement != null ? psiElement.getPrevSibling() : null;
    ASTNode prevSiblingNode = prevSibling != null ? prevSibling.getNode() : null;
    return (prevSiblingNode != null ? prevSiblingNode.getElementType() : null) == ErlangTypes.ERL_RADIX;
  }

  private static void suggestModules(CompletionResultSet result, PsiElement position, boolean withColon) {
    Project project = position.getProject();

    Collection<String> names = ErlangModuleIndex.getNames(project);
    for (String name : names) {
      result.addElement(
        PrioritizedLookupElement.withPriority(
          LookupElementBuilder.create(name)
            .withIcon(ErlangIcons.MODULE)
            .withInsertHandler(withColon ? new SingleCharInsertHandler(':') : null),
          MODULE_PRIORITY));
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

  private static class RunCompletionInsertHandler implements InsertHandler<LookupElement> {
    @Override
    public void handleInsert(final InsertionContext context, LookupElement item) {
      if (item.getLookupString().endsWith("/"))
        context.setLaterRunnable(new Runnable() {
          @Override
          public void run() {
            new CodeCompletionHandlerBase(CompletionType.BASIC).invokeCompletion(context.getProject(), context.getEditor());
          }
        });
    }
  }
}
