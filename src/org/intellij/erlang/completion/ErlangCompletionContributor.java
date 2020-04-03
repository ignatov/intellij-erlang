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

package org.intellij.erlang.completion;

import com.intellij.application.options.CodeStyle;
import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.ObjectUtils;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.JBIterable;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.index.ErlangApplicationIndex;
import org.intellij.erlang.index.ErlangAtomIndex;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.intellij.erlang.rebar.util.RebarConfigUtil;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.stubs.index.ErlangBehaviourModuleIndex;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.*;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.StandardPatterns.instanceOf;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangCompletionContributor extends CompletionContributor {
  private static final int ATOM_PRIORITY               = 50;
  public static final int TYPE_PRIORITY               = 10;
  public static final int MODULE_FUNCTIONS_PRIORITY   = -4;
  public static final int BIF_PRIORITY                = -5;
  public static final int EXTERNAL_FUNCTIONS_PRIORITY = -7;
  public static final int KEYWORD_PRIORITY            = -10;
  private static final int MODULE_PRIORITY             = -15;

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
    PsiElement previousByOffset = elementAt != null ? PsiTreeUtil.prevVisibleLeaf(elementAt) : startOffset > 0 ? file.findElementAt(startOffset - 1) : null;
    ErlangBehaviour behaviour = PsiTreeUtil.getParentOfType(elementAt, ErlangBehaviour.class);

    ErlangCompositeElement typeParent = PsiTreeUtil.getParentOfType(elementAt, ErlangTypeSig.class, ErlangTypedRecordFields.class, ErlangTypeDefinition.class);
    if (parent instanceof ErlangExport || PsiTreeUtil.getParentOfType(parent , ErlangExportFunctions.class, false) != null
      || parent instanceof ErlangImportDirective || parent instanceof ErlangImportFunctions
      || exportType != null || export != null || prevIsRadix(elementAt)
      || is(previousByOffset, ErlangTypes.ERL_RADIX)
      || (previousByOffset != null && previousByOffset.getParent() instanceof ErlangRecordField
      || parent instanceof ErlangRecordTuple || recordTuple != null || parent instanceof ErlangRecordField) && !is(previousByOffset, ErlangTypes.ERL_OP_EQ)
      || typeParent != null || isRecordFunctionCallCompletion(previousByOffset) || behaviour != null) {
      context.setDummyIdentifier("a");
    }
  }

  private static boolean isRecordFunctionCallCompletion(@Nullable PsiElement previousByOffset) {
    PsiElement prevSibling = previousByOffset != null ? previousByOffset.getPrevSibling() : null;
    if (prevSibling == null) return false;
    return is(previousByOffset, ErlangTypes.ERL_COMMA) && inIsRecord(0).accepts(prevSibling, new ProcessingContext());
  }

  public ErlangCompletionContributor() {
    extend(CompletionType.BASIC, psiElement().inFile(instanceOf(ErlangFile.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        PsiFile file = position.getContainingFile();
        if (ErlangParserUtil.isApplicationConfigFileType(file)) return;

        boolean inConsole = ErlangParserUtil.isConsole(file);
        PsiElement parent = position.getParent();
        if (parent instanceof ErlangAtom) {
          // This assignment makes the code below work the same way as it did before ErlangAtom stopped being a leaf element.
          parent = parent.getParent();
        }
        PsiElement grandPa = parent.getParent();
        PsiElement originalPosition = parameters.getOriginalPosition();
        PsiElement originalParent = originalPosition != null ? originalPosition.getParent() : null;

        if (originalParent instanceof ErlangIncludeString && originalPosition instanceof LeafPsiElement &&
            ErlangTypes.ERL_STRING == ((LeafPsiElement) originalPosition).getElementType()) {
          TextRange range = new TextRange(((LeafPsiElement) originalPosition).getStartOffset() + 1, parameters.getOffset());
          String includeText = range.getLength() >=0 ? range.substring(file.getText()) : "";
          if (grandPa instanceof ErlangInclude) {
            result.addAllElements(getModulePathLookupElements(file, includeText));
          }
          else if (grandPa instanceof ErlangIncludeLib) {
            result.addAllElements(getLibPathLookupElements(file, includeText));
          }
        }

        if (originalParent instanceof ErlangStringLiteral || originalPosition instanceof PsiComment) return;

        if (grandPa instanceof ErlangType) {
          result.addAllElements(getTypeLookupElements(file, true, false));
        }

        if (originalParent instanceof ErlangRecordExpression || prevIsRadix(originalPosition) || prevIsRadix(grandPa)) {
          result.addAllElements(getRecordLookupElements(file));
        }
        else if (grandPa instanceof ErlangExportFunction && file instanceof ErlangFile) {
          result.addAllElements(createFunctionLookupElements(((ErlangFile) file).getFunctions(), true));
        }
        else {
          ErlangColonQualifiedExpression colonQualified = PsiTreeUtil.getParentOfType(position, ErlangColonQualifiedExpression.class);
          if (colonQualified != null && (PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class) != null || inConsole)) {
            ErlangQAtom moduleAtom = getQAtom(colonQualified);

            result.addAllElements(getFunctionLookupElements(file, false, moduleAtom));

            // when completing at my_module:<caret>, ... the cursor is actually located at comma and not at the colon qualified expression
            ErlangColonQualifiedExpression originalColonQExpr = PsiTreeUtil.getParentOfType(originalPosition, ErlangColonQualifiedExpression.class);
            String moduleName = moduleAtom != null ? getName(moduleAtom) : null;
            String prefix = originalColonQExpr != null ?
              StringUtil.first(originalColonQExpr.getText(), parameters.getOffset() - originalColonQExpr.getTextOffset(), false) :
              moduleName != null ? moduleName + ":" : null;
            (StringUtil.isEmpty(prefix) ? result : result.withPrefixMatcher(result.getPrefixMatcher().cloneWithPrefix(prefix) ))
              .addAllElements(getAllExportedFunctionsWithModuleLookupElements(file.getProject(), false, moduleName));
          }
          else if (grandPa instanceof ErlangRecordField || grandPa instanceof ErlangRecordTuple) {
            Pair<List<ErlangTypedExpr>, List<ErlangQAtom>> recordFields = getRecordFields(grandPa);
            final boolean withoutEq = is(grandPa.getFirstChild(), ErlangTypes.ERL_DOT);
            result.addAllElements(ContainerUtil.map(recordFields.first, e -> createFieldLookupElement(e.getProject(), e.getName(), withoutEq)));
            result.addAllElements(ContainerUtil.map(recordFields.second, a -> createFieldLookupElement(a.getProject(), a.getText(), withoutEq)));
            return;
          }
          else if (grandPa instanceof ErlangMacros) {
            return;
          }
          else if (PsiTreeUtil.getParentOfType(position, ErlangExport.class) == null) {
            boolean inside = PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class, ErlangFunTypeSigs.class, ErlangTypeRef.class) != null;
            boolean insideImport = PsiTreeUtil.getParentOfType(position, ErlangImportDirective.class, ErlangImportFunctions.class) instanceof ErlangImportDirective;
            boolean insideBehaviour = PsiTreeUtil.getParentOfType(position, ErlangBehaviour.class) != null;
            if (inside || inConsole && !isDot(position) || insideImport) {
              boolean withColon = !insideImport && null == PsiTreeUtil.getParentOfType(position, ErlangFunctionCallExpression.class, false);
              suggestModules(result, position, withColon);
            }
            else if (insideBehaviour) {
              suggestBehaviours(result, position);
            }
          }
          if (colonQualified == null
            && grandPa instanceof ErlangExpression
            && (inFunction(position) || inConsole || PsiTreeUtil.getParentOfType(position, ErlangTypedRecordFields.class) != null)) {
            result.addAllElements(getFunctionLookupElements(file, false, null));
            result.addAllElements(getAllExportedFunctionsWithModuleLookupElements(file.getProject(), false, null));
          }

          int invocationCount = parameters.getInvocationCount();
          boolean moduleScope = invocationCount > 0 && invocationCount % 2 == 0;
          boolean moduleWithDeps = invocationCount > 0 && invocationCount % 3 == 0;
          
          if (moduleScope || moduleWithDeps) {
            Project project = file.getProject();

            Module module = ModuleUtilCore.findModuleForPsiElement(position);
            GlobalSearchScope scope = module != null && moduleScope ? GlobalSearchScope.moduleScope(module) : ProjectScope.getProjectScope(project);

            Collection<String> names = ErlangAtomIndex.getNames(project, scope);
            for (String name : names) {
              result.addElement(PrioritizedLookupElement.withPriority(
                LookupElementBuilder.create(name).withLookupString(name.toLowerCase()).withIcon(ErlangIcons.ATOM), ATOM_PRIORITY));
            }
          }

          String shortcut = CompletionUtil.getActionShortcut(IdeActions.ACTION_CODE_COMPLETION);
          if (invocationCount == 1 && new Random().nextBoolean()) {
            result.addLookupAdvertisement("Press " + shortcut + " to activate atom completion from application scope");
          }
          if (moduleScope) {
            result.addLookupAdvertisement("Press " + shortcut + " to activate atom completion from project scope");
          }
        }

        // fun foo/n
        if (!(parent instanceof ErlangRecordExpression) && grandPa instanceof ErlangFunctionWithArityVariables) {
          result.addAllElements(ErlangPsiImplUtil.getFunctionLookupElements(file, true, null));
        }
      }

      private boolean isDot(@NotNull PsiElement position) {
        PsiElement dot = PsiTreeUtil.prevVisibleLeaf(position);
        return dot != null && dot.getNode().getElementType() == ErlangTypes.ERL_DOT;
      }
    });
    extend(CompletionType.SMART, psiElement().inside(true, psiElement(ErlangArgumentList.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, @NotNull ProcessingContext context, @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        Set<ErlangExpressionType> expectedTypes = ErlangCompletionUtil.expectedArgumentTypes(position);
        if (expectedTypes.isEmpty()) return;

        List<LookupElement> functionLookupElements = getFunctionLookupElements(position.getContainingFile(), false, null);
        for (LookupElement lookupElement : functionLookupElements) {
          ErlangFunction function = ObjectUtils.tryCast(lookupElement.getPsiElement(), ErlangFunction.class);
          ErlangExpressionType type = function != null ? ErlangExpressionType.calculateFunctionType(function) : null;
          if (type != null && ErlangCompletionUtil.containsType(expectedTypes, type)) {
            result.addElement(lookupElement);
          }
        }
      }
    });
  }

  private static LookupElement createFieldLookupElement(@NotNull Project project, @NotNull String text, boolean withoutEq) {
    String maybeQuotedText = ErlangPsiImplUtil.needQuotation(text)
                             ? "'" + text + "'"
                             : text;
    return LookupElementBuilder.create(maybeQuotedText)
                               .withIcon(ErlangIcons.FIELD)
                               .withInsertHandler(withoutEq ? null : equalsInsertHandler(project));
  }

  @NotNull
  private static SingleCharInsertHandler equalsInsertHandler(@NotNull Project project) {
    return new SingleCharInsertHandler('=', CodeStyle.getSettings(project).getCustomSettings(ErlangCodeStyleSettings.class).SPACE_AROUND_EQ_IN_RECORDS);
  }

  @NotNull
  private static List<LookupElement> getLibPathLookupElements(@NotNull PsiFile file, @NotNull final String includeText) {
    if (FileUtil.isAbsolute(includeText)) return Collections.emptyList();

    final VirtualFile virtualFile = file.getOriginalFile().getVirtualFile();
    List<LookupElement> result = new ArrayList<>();
    List<String> split = StringUtil.split(includeText, "/");
    if (split.isEmpty()) {
      split = Collections.singletonList("");
    }

    String appName = split.get(0);
    String pathSeparator = includeText.endsWith("/") ? "/" : "";
    String libRelativePath = split.size() > 1 ? StringUtil.join(split.subList(1, split.size()), "/") + pathSeparator : "";
    boolean completingAppName = split.size() == 1 && !includeText.endsWith("/");
    List<VirtualFile> appDirs = getApplicationDirectories(file.getProject(), appName, !completingAppName);
    List<VirtualFile> matchingFiles = new ArrayList<>();

    for (VirtualFile appRoot : appDirs) {
      final String appFullName = appRoot != null ? appRoot.getName() : null;
      String appShortName = appFullName != null ? getAppShortName(appFullName) : null;
      if (appRoot == null) continue;
      if (completingAppName) {
        result.add(getDefaultPathLookupElementBuilder(includeText, appRoot, appShortName)
          .withPresentableText(appShortName + "/")
          .withTypeText("in " + appFullName, true));
        continue;
      }
      addMatchingFiles(appRoot, libRelativePath, matchingFiles);
      result.addAll(ContainerUtil.mapNotNull(matchingFiles, (Function<VirtualFile, LookupElement>) f -> f.equals(virtualFile) ? null : getDefaultPathLookupElementBuilder(includeText, f, null).withTypeText("in " + appFullName, true)));
      matchingFiles.clear();
    }
    result.addAll(getModulePathLookupElements(file, includeText));
    return result;
  }

  @NotNull
  private static List<VirtualFile> getApplicationDirectories(@NotNull Project project, @NotNull final String appName, boolean nameIsComplete) {
    GlobalSearchScope searchScope = GlobalSearchScope.allScope(project);
    if (nameIsComplete) {
      return ContainerUtil.createMaybeSingletonList(ErlangApplicationIndex.getApplicationDirectoryByName(appName, searchScope));
    }
    return ContainerUtil.filter(ErlangApplicationIndex.getAllApplicationDirectories(project, searchScope), virtualFile -> virtualFile != null && virtualFile.getName().startsWith(appName));
  }

  @NotNull
  private static String getAppShortName(@NotNull String appFullName) {
    int dashIdx = appFullName.indexOf('-');
    return dashIdx != -1 ? appFullName.substring(0, dashIdx) : appFullName;
  }

  @NotNull
  private static List<LookupElement> getModulePathLookupElements(@NotNull PsiFile file, @NotNull String includeText) {
    VirtualFile includeOwner = file.getOriginalFile().getVirtualFile();
    VirtualFile parentFile = includeOwner != null ? includeOwner.getParent() : null;
    List<LookupElement> result = new ArrayList<>();
    if (FileUtil.isAbsolute(includeText)) return result;
    //search in this module's directory
    result.addAll(getModulePathLookupElements(parentFile, includeOwner, includeText));
    //search in include directories
    Module module = ModuleUtilCore.findModuleForPsiElement(file);
    for (VirtualFile includeDir : ErlangIncludeDirectoryUtil.getIncludeDirectories(module)) {
      result.addAll(getModulePathLookupElements(includeDir, includeOwner, includeText));
    }
    if (ErlangSystemUtil.isSmallIde()) {
      VirtualFile otpAppRoot = getContainingOtpAppRoot(file.getProject(), includeOwner);
      VirtualFile otpIncludeDirectory = otpAppRoot != null ? otpAppRoot.findChild("include") : null;
      result.addAll(getModulePathLookupElements(otpIncludeDirectory, includeOwner, includeText));
      ErlangFile rebarConfigPsi = RebarConfigUtil.getRebarConfig(file.getProject(), otpAppRoot);
      if (rebarConfigPsi != null && otpAppRoot != null) {
        for (String relativeIncludePath : ContainerUtil.reverse(RebarConfigUtil.getIncludePaths(rebarConfigPsi))) {
          VirtualFile includePath = VfsUtilCore.findRelativeFile(relativeIncludePath, otpAppRoot);
          result.addAll(getModulePathLookupElements(includePath, includeOwner, includeText));
        }
      }
    }
    return result;
  }

  @NotNull
  private static List<LookupElement> getModulePathLookupElements(@Nullable VirtualFile includeDir, @Nullable final VirtualFile includeOwner, @NotNull final String includeText) {
    if (includeDir == null || !includeDir.isDirectory()) return ContainerUtil.emptyList();
    List<VirtualFile> matchingFiles = new ArrayList<>();
    addMatchingFiles(includeDir, includeText, matchingFiles);
    return ContainerUtil.mapNotNull(matchingFiles, f -> f.equals(includeOwner) ? null : getDefaultPathLookupElementBuilder(includeText, f, null));
  }

  private static LookupElementBuilder getDefaultPathLookupElementBuilder(@NotNull String includeText, @NotNull VirtualFile lookedUpFile, @Nullable String appName) {
    String slash = lookedUpFile.isDirectory() ? "/" : "";
    Icon icon = lookedUpFile.isDirectory() ? ErlangIcons.MODULE : lookedUpFile.getFileType().getIcon();
    return LookupElementBuilder.create(getCompletedString(includeText, lookedUpFile, appName))
                               .withPresentableText(lookedUpFile.getName() + slash)
                               .withIcon(icon)
                               .withInsertHandler(new RunCompletionInsertHandler());
  }

  @NotNull
  private static String getCompletedString(@NotNull String beforeCompletion, @NotNull VirtualFile lookedUpFile, @Nullable String appName) {
    String prefixPath = beforeCompletion.substring(0, beforeCompletion.lastIndexOf('/') + 1);
    String completion = appName == null ? lookedUpFile.getName() : appName;
    String pathSeparator = appName != null || lookedUpFile.isDirectory() ? "/" : "";
    return prefixPath + completion + pathSeparator;
  }

  private static void addMatchingFiles(VirtualFile searchRoot, @NotNull String includeText, @NotNull List<VirtualFile> result) {
    String[] split = includeText.split("/");
    if (split.length == 0) return;

    int joinEndIndex = includeText.endsWith("/") ? split.length : split.length - 1;
    final String childPrefix = joinEndIndex == split.length ? "" : split[split.length - 1];
    VirtualFile directory = VfsUtilCore.findRelativeFile(StringUtil.join(split, 0, joinEndIndex, "/"), searchRoot);
    VirtualFile[] children = directory != null ? directory.getChildren() : null;
    if (children == null) return;

    JBIterable
      .of(children)
      .filter(new Condition<VirtualFile>() {
        @Override
        public boolean value(VirtualFile child) {
          return child.getName().startsWith(childPrefix) && (child.isDirectory() || canBeIncluded(child));
        }

        private boolean canBeIncluded(@NotNull VirtualFile file) {
          FileType type = file.getFileType();
          return type == ErlangFileType.HEADER ||
                 type == ErlangFileType.MODULE;
        }
      })
      .addAllTo(result);
  }

  private static boolean prevIsRadix(@Nullable PsiElement element) {
    if (element == null) return false;
    ASTNode prev = FormatterUtil.getPreviousNonWhitespaceSibling(element.getNode());
    return prev != null && prev.getElementType() == ErlangTypes.ERL_RADIX;
  }

  private static void suggestModules(@NotNull CompletionResultSet result, @NotNull PsiElement position, boolean withColon) {
    Project project = position.getProject();

    Collection<String> names = ErlangModuleIndex.getNames(project);
    for (String name : names) {
      result.addElement(
        PrioritizedLookupElement.withPriority(
          LookupElementBuilder.create(name)
            .withIcon(ErlangIcons.MODULE)
            .withInsertHandler(new QuoteInsertHandler.ModuleInsertHandler(name, withColon)),
          MODULE_PRIORITY));
    }
  }

  private static void suggestBehaviours(@NotNull CompletionResultSet result, @NotNull PsiElement position) {
    Project project = position.getProject();
    Collection<ErlangModule> modules = ErlangBehaviourModuleIndex.getModules(project,
                                                                             GlobalSearchScope.allScope(project));
    for (ErlangModule module : modules) {
      QuoteInsertHandler.ModuleInsertHandler handler =
        new QuoteInsertHandler.ModuleInsertHandler(module.getName(), false);
      result.addElement(
        PrioritizedLookupElement.withPriority(
          LookupElementBuilder.create(module)
                              .withIcon(ErlangIcons.MODULE)
                              .withInsertHandler(handler),
          MODULE_PRIORITY));
    }
  }

  private static class RunCompletionInsertHandler implements InsertHandler<LookupElement> {
    @Override
    public void handleInsert(@NotNull final InsertionContext context, @NotNull LookupElement item) {
      if (item.getLookupString().endsWith("/"))
        context.setLaterRunnable(() -> new CodeCompletionHandlerBase(CompletionType.BASIC).invokeCompletion(context.getProject(), context.getEditor()));
    }
  }
}
