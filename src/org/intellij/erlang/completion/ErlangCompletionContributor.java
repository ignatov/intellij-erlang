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
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
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
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.impl.source.tree.TreeUtil;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.text.CaseInsensitiveStringHashingStrategy;
import gnu.trove.THashSet;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.formatter.settings.ErlangCodeStyleSettings;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.index.ErlangApplicationIndex;
import org.intellij.erlang.index.ErlangAtomIndex;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.parser.ErlangLexer;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangFileImpl;
import org.intellij.erlang.psi.impl.ErlangVariableReferenceImpl;
import org.intellij.erlang.rebar.util.RebarConfigUtil;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.types.ErlangExpressionType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.*;

import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.patterns.StandardPatterns.instanceOf;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangCompletionContributor extends CompletionContributor {
  public static final int ATOM_PRIORITY               = 50;
  public static final int TYPE_PRIORITY               = 10;
  public static final int MODULE_FUNCTIONS_PRIORITY   = -4;
  public static final int BIF_PRIORITY                = -5;
  public static final int EXTERNAL_FUNCTIONS_PRIORITY = -7;
  public static final int KEYWORD_PRIORITY            = -10;
  public static final int MODULE_PRIORITY             = -15;

  public static final THashSet<String> KEYWORDS_WITH_PARENTHESIS = ContainerUtil.newTroveSet(CaseInsensitiveStringHashingStrategy.INSTANCE,
    "include", "include_lib", "module", "export", "export_type", "import", "define", "record", "behaviour"
  );

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
    //noinspection unchecked
    ErlangCompositeElement typeParent = PsiTreeUtil.getParentOfType(elementAt, ErlangTypeSig.class, ErlangTypedRecordFields.class, ErlangTypeDefinition.class);
    if (parent instanceof ErlangExport || parent instanceof ErlangExportFunctions
      || parent instanceof ErlangImportDirective || parent instanceof ErlangImportFunctions
      || exportType != null || export != null || prevIsRadix(elementAt)
      || is(previousByOffset, ErlangTypes.ERL_RADIX)
      || (previousByOffset != null && previousByOffset.getParent() instanceof ErlangRecordField
      || parent instanceof ErlangRecordTuple || recordTuple != null || parent instanceof ErlangRecordField) && !is(previousByOffset, ErlangTypes.ERL_OP_EQ)
      || typeParent != null || isRecordFunctionCallCompletion(previousByOffset)) {
      context.setDummyIdentifier("a");
    }
  }

  private static boolean isRecordFunctionCallCompletion(@Nullable PsiElement previousByOffset) {
    PsiElement prevSibling = previousByOffset != null ? previousByOffset.getPrevSibling() : null;
    if (prevSibling == null) return false;
    return is(previousByOffset, ErlangTypes.ERL_COMMA) && inIsRecord(0).accepts(prevSibling, new ProcessingContext());
  }

  public ErlangCompletionContributor() {
    extend(CompletionType.BASIC, psiElement().inFile(instanceOf(ErlangFileImpl.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
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
        else if (originalParent instanceof ErlangExportFunctions && file instanceof ErlangFile) {
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
            result.addAllElements(ContainerUtil.map(recordFields.first, new Function<ErlangTypedExpr, LookupElement>() {
              @Override
              public LookupElement fun(ErlangTypedExpr a) {
                return createFieldLookupElement(a.getProject(), a.getName(), withoutEq);
              }
            }));
            result.addAllElements(ContainerUtil.map(recordFields.second, new Function<ErlangQAtom, LookupElement>() {
              @Override
              public LookupElement fun(ErlangQAtom a) {
                return createFieldLookupElement(a.getProject(), a.getText(), withoutEq);
              }
            }));
            return;
          }
          else if (grandPa instanceof ErlangMacros) {
            return;
          }
          else if (PsiTreeUtil.getParentOfType(position, ErlangExport.class) == null) {
            for (String keyword : suggestKeywords(position)) {
              result.addElement(createKeywordLookupElement(keyword));
            }
            //noinspection unchecked
            boolean inside = PsiTreeUtil.getParentOfType(position, ErlangClauseBody.class, ErlangFunTypeSigs.class, ErlangTypeRef.class) != null;
            //noinspection unchecked
            boolean insideImport = PsiTreeUtil.getParentOfType(position, ErlangImportDirective.class, ErlangImportFunctions.class) instanceof ErlangImportDirective;
            if (inside || inConsole && !isDot(position) || insideImport) {
              boolean withColon = !insideImport && null == PsiTreeUtil.getParentOfType(position, ErlangFunctionCallExpression.class, false);
              suggestModules(result, position, withColon);
            }
          }
          if (colonQualified == null
            && grandPa instanceof ErlangExpression
            && (inFunction(position) || inConsole || PsiTreeUtil.getParentOfType(position, ErlangTypedRecordFields.class) != null)) {
            result.addAllElements(getFunctionLookupElements(file, false, null));

            // If we have some input, we can suggest modules and functions which match the input
            if (is(originalPosition, ErlangTypes.ERL_ATOM_NAME)) {
              result.addAllElements(getAllExportedFunctionsWithModuleLookupElements(file.getProject(), false, null));
            }
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

          String shortcut = getActionShortcut(IdeActions.ACTION_CODE_COMPLETION);
          if (invocationCount == 1 && new Random().nextBoolean()) {
            result.addLookupAdvertisement("Press " + shortcut + " to activate atom completion from application scope");
          }
          if (moduleScope) {
            result.addLookupAdvertisement("Press " + shortcut + " to activate atom completion from project scope");
          }
        }
      }

      private boolean isDot(@NotNull PsiElement position) {
        PsiElement dot = PsiTreeUtil.prevVisibleLeaf(position);
        return dot != null && dot.getNode().getElementType() == ErlangTypes.ERL_DOT;
      }
    });
    extend(CompletionType.SMART, psiElement().inside(true, psiElement(ErlangArgumentList.class)), new CompletionProvider<CompletionParameters>() {
      @Override
      protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
        PsiElement position = parameters.getPosition();
        ErlangQVar var = PsiTreeUtil.getParentOfType(position, ErlangQVar.class);
        PsiReference reference = var == null ? null : var.getReference();
        ErlangExpression expression = PsiTreeUtil.getParentOfType(position, ErlangExpression.class);
        if (expression == null || !(reference instanceof ErlangVariableReferenceImpl)) return;
        PsiElement argList = expression.getParent();
        if (argList == null || !(argList instanceof ErlangArgumentList)) return;

        int pos = 0;
        for (PsiElement ch : argList.getChildren()) {
          if (ch.equals(expression)) break;
          pos++;
        }

        Set<ErlangExpressionType> expectedTypes = getExpectedTypes(argList, pos);
        if (expectedTypes.isEmpty()) return;

        Collection<ErlangQVar> vars = new THashSet<ErlangQVar>();
        ErlangFunctionClause clause = PsiTreeUtil.getParentOfType(expression, ErlangFunctionClause.class);
        ((ErlangVariableReferenceImpl) reference).populateVariables(clause, vars);

        for (ErlangQVar v : vars) {
          if (inLeftPartOfAssignment(v, true)) {
            ErlangAssignmentExpression assign = PsiTreeUtil.getParentOfType(v, ErlangAssignmentExpression.class);
            ErlangExpression right = assign != null ? assign.getRight() : null;
            ErlangExpressionType varType = ErlangExpressionType.create(right);

            if (contains(expectedTypes, varType)) {
              result.addElement(LookupElementBuilder.create(v).withIcon(ErlangIcons.VARIABLE));
            }
          }
        }

        if (clause == null) return;
        List<LookupElement> functionLookupElements = getFunctionLookupElements(clause.getContainingFile(), false, null);
        for (LookupElement lookupElement : functionLookupElements) {
          PsiElement psiElement = lookupElement.getPsiElement();
          if (psiElement instanceof ErlangFunction) {
            ErlangExpressionType erlangExpressionType = ErlangExpressionType.calculateFunctionType((ErlangFunction) psiElement);
            if (contains(expectedTypes, erlangExpressionType)) result.addElement(lookupElement);
          }
        }
      }

      private boolean contains(Set<ErlangExpressionType> expectedTypes, ErlangExpressionType varType) {
        for (ErlangExpressionType type : expectedTypes) {
          if (type.accept(varType)) return true;
        }
        return false;
      }

      @NotNull
      private Set<ErlangExpressionType> getExpectedTypes(@NotNull PsiElement argList, int pos) {
        Set<ErlangExpressionType> expectedTypes = ContainerUtil.newHashSet();

        PsiElement call = argList.getParent();
        if (!(call instanceof ErlangFunctionCallExpression)) return expectedTypes;
        try {
          PsiReference callReference = call.getReference();
          //noinspection ConstantConditions
          ResolveResult[] resolveResults = ((PsiPolyVariantReference) callReference).multiResolve(true);

          for (ResolveResult r : resolveResults) {
            PsiElement element = r.getElement();
            if (element instanceof ErlangFunction) {
              ErlangSpecification spec = ((ErlangFunction) element).findSpecification();
              if (spec == null) return expectedTypes;
              ErlangFunTypeSigs signature = getSignature(spec);
              List<ErlangTypeSig> typeSigList = signature != null ? signature.getTypeSigList() : ContainerUtil.<ErlangTypeSig>emptyList();
              for (ErlangTypeSig sig : typeSigList) {
                ErlangFunTypeArguments arguments = sig.getFunType().getFunTypeArguments();
                ErlangType type = arguments.getTypeList().get(pos);
                if (type == null) continue;
                processType(type, expectedTypes);
              }
            }
          }
        } catch (Exception ignored) {
        }
        return expectedTypes;
      }

      private void processType(@NotNull ErlangType type, @NotNull Set<ErlangExpressionType> expectedTypes) {
        for (ErlangType childType : PsiTreeUtil.getChildrenOfTypeAsList(type, ErlangType.class)) {
          processType(childType, expectedTypes);
        }
        ErlangTypeRef typeRef = type.getTypeRef();
        String key = typeRef != null ? typeRef.getText() : type.getFirstChild().getText();
        ErlangExpressionType et = ErlangExpressionType.TYPE_MAP.get(key);
        ContainerUtil.addIfNotNull(expectedTypes, et);
      }
    });
  }

  private static LookupElement createFieldLookupElement(@NotNull Project project, @NotNull String text, boolean withoutEq) {
    ErlangCodeStyleSettings customSettings = CodeStyleSettingsManager.getSettings(project).getCustomSettings(ErlangCodeStyleSettings.class);
    boolean surroundWithSpaces = customSettings.SPACE_AROUND_EQ_IN_RECORDS;
    return LookupElementBuilder.create(text).withIcon(ErlangIcons.FIELD).withInsertHandler(withoutEq ? null : new SingleCharInsertHandler('=', surroundWithSpaces));
  }

  @NotNull
  private static LookupElement createKeywordLookupElement(@NotNull String keyword) {
    boolean needHandler = KEYWORDS_WITH_PARENTHESIS.contains(keyword);
    boolean needQuotas = "include".equalsIgnoreCase(keyword) || "include_lib".equalsIgnoreCase(keyword);
    boolean needBrackets = "export".equalsIgnoreCase(keyword) || "export_type".equalsIgnoreCase(keyword);
    return PrioritizedLookupElement.withPriority(LookupElementBuilder.create(keyword)
      .withInsertHandler(needHandler ? new ErlangKeywordInsertHandler(needQuotas, needBrackets) : null)
      .withTailText(needHandler ? "()" : null)
      .bold(), KEYWORD_PRIORITY);
  }

  private static List<LookupElement> getLibPathLookupElements(PsiFile file, final String includeText) {
    if (FileUtil.isAbsolute(includeText)) return Collections.emptyList();

    final VirtualFile virtualFile = file.getOriginalFile().getVirtualFile();
    List<LookupElement> result = new ArrayList<LookupElement>();
    List<String> split = StringUtil.split(includeText, "/");
    if (split.isEmpty()) {
      split = Collections.singletonList("");
    }

    String appName = split.get(0);
    String pathSeparator = includeText.endsWith("/") ? "/" : "";
    String libRelativePath = split.size() > 1 ? StringUtil.join(split.subList(1, split.size()), "/") + pathSeparator : "";
    boolean completingAppName = split.size() == 1 && !includeText.endsWith("/");
    List<VirtualFile> appDirs = getApplicationDirectories(file.getProject(), appName, !completingAppName);
    List<VirtualFile> matchingFiles = new ArrayList<VirtualFile>();

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
      result.addAll(ContainerUtil.mapNotNull(matchingFiles, new Function<VirtualFile, LookupElement>() {
        @Nullable
        @Override
        public LookupElement fun(VirtualFile f) {
          return f.equals(virtualFile) ? null : getDefaultPathLookupElementBuilder(includeText, f, null).withTypeText("in " + appFullName, true);
        }
      }));
      matchingFiles.clear();
    }
    result.addAll(getModulePathLookupElements(file, includeText));
    return result;
  }

  private static List<VirtualFile> getApplicationDirectories(Project project, final String appName, boolean nameIsComplete) {
    GlobalSearchScope searchScope = GlobalSearchScope.allScope(project);
    if (nameIsComplete) {
      return ContainerUtil.createMaybeSingletonList(ErlangApplicationIndex.getApplicationDirectoryByName(appName, searchScope));
    }
    return ContainerUtil.filter(ErlangApplicationIndex.getAllApplicationDirectories(project, searchScope), new Condition<VirtualFile>() {
      @Override
      public boolean value(VirtualFile virtualFile) {
        return virtualFile != null && virtualFile.getName().startsWith(appName);
      }
    });
  }

  private static String getAppShortName(String appFullName) {
    int dashIdx = appFullName.indexOf('-');
    return dashIdx != -1 ? appFullName.substring(0, dashIdx) : appFullName;
  }

  private static List<LookupElement> getModulePathLookupElements(PsiFile file, String includeText) {
    VirtualFile includeOwner = file.getOriginalFile().getVirtualFile();
    VirtualFile parentFile = includeOwner != null ? includeOwner.getParent() : null;
    List<LookupElement> result = new ArrayList<LookupElement>();
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

  private static List<LookupElement> getModulePathLookupElements(@Nullable VirtualFile includeDir, @Nullable final VirtualFile includeOwner, @NotNull final String includeText) {
    if (includeDir == null || !includeDir.isDirectory()) return ContainerUtil.emptyList();
    List<VirtualFile> matchingFiles = new ArrayList<VirtualFile>();
    addMatchingFiles(includeDir, includeText, matchingFiles);
    return ContainerUtil.mapNotNull(matchingFiles, new Function<VirtualFile, LookupElement>() {
      @Nullable
      @Override
      public LookupElement fun(VirtualFile f) {
        return f.equals(includeOwner) ? null : getDefaultPathLookupElementBuilder(includeText, f, null);
      }
    });
  }

  private static LookupElementBuilder getDefaultPathLookupElementBuilder(String includeText, VirtualFile lookedUpFile, @Nullable String appName) {
    String slash = lookedUpFile.isDirectory() ? "/" : "";
    Icon icon = lookedUpFile.isDirectory() ? ErlangIcons.MODULE : ErlangFileType.getIconForFile(lookedUpFile.getName());
    return LookupElementBuilder.create(getCompletedString(includeText, lookedUpFile, appName))
                               .withPresentableText(lookedUpFile.getName() + slash)
                               .withIcon(icon)
                               .withInsertHandler(new RunCompletionInsertHandler());
  }

  private static String getCompletedString(String beforeCompletion, VirtualFile lookedUpFile, @Nullable String appName) {
    String prefixPath = beforeCompletion.substring(0, beforeCompletion.lastIndexOf('/') + 1);
    String completion = appName == null ? lookedUpFile.getName() : appName;
    String pathSeparator = appName != null || lookedUpFile.isDirectory() ? "/" : "";
    return prefixPath + completion + pathSeparator;
  }

  private static void addMatchingFiles(VirtualFile searchRoot, String includeText, List<VirtualFile> result) {
    String[] split = includeText.split("/");

    if (split.length != 0) {
      int joinEndIndex = includeText.endsWith("/") ? split.length : split.length - 1;
      String childPrefix = joinEndIndex == split.length ? "" : split[split.length - 1];
      VirtualFile directory = VfsUtilCore.findRelativeFile(StringUtil.join(split, 0, joinEndIndex, "/"), searchRoot);
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

  private static boolean prevIsRadix(@Nullable PsiElement element) {
    if (element == null) return false;
    ASTNode prev = FormatterUtil.getPreviousNonWhitespaceSibling(element.getNode());
    return prev != null && prev.getElementType() == ErlangTypes.ERL_RADIX;
  }

  private static void suggestModules(CompletionResultSet result, PsiElement position, boolean withColon) {
    Project project = position.getProject();

    Collection<String> names = ErlangModuleIndex.getNames(project);
    for (String name : names) {
      result.addElement(
        PrioritizedLookupElement.withPriority(
          LookupElementBuilder.create(name)
            .withIcon(ErlangIcons.MODULE)
            .withInsertHandler(new ModuleInsertHandler(project, name, withColon)),
          MODULE_PRIORITY));
    }
  }

  private static Collection<String> suggestKeywords(PsiElement position) {
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
    public void handleInsert(InsertionContext context, LookupElement item) {
      super.handleInsert(context, item);
      Editor editor = context.getEditor();

      Document document = editor.getDocument();
      if (!document.getText().substring(context.getTailOffset()).startsWith(".")) {
        document.insertString(context.getTailOffset(), ".");
      }

      if (insertQuotas()) doInsert(editor, document, "\"", "\"");
      if (insertBrackets()) doInsert(editor, document, "[", "]");
    }

    private static void doInsert(Editor editor, Document document, String open, String closed) {
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
