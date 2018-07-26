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

package org.intellij.erlang.psi.impl;

import com.intellij.codeInsight.completion.BasicInsertHandler;
import com.intellij.codeInsight.completion.InsertHandler;
import com.intellij.codeInsight.completion.InsertionContext;
import com.intellij.codeInsight.completion.PrioritizedLookupElement;
import com.intellij.codeInsight.completion.util.ParenthesesInsertHandler;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.execution.console.LanguageConsoleImpl;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.*;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.patterns.PatternCondition;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.*;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.ResolveScopeManager;
import com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry;
import com.intellij.psi.impl.source.resolve.reference.impl.PsiMultiReference;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.stubs.NamedStubBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.*;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangParserDefinition;
import org.intellij.erlang.ErlangStringLiteralEscaper;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.completion.ErlangCompletionContributor;
import org.intellij.erlang.completion.QuoteInsertHandler;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.index.ErlangApplicationIndex;
import org.intellij.erlang.index.ErlangModuleIndex;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.rebar.util.RebarConfigUtil;
import org.intellij.erlang.roots.ErlangIncludeDirectoryUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.intellij.erlang.stubs.*;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ErlangPsiImplUtil {
  public static final Set<String> KNOWN_MACROS = ContainerUtil.set("MODULE", "MODULE_STRING", "FILE", "LINE", "MACHINE", "FUNCTION_NAME", "FUNCTION_ARITY");
  public static final Set<String> BUILT_IN_TYPES = ContainerUtil.set(
    "any", "atom", "boolean", "byte", "char", "float", "integer", "iolist", "list", "maybe_improper_list", "mfa",
    "module", "neg_integer", "no_return", "node", "non_neg_integer", "none", "nonempty_string", "number", "pid", "port",
    "pos_integer", "ref", "string", "term", "timeout"
  );
  public static final Key<LanguageConsoleImpl> ERLANG_CONSOLE = Key.create("ERLANG_CONSOLE");

  @NotNull
  private static Pattern ATOM_PATTERN = Pattern.compile("[a-z][a-zA-Z_@0-9]*");
  @NotNull
  private static Pattern QUOTED_ATOM_NAME = Pattern.compile("(\\\\\\^.|\\\\.|[^'])*"); //see https://github.com/rvirding/leex/blob/master/examples/erlang_scan.xrl

  private ErlangPsiImplUtil() {
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangQVar o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processor.execute(o, state);
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangQVar o) {
    return new ErlangVariableReferenceImpl(o, TextRange.from(0, o.getTextLength()));
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangQAtom o) { // todo: use multi reference
    PsiReference[] referencesFromProviders = ReferenceProvidersRegistry.getReferencesFromProviders(o);
    PsiReference atomReference = createAtomReference(o);
    PsiReference[] psiReferences = atomReference == null ? referencesFromProviders : ArrayUtil.append(referencesFromProviders, atomReference);
    if (psiReferences.length == 0) return null;
    return new PsiMultiReference(psiReferences, o);
  }

  @Nullable
  private static PsiReference createAtomReference(@NotNull final ErlangQAtom o) {
    if (!standaloneAtom(o)) return null;
    return new PsiPolyVariantReferenceBase<ErlangQAtom>(o, TextRange.create(0, o.getTextLength())) {
      @NotNull
      @Override
      public ResolveResult[] multiResolve(boolean b) {
        return new ResolveResult[]{};
      }

      @Override
      public boolean isReferenceTo(PsiElement element) {
        return element instanceof ErlangQAtom && standaloneAtom(o) && Comparing.equal(element.getText(), getElement().getText());
      }

      @Override
      public PsiElement handleElementRename(String newName) throws IncorrectOperationException {
        renameQAtom(o, newName);
        return getElement();
      }

      @NotNull
      @Override
      public Object[] getVariants() { // todo
        return ArrayUtil.EMPTY_OBJECT_ARRAY;
      }
    };
  }

  public static boolean standaloneAtom(@NotNull ErlangQAtom o) {
    if (o.getAtom() == null) return false;
    PsiElement parent = o.getParent();
    return parent instanceof ErlangMaxExpression || parent instanceof ErlangAtomWithArityExpression ||
      (parent instanceof ErlangTypeRef || parent instanceof ErlangBitType) && !FormatterUtil.isFollowedBy(parent.getNode(), ErlangTypes.ERL_PAR_LEFT);
  }

  @NotNull
  public static Pair<List<ErlangTypedExpr>, List<ErlangQAtom>> getRecordFields(PsiElement element) {
    List<ErlangTypedExpr> result = ContainerUtil.newArrayListWithCapacity(0);
    List<ErlangQAtom> atoms = ContainerUtil.newArrayListWithCapacity(0);
    ErlangRecordExpression recordExpression = PsiTreeUtil.getParentOfType(element, ErlangRecordExpression.class);
    PsiReference reference = recordExpression != null ? recordExpression.getReferenceInternal() : null;
    PsiElement resolve = reference != null ? reference.resolve() : null;

    if (resolve == null && recordExpression != null) {
      ErlangMacros macros = recordExpression.getMacros();
      PsiReference macrosReference = macros != null ? macros.getReference() : null;
      PsiElement macroDefinition = macrosReference != null ? macrosReference.resolve() : null;
      if (macroDefinition instanceof ErlangMacrosDefinition) {
        ErlangMacrosBody body = ((ErlangMacrosDefinition) macroDefinition).getMacrosBody();
        final Ref<ErlangRecordRef> ref = Ref.create();
        if (body != null) {
          body.accept(new ErlangRecursiveVisitor() {
            @Override
            public void visitRecordRef(@NotNull ErlangRecordRef o) {
              ref.setIfNull(o);
            }
          });
        }

        if (!ref.isNull()) {
          PsiReference r = ref.get().getReference();
          PsiElement rr = r != null ? r.resolve() : null;
          if (rr instanceof ErlangRecordDefinition) {
            resolve = rr;
          }
        }
      }
    }

    if (resolve instanceof ErlangRecordDefinition) {
      ErlangTypedRecordFields typedRecordFields = ((ErlangRecordDefinition) resolve).getTypedRecordFields();
      if (typedRecordFields != null) {
        for (ErlangTypedExpr e : typedRecordFields.getTypedExprList()) {
          ErlangMacros macros = e.getQAtom().getMacros();
          if (macros == null) {
            result.add(e);
          }
          else {
            processRecordFields(macros, atoms);
          }
        }
        for (ErlangGenericFunctionCallExpression gc : typedRecordFields.getGenericFunctionCallExpressionList()) {
          ErlangQAtom qAtom = ContainerUtil.getFirstItem(gc.getQAtomList());
          ErlangMacros macros = qAtom == null ? null : qAtom.getMacros();
          if (macros != null) {
            processRecordFields(macros, atoms);
          }
        }
      }
    }

    return Pair.create(result, atoms);
  }

   // for #149: Nitrogen support
  private static void processRecordFields(@NotNull ErlangMacros macros, @NotNull List<ErlangQAtom> atoms) {
    PsiReference psiReference = macros.getReference();
    PsiElement macrosDefinition = psiReference != null ? psiReference.resolve() : null;
    if (macrosDefinition instanceof ErlangMacrosDefinition) {
      ErlangMacrosBody macrosBody = ((ErlangMacrosDefinition) macrosDefinition).getMacrosBody();
      List<ErlangExpression> expressionList = macrosBody != null ? macrosBody.getExpressionList() : ContainerUtil.emptyList();
      for (ErlangExpression ee : expressionList) {
        if (ee instanceof ErlangMaxExpression) {
          ErlangQAtom qAtom = ((ErlangMaxExpression) ee).getQAtom();
          ContainerUtil.addIfNotNull(atoms, qAtom);
        }
        else if (ee instanceof ErlangAssignmentExpression) {
          ErlangExpression left = ((ErlangAssignmentExpression) ee).getLeft();
          if (left instanceof ErlangMaxExpression) {
            ErlangQAtom qAtom = ((ErlangMaxExpression) left).getQAtom();
            ContainerUtil.addIfNotNull(atoms, qAtom);
          }
        }
        else if (ee instanceof ErlangFunctionCallExpression) {
          ErlangMacros m = ((ErlangFunctionCallExpression) ee).getQAtom().getMacros();
          if (m != null) {
            processRecordFields(m, atoms);
          }
        }
      }
    }
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangRecordField o) {
    return getRecordFieldReference(o.getFieldNameAtom());
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangFieldType o) {
    return getRecordFieldReference(o.getQAtom());
  }

  @Nullable
  private static PsiReference getRecordFieldReference(@Nullable ErlangQAtom atom) {
    if (atom == null) return null;
    return new ErlangQAtomBasedReferenceImpl(atom, getTextRangeForReference(atom), getNameIdentifier(atom).getText()) {
      @Override
      public PsiElement resolveInner() {
        Pair<List<ErlangTypedExpr>, List<ErlangQAtom>> recordFields = getRecordFields(myElement);
        for (ErlangTypedExpr field : recordFields.first) {
          if (field.getName().equals(myReferenceName)) return field;
        }
        for (ErlangQAtom qAtom : recordFields.second) {
          ErlangAtom aa = qAtom.getAtom();
          if (aa != null) {
            if (myReferenceName.equals(aa.getName())) return qAtom;
          }
        }
        return null;
      }

      @NotNull
      @Override
      public Object[] getVariants() {
        return ArrayUtil.EMPTY_OBJECT_ARRAY;
      }
    };
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangIncludeString o) {
    return o.getTextLength() < 2 ? null :
           new PsiReferenceBase<ErlangIncludeString>(o, TextRange.from(1, o.getTextLength() - 2)) {
             @Override
             public PsiElement resolve() {
               ErlangFile file = (ErlangFile) myElement.getContainingFile();
               PsiElement parent = myElement.getParent();
               List<ErlangFile> files = parent instanceof ErlangInclude ?
                                        getDirectlyIncludedFiles((ErlangInclude) parent, file) :
                                        getDirectlyIncludedFiles((ErlangIncludeLib) parent, file);
               return ContainerUtil.getFirstItem(files);
             }

             @Override
             public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException {
               if (!(element instanceof ErlangFile)) {
                 throw new IncorrectOperationException("Unexpected element");
               }

               String includeStringText = computeNewIncludeStringText((ErlangFile) element);
               if (includeStringText == null) {
                 throw new IncorrectOperationException("Failed to update include path");
               }

               myElement.getString().replace(
                 ErlangElementFactory.createIncludeString(element.getProject(), includeStringText).getString());
               return myElement;
             }

             @NotNull
             @Override
             public PsiElement handleElementRename(@NotNull String newName) throws IncorrectOperationException {
               PsiElement resolve = resolve();
               if (resolve instanceof ErlangFile) {
                 PsiElement st;
                 try {
                   String fileName = ((ErlangFile) resolve).getName();
                   String newIncludeString = StringUtil.unquoteString(myElement.getString().getText()).replace(fileName, newName);
                   st = ErlangElementFactory.createStringFromText(myElement.getProject(), newIncludeString);
                 }
                 catch (Exception e) {
                   st = null;
                 }

                 if (st != null) {
                   myElement.getString().replace(st);
                 }
               }
               return myElement;
             }

             @NotNull
             @Override
             public Object[] getVariants() {
               return ArrayUtil.EMPTY_OBJECT_ARRAY;
             }

             @Nullable
             private String computeNewIncludeStringText(@NotNull ErlangFile newHeader) {
               VirtualFile newHeaderVirtualFile = newHeader.getVirtualFile();
               File newHeaderIoFile = VfsUtilCore.virtualToIoFile(newHeaderVirtualFile);

               ErlangFile file = (ErlangFile) myElement.getContainingFile();
               VirtualFile virtualFile = file.getVirtualFile();
               File ioFile = VfsUtilCore.virtualToIoFile(virtualFile);

               String relativePath = FileUtilRt.getRelativePath(ioFile, newHeaderIoFile);

               Module module = ModuleUtilCore.findModuleForPsiElement(file);
               for (VirtualFile includeDir : ErlangIncludeDirectoryUtil.getIncludeDirectories(module)) {
                 if (!VfsUtilCore.isAncestor(includeDir, newHeaderVirtualFile, true)) continue;

                 File includeIoDir = VfsUtilCore.virtualToIoFile(includeDir);
                 String includeDirToHeaderPath = FileUtilRt.getRelativePath(includeIoDir, newHeaderIoFile);
                 if (includeDirToHeaderPath != null &&
                     (relativePath == null || relativePath.length() > includeDirToHeaderPath.length())) {
                   relativePath = includeDirToHeaderPath;
                 }
               }

               return PathUtil.toSystemIndependentName(relativePath);
             }
           };
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangFunctionCallExpression o) {
    PsiElement parent = o.getParent();
    ErlangModuleRef moduleReference = null;
    if (parent instanceof ErlangGlobalFunctionCallExpression) {
      moduleReference = ((ErlangGlobalFunctionCallExpression) parent).getModuleRef();
    }
    ErlangQAtom moduleAtom = moduleReference == null ? null : moduleReference.getQAtom();
    ErlangQAtom nameAtom = o.getQAtom();

    return new ErlangFunctionReferenceImpl(nameAtom, moduleAtom, o.getArgumentList().getExpressionList().size());
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangFunctionWithArity o) {
    ErlangQVar prevVar = PsiTreeUtil.getPrevSiblingOfType(o, ErlangQVar.class);
    if (prevVar != null) return null;
    ErlangModuleRef moduleReference = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    boolean isModule = isModule(moduleReference);
    if (moduleReference != null && moduleReference.getQAtom().getMacros() != null && !isModule) return null;
    ErlangQAtom moduleAtom = moduleReference == null ? null : moduleReference.getQAtom();

    ErlangQAtom nameAtom = o.getQAtom();
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl(nameAtom, isModule ? null : moduleAtom, getArity(arity));
  }

  private static boolean isModule(@Nullable ErlangModuleRef moduleReference) {
    if (moduleReference == null) return false;
    return moduleReference.getQAtom().getText().equals("?MODULE");
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangExportFunction o) {
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl(o.getQAtom(), null, getArity(arity));
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangImportFunction o) {
    ErlangImportDirective importDirective = PsiTreeUtil.getParentOfType(o, ErlangImportDirective.class);
    ErlangModuleRef moduleRef = importDirective != null ? importDirective.getModuleRef() : null;
    ErlangQAtom moduleRefQAtom = moduleRef != null ? moduleRef.getQAtom() : null;
    int arity = getArity(o);
    return new ErlangFunctionReferenceImpl(o.getQAtom(), moduleRefQAtom, arity);
  }

  @NotNull
  public  static PsiReference getReference(@NotNull ErlangCallbackFunction o) {
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl(o.getQAtom(), null, getArity(arity));
  }

  public static int getArity(@Nullable PsiElement arity) {
    return StringUtil.parseInt(arity == null ? "" : arity.getText(), -1);
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangMacros o) {
    return getReference(o.getMacrosName());
  }

  @Nullable
  public static PsiReference getReference(@Nullable ErlangMacrosName o) {
    return o != null ? new ErlangMacrosReferenceImpl(o) : null;
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangTypeRef o) {
    return getModuleReference(o, o.getQAtom());
  }

  @NotNull
  private static PsiReference getModuleReference(ErlangCompositeElement o, @NotNull ErlangQAtom atom) {
    ErlangModuleRef moduleRef = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    return new ErlangTypeReferenceImpl(atom, moduleRef);
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangExportType o) {
    return getModuleReference(o, o.getQAtom());
  }

  @SuppressWarnings("unchecked")
  public static boolean inDefinitionBeforeArgumentList(PsiElement psiElement) {
    return inArgumentDefinition(psiElement) && inArgumentList(psiElement) && PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class, ErlangArgumentList.class) instanceof ErlangArgumentDefinition;
  }

  public static boolean inArgumentDefinition(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentDefinition.class) != null;
  }

  @SuppressWarnings("unchecked")
  public static boolean inArgumentList(PsiElement psiElement) {
    ErlangArgumentList argList = PsiTreeUtil.getParentOfType(psiElement, ErlangArgumentList.class, true,
      ErlangFunctionCallExpression.class, ErlangFunClause.class, ErlangListComprehension.class);
    PsiElement parent = argList != null ? argList.getParent() : null;
    return parent instanceof ErlangFunctionCallExpression && ((ErlangFunctionCallExpression) parent).getQAtom().getMacros() == null;
  }

  public static boolean inFunctionTypeArgument(PsiElement psiElement) {
    ErlangType topType = PsiTreeUtil.getParentOfType(psiElement, ErlangType.class);
    return PsiTreeUtil.getParentOfType(topType, ErlangFunTypeArguments.class) != null;
  }

  public static boolean inDefine(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangMacrosDefinition.class) != null;
  }

  public static boolean inMacroCallArguments(@NotNull PsiElement psiElement) {
    PsiElement child = psiElement;
    ErlangFunctionCallExpression functionCall;
    while ((functionCall = PsiTreeUtil.getParentOfType(child, ErlangFunctionCallExpression.class, true)) != null) {
      boolean isMacroCall = functionCall.getQAtom().getMacros() != null;
      if (isMacroCall && PsiTreeUtil.isAncestor(functionCall.getArgumentList(), psiElement, true)) {
        return true;
      }
      child = functionCall;
    }
    return false;
  }

  public static boolean inCallback(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangCallbackSpec.class) != null;
  }

  public static boolean inRecordDefinition(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangRecordDefinition.class) != null;
  }

  public static boolean inAtomAttribute(PsiElement psiElement) {
    //noinspection unchecked
    return PsiTreeUtil.getParentOfType(psiElement, ErlangAtomAttribute.class, ErlangTypeDefinition.class) != null;
  }

  public static boolean inSpecification(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangSpecification.class) != null;
  }

  public static boolean inColonQualified(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangColonQualifiedExpression.class) != null;
  }

  public static boolean inLeftPartOfAssignment(@NotNull PsiElement psiElement) {
    return inLeftPartOfAssignment(psiElement, true);
  }

  public static boolean inLeftPartOfAssignment(@NotNull PsiElement psiElement, boolean strict) {
    ErlangAssignmentExpression assignment = PsiTreeUtil.getParentOfType(psiElement, ErlangAssignmentExpression.class, true);
    while (assignment != null) {
      if (PsiTreeUtil.isAncestor(assignment.getLeft(), psiElement, strict)) {
        return true;
      }
      assignment = PsiTreeUtil.getParentOfType(assignment, ErlangAssignmentExpression.class, true);
    }
    return false;
  }

  public static boolean isForceSkipped(@NotNull ErlangQVar o) {
    return o.getName().startsWith("_");
  }

  @NotNull
  public static List<LookupElement> getFunctionLookupElements(@NotNull PsiFile containingFile, boolean withArity, @Nullable ErlangQAtom moduleAtom) {
    if (containingFile instanceof ErlangFile && !ErlangParserUtil.isApplicationConfigFileType(containingFile)) {
      List<ErlangFunction> functions = ContainerUtil.newArrayList();
      List<LookupElement> lookupElements = ContainerUtil.newArrayList();

      ErlangSdkRelease release = ErlangSdkType.getRelease(containingFile);
      if (moduleAtom != null) {
        String moduleName = getName(moduleAtom);
        functions.addAll(getExternalFunctionForCompletion(containingFile.getProject(), moduleName));

        if (release == null || release.needBifCompletion(moduleName)) {
          addBifs(lookupElements, ErlangBifTable.getBifs(moduleName), withArity);
        }
        addBifs(lookupElements, ErlangBifTable.getBifs("", ErlangBifTable.MODULE_INFO), withArity);
      }
      else {
        ErlangFile erlangFile = (ErlangFile) containingFile;
        functions.addAll(erlangFile.getFunctions());
        functions.addAll(getExternalFunctionForCompletion(containingFile.getProject(), "erlang"));

        List<ErlangImportFunction> directlyImported = erlangFile.getImportedFunctions();
        List<ErlangImportFunction> importsFromIncludes = getImportsFromIncludes(erlangFile, true, "", 0);
        for (ErlangImportFunction importFunction : ContainerUtil.concat(directlyImported, importsFromIncludes)) {
          LookupElement element = createFunctionLookupElement(getName(importFunction), getArity(importFunction), withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY);
          lookupElements.add(element);
        }

        if (!withArity && (release == null || release.needBifCompletion("erlang"))) {
          addBifs(lookupElements, ErlangBifTable.getBifs("erlang"));
        }
        if (!withArity && (release == null || release.needBifCompletion(""))) {
          addBifs(lookupElements, ErlangBifTable.getBifs(""));
        }
      }

      functions.addAll(getErlangFunctionsFromIncludes((ErlangFile) containingFile, true, "", 0));
      lookupElements.addAll(createFunctionLookupElements(functions, withArity));
      return lookupElements;
    }
    return Collections.emptyList();
  }

  private static void addBifs(@NotNull List<LookupElement> lookupElements, @NotNull Collection<ErlangBifDescriptor> bifs) {
    for (ErlangBifDescriptor bif : bifs) {
      lookupElements.add(createFunctionLookupElement(bif.getName(), bif.getArity(), false, ErlangCompletionContributor.BIF_PRIORITY));
    }
  }

  @NotNull
  public static Collection<LookupElement> getAllExportedFunctionsWithModuleLookupElements(@NotNull Project project,
                                                                                          boolean withArity,
                                                                                          @Nullable String exclude) {
    List<LookupElement> lookupElements = ContainerUtil.newArrayList();
    for (String moduleName : ErlangModuleIndex.getNames(project)) {
      if (moduleName.equals(exclude)) continue;
      for (ErlangFunction function : getExternalFunctionForCompletion(project, moduleName)) {
        String functionName = function.getName();
        String fullName = moduleName + ":" + functionName;
        int arity = function.getArity();
        lookupElements.add(
          PrioritizedLookupElement.withPriority(
            LookupElementBuilder.create(function, fullName)
              .withIcon(ErlangIcons.FUNCTION).withTailText("/" + arity)
              .withInsertHandler(getInsertHandler(functionName, moduleName, arity, withArity)),
            ErlangCompletionContributor.EXTERNAL_FUNCTIONS_PRIORITY));
      }
    }
    return lookupElements;
  }

  private static void addBifs(@NotNull List<LookupElement> lookupElements, @NotNull Collection<ErlangBifDescriptor> bifs, boolean withArity) {
    for (ErlangBifDescriptor bif : bifs) {
      lookupElements.add(createFunctionLookupElement(bif.getName(), bif.getArity(), withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY));
    }
  }

  @NotNull
  public static List<LookupElement> createFunctionLookupElements(@NotNull List<ErlangFunction> functions, final boolean withArity) {
    return ContainerUtil.map(functions, function -> createFunctionsLookupElement(function, withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY));
  }

  @NotNull
  private static LookupElement createFunctionsLookupElement(@NotNull ErlangFunction function, boolean withArity, double priority) {
    int arity = function.getArity();
    return PrioritizedLookupElement.withPriority(LookupElementBuilder.create(function)
      .withIcon(ErlangIcons.FUNCTION).withTailText("/" + arity)
      .withInsertHandler(getInsertHandler(function.getName(), arity, withArity)), priority);
  }

  @NotNull
  private static LookupElement createFunctionLookupElement(@NotNull String name, int arity, boolean withArity, int priority) {
    return PrioritizedLookupElement.withPriority(LookupElementBuilder.create(name + arity, name)
      .withIcon(ErlangIcons.FUNCTION).withTailText("/" + arity)
      .withInsertHandler(getInsertHandler(name, arity, withArity)), (double) priority);
  }

  @NotNull
  private static InsertHandler<LookupElement> getInsertHandler(@NotNull final String name, final int arity, boolean withArity) {
    return getInsertHandler(name, null, arity, withArity);
  }

  @NotNull
  private static InsertHandler<LookupElement> getInsertHandler(@NotNull final String name, @Nullable final String moduleName, final int arity, boolean withArity) {
    return withArity ?
      new BasicInsertHandler<LookupElement>() {
        @Override
        public void handleInsert(@NotNull InsertionContext context, LookupElement item) {
          QuoteInsertHandler.process(name, moduleName, context);
          Editor editor = context.getEditor();
          Document document = editor.getDocument();
          context.commitDocument();
          PsiElement next = findNextToken(context);
          ASTNode intNode = FormatterUtil.getNextNonWhitespaceSibling(next != null ? next.getNode() : null);

          if (next != null && "/".equals(next.getText())) {
            next.delete();
          }
          if (intNode != null && intNode.getElementType() == ErlangTypes.ERL_INTEGER) {
            intNode.getPsi().delete();
          }
          PsiDocumentManager.getInstance(context.getProject()).doPostponedOperationsAndUnblockDocument(document);
          document.insertString(context.getTailOffset(), "/" + arity);
          editor.getCaretModel().moveToOffset(context.getTailOffset());
        }

        @Nullable
        private PsiElement findNextToken(@NotNull InsertionContext context) {
          PsiFile file = context.getFile();
          PsiElement element = file.findElementAt(context.getTailOffset());
          if (element instanceof PsiWhiteSpace) {
            element = file.findElementAt(element.getTextRange().getEndOffset());
          }
          return element;
        }
      } :
      new ParenthesesInsertHandler<LookupElement>() {
        @Override
        public void handleInsert(@NotNull InsertionContext context, LookupElement item) {
          QuoteInsertHandler.process(name, moduleName, context);
          super.handleInsert(context, item);
        }

        @Override
        protected boolean placeCaretInsideParentheses(InsertionContext context, LookupElement item) {
          return arity > 0;
        }
      };
  }

  @NotNull
  public static List<LookupElement> getMacrosLookupElements(@NotNull PsiFile containingFile) {
    if (containingFile instanceof ErlangFile) {
      List<ErlangMacrosDefinition> concat = ContainerUtil.concat(((ErlangFile) containingFile).getMacroses(), getErlangMacrosFromIncludes((ErlangFile) containingFile, true, ""));
      List<LookupElement> fromFile = ContainerUtil.map(
        concat,
        md -> LookupElementBuilder.create(md).withIcon(ErlangIcons.MACROS));
      List<LookupElement> stdMacros = ContainerUtil.newArrayList();
      for (String m : KNOWN_MACROS) {
        stdMacros.add(LookupElementBuilder.create(m).withIcon(ErlangIcons.MACROS));
      }
      return ContainerUtil.concat(fromFile, stdMacros);
    }
    return Collections.emptyList();
  }

  @NotNull
  public static List<LookupElement> getRecordLookupElements(@NotNull PsiFile containingFile) {
    if (containingFile instanceof ErlangFile) {
      List<ErlangRecordDefinition> concat = ContainerUtil.concat(((ErlangFile) containingFile).getRecords(), getErlangRecordFromIncludes((ErlangFile) containingFile, true, ""));
      return ContainerUtil.map(
        concat,
        rd -> LookupElementBuilder.create(rd).withIcon(ErlangIcons.RECORD));
    }
    return Collections.emptyList();
  }

  @NotNull
  public static List<LookupElement> getTypeLookupElements(@NotNull PsiFile containingFile, boolean addBuiltInTypes, final boolean withArity) {
    if (containingFile instanceof ErlangFile) {
      ErlangFile erlangFile = (ErlangFile) containingFile;
      List<ErlangTypeDefinition> types = ContainerUtil.concat(erlangFile.getTypes(), getErlangTypeFromIncludes(erlangFile, true, ""));

      final ParenthesesInsertHandler<LookupElement> handler = new ParenthesesInsertHandler<LookupElement>() {
        @Override
        protected boolean placeCaretInsideParentheses(InsertionContext context, LookupElement item) {
          return false;
        }
      };

      List<LookupElement> builtInTypes = addBuiltInTypes ? ContainerUtil.map(BUILT_IN_TYPES, s -> PrioritizedLookupElement.withPriority(
        LookupElementBuilder.create(s).withIcon(ErlangIcons.TYPE).withInsertHandler(handler),
        ErlangCompletionContributor.TYPE_PRIORITY)) : ContainerUtil.emptyList();

      List<LookupElement> foundedTypes = ContainerUtil.map(
        types,
        rd -> PrioritizedLookupElement.withPriority(
          LookupElementBuilder.create(rd).withIcon(ErlangIcons.TYPE).withInsertHandler(getInsertHandler(rd.getName(), getArity(rd), withArity)),
          ErlangCompletionContributor.TYPE_PRIORITY));
      return ContainerUtil.concat(foundedTypes, builtInTypes);
    }
    return Collections.emptyList();
  }

  public static int getArity(@NotNull ErlangTypeDefinition o) {
    ErlangTypeDefinitionStub stub = o.getStub();
    if (stub != null) return stub.getArity();
    ErlangArgumentDefinitionList argumentDefinitionList = o.getArgumentDefinitionList();
    if (argumentDefinitionList == null) return 0;
    return argumentDefinitionList.getArgumentDefinitionList().size();
  }

  private static int calculateFunctionClauseArity(@NotNull ErlangFunctionClause clause) {
    ErlangArgumentDefinitionList argumentDefinitionList = clause.getArgumentDefinitionList();
    return argumentDefinitionList.getArgumentDefinitionList().size();
  }

  @NotNull
  public static String getName(@NotNull ErlangFunction o) {
    return getNameImpl(o);
  }

  @NotNull
  public static String getName(@NotNull ErlangImportFunction o) {
    return getName(o.getQAtom());
  }

  @NotNull
  public static String getName(@NotNull ErlangQVar o) {
    return o.getText();
  }

  public static int getArity(@NotNull ErlangFunction o) {
    ErlangFunctionStub stub = o.getStub();
    if (stub != null) return stub.getArity();
    return o.getFirstClause().getArgumentDefinitionList().getArgumentDefinitionList().size();
  }

  public static int getArity(@NotNull ErlangSpecification o) {
    ErlangSpecificationStub stub = o.getStub();
    if (stub != null) return stub.getArity();

    ErlangFunTypeSigs signature = getSignature(o);
    List<ErlangTypeSig> typeSigs = signature != null ? signature.getTypeSigList()
                                                     : ContainerUtil.emptyList();
    ErlangTypeSig sig = typeSigs.size() > 0 ? typeSigs.get(0) : null;
    return sig != null ? sig.getFunType().getFunTypeArguments().getTypeList().size() : -1;
  }

  public static int getArity(@NotNull ErlangImportFunction o) {
    return getArity(o.getInteger());
  }

  @NotNull
  public static String getName(@NotNull ErlangRecordDefinition o) {
    return getNameImpl(o);
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    return atom != null ? getNameIdentifier(atom) : o;
  }

  public static int getTextOffset(@NotNull ErlangRecordDefinition o) {
    if (o.getNameIdentifier() == o) return 0;//o.getNode().getTextOffset();
    return o.getNameIdentifier().getTextOffset();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangQVar o) {
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangQAtom o) {
    ErlangAtom atom = o.getAtom();
    return atom != null ? getNameIdentifier(atom) : o;
  }

  @NotNull
  public static String getName(@NotNull ErlangQAtom o) {
    return getNameIdentifier(o).getText();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangMacrosName o) {
    ErlangAtom atom = o.getAtom();
    if (atom != null) {
      return getNameIdentifier(atom);
    }
    return ObjectUtils.notNull(o.getVar(), o);
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangFunction o) {
    return getNameIdentifier(o.getAtomName());
  }

  @Nullable
  public static PsiReference getReferenceInternal(@NotNull ErlangRecordExpression o) {
    ErlangRecordRef recordRef = o.getRecordRef();
    return recordRef != null ? recordRef.getReference() : null;
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangRecordRef o) {
    return createRecordRef(o.getQAtom());
  }

  @NotNull
  public static ErlangRecordReferenceImpl createRecordRef(@NotNull ErlangQAtom atom) {
    return new ErlangRecordReferenceImpl(atom);
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangModuleRef o) {
    return createModuleReference(o.getQAtom());
  }

  @NotNull
  public static PsiReference createModuleReference(@NotNull ErlangQAtom atom) {
    return new ErlangModuleReferenceImpl(atom);
  }

  @Nullable
  @Contract("null->null")
  public static ErlangFile resolveToFile(@Nullable ErlangModuleRef ref) {
    PsiReference reference = ref != null ? ref.getReference() : null;
    PsiElement resolved = reference != null ? reference.resolve() : null;
    return ObjectUtils.tryCast(resolved != null ? resolved.getContainingFile() : null, ErlangFile.class);
  }

  @Nullable
  private static ErlangFunction resolveToFunction(@Nullable ErlangFunctionCallExpression call) {
    PsiReference reference = call != null ? call.getReference() : null;
    PsiElement resolvedFunction = reference != null ? reference.resolve() : null;
    return ObjectUtils.tryCast(resolvedFunction, ErlangFunction.class);
  }

  @Nullable
  private static ErlangExpression getFunctionCallArgument(@NotNull ErlangFunctionCallExpression call,
                                                          @Nullable PsiElement child) {
    ErlangArgumentList argumentList = call.getArgumentList();
    PsiElement argument = child;
    PsiElement parent;
    while (argument != null && (parent = argument.getParent()) != argumentList) {
      argument = parent;
    }
    return ObjectUtils.tryCast(argument, ErlangExpression.class);
  }

  private static int getPositionInFunctionCall(@NotNull ErlangFunctionCallExpression call, @Nullable PsiElement child) {
    return call.getArgumentList().getExpressionList().indexOf(getFunctionCallArgument(call, child));
  }

  public static boolean renameQAtom(@Nullable ErlangQAtom qAtom, String newName) {
    return renameAtom(qAtom != null ? qAtom.getAtom() : null, newName);
  }

  public static boolean renameAtom(@Nullable ErlangAtom atom, String newName) {
    if (atom != null) {
      atom.setName(newName);
      return true;
    }
    return false;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangFunction o, @NotNull String newName) {
    for (ErlangFunctionClause clause : o.getFunctionClauseList()) {
      renameQAtom(clause.getQAtom(), newName);
    }
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangQVar o, @NotNull String newName) {
    o.replace(ErlangElementFactory.createQVarFromText(o.getProject(), newName));
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangRecordDefinition o, @NotNull String newName) {
    renameQAtom(o.getQAtom(), newName);
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangTypeDefinition o, @NotNull String newName) {
    renameQAtom(o.getQAtom(), newName);
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangSpecification o, @NotNull String newName) {
    ErlangFunTypeSigs sigs = o.getFunTypeSigs();
    renameQAtom(sigs != null ? sigs.getSpecFun().getQAtom() : null, newName);
    return o;
  }

  @NotNull
  public static String getName(@NotNull ErlangModule o) {
    return getNameImpl(o);
  }

  @NotNull
  public static String getName(@NotNull ErlangSpecification o) {
    ErlangSpecificationStub stub = o.getStub();
    String nameFromStub = stub != null ? stub.getName() : null;
    if (nameFromStub != null) return nameFromStub;

    ErlangFunTypeSigs signature = getSignature(o);
    ErlangAtom atom = signature != null ? signature.getSpecFun().getQAtom().getAtom() : null;
    return atom != null ? atom.getName() : "";
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangModule o, @NotNull String newName) {
    VirtualFile virtualFile = o.getContainingFile().getVirtualFile();
    if (virtualFile != null) {
      try {
        String ext = FileUtilRt.getExtension(virtualFile.getName());
        virtualFile.rename(o, StringUtil.replace(newName, "'", "") + "." + ext);
        renameQAtom(o.getQAtom(), newName);
      } catch (Exception ignored) {
      }
    }
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangModule o) {
    ErlangQAtom qAtom = o.getQAtom();
    return qAtom != null ? getNameIdentifier(qAtom) : o;
  }

  public static int getTextOffset(@NotNull ErlangModule o) {
    if (o.getNameIdentifier() == o) return 0; //o.getNode().getTextOffset();
    return o.getNameIdentifier().getTextOffset();
  }

  @NotNull
  public static String getName(@NotNull ErlangFunctionCallExpression o) {
    return o.getNameIdentifier().getText();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangFunctionCallExpression o) {
    return getNameIdentifier(o.getQAtom());
  }

  public static int getTextOffset(@NotNull ErlangFunctionCallExpression o) {
    return o.getQAtom().getTextOffset();
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangListComprehension o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processDeclarationRecursive(o, processor, state);
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangCaseExpression o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    List<ErlangCrClause> crClauseList = o.getCrClauseList();
    boolean result = true;
    for (ErlangCrClause c : crClauseList) {
      ErlangClauseBody clauseBody = c.getClauseBody();
      if (clauseBody != null) result &= processDeclarationRecursive(clauseBody, processor, state);
    }
    return result;
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangModule o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processDeclarationRecursive(o, processor, state);
  }

  private static boolean processDeclarationRecursive(ErlangCompositeElement o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state) {
    Queue<ErlangCompositeElement> queue = new LinkedList<>();
    queue.add(o);
    while (!queue.isEmpty()) {
      ErlangCompositeElement top = queue.remove();
      if (!processor.execute(top, state)) return false;
      queue.addAll(PsiTreeUtil.getChildrenOfTypeAsList(top, ErlangCompositeElement.class));
    }
    return true;
  }

  static boolean inModule(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangModule.class) != null;
  }

  @NotNull
  private static Collection<ErlangFile> getIncludedFiles(@NotNull ErlangFile file) {
    HashSet<ErlangFile> includedFiles = new HashSet<>();
    addIncludedFiles(file, includedFiles);
    return includedFiles;
  }

  private static void addIncludedFiles(@NotNull ErlangFile erlangFile, @NotNull Set<ErlangFile> alreadyAdded) {
    List<ErlangFile> directlyIncludedFiles = getDirectlyIncludedFiles(erlangFile);
    boolean added = false;
    for (ErlangFile f : directlyIncludedFiles) {
      added |= alreadyAdded.add(f);
    }
    if (added) {
      for (ErlangFile f : directlyIncludedFiles) {
        addIncludedFiles(f, alreadyAdded);
      }
    }
  }

  @NotNull
  public static List<ErlangFile> getDirectlyIncludedFiles(@NotNull ErlangFile erlangFile) {
    List<ErlangFile> files = ContainerUtil.newArrayList();
    for (ErlangInclude include : erlangFile.getIncludes()) {
      files.addAll(getDirectlyIncludedFiles(include, erlangFile));
    }
    for (ErlangIncludeLib includeLib : erlangFile.getIncludeLibs()) {
      files.addAll(getDirectlyIncludedFiles(includeLib, erlangFile));
    }
    return files;
  }

  @NotNull
  public static List<ErlangFile> getDirectlyIncludedFiles(@NotNull ErlangIncludeLib includeLib, @NotNull ErlangFile erlangFile) {
    ErlangIncludeString includeString = includeLib.getIncludeStringSafe();
    String[] split = includeString != null ? StringUtil.unquoteString(includeString.getText()).split("/") : null;

    if (split != null && split.length >= 2) {
      String libName = split[0];
      String relativePath = StringUtil.join(split, 1, split.length, "/");
      Project project = includeLib.getProject();
      VirtualFile appDir = ErlangApplicationIndex.getApplicationDirectoryByName(libName, GlobalSearchScope.allScope(project));
      ErlangFile includedFile = getRelativeErlangFile(project, relativePath, appDir);
      if (includedFile != null) {
        return ContainerUtil.newSmartList(includedFile);
      }
    }
    //either include_lib does not specify a library, or it was not found, falling back to 'include' behaviour.
    return getDirectlyIncludedFiles(includeString, erlangFile);
  }

  @NotNull
  public static List<ErlangFile> getDirectlyIncludedFiles(@NotNull ErlangInclude include, @NotNull ErlangFile erlangFile) {
    ErlangIncludeString includeString = include.getIncludeStringSafe();
    return getDirectlyIncludedFiles(includeString, erlangFile);
  }

  @NotNull
  private static List<ErlangFile> getDirectlyIncludedFiles(@Nullable ErlangIncludeString includeString,
                                                           @NotNull ErlangFile erlangFile) {
    if (includeString == null) return ContainerUtil.emptyList();
    VirtualFile containingVirtualFile = erlangFile.getOriginalFile().getVirtualFile();
    VirtualFile parent = containingVirtualFile != null ? containingVirtualFile.getParent() : null;
    String relativePath = StringUtil.unquoteString(includeString.getText());
    Project project = erlangFile.getProject();
    ErlangFile relativeToDirectParent = getRelativeErlangFile(project, relativePath, parent);
    if (relativeToDirectParent != null) return ContainerUtil.newSmartList(relativeToDirectParent);
    //relative to direct parent include file was not found
    //let's search in include directories
    if (containingVirtualFile != null) {
      Module module = ModuleUtilCore.findModuleForFile(containingVirtualFile, project);
      for (VirtualFile includeDir : ErlangIncludeDirectoryUtil.getIncludeDirectories(module)) {
        ErlangFile includedFile = getRelativeErlangFile(project, relativePath, includeDir);
        if (includedFile != null) return ContainerUtil.newSmartList(includedFile);
      }
    }
    //TODO consider providing source roots functionality to small IDEs
    if (ErlangSystemUtil.isSmallIde()) {
      VirtualFile appRoot = getContainingOtpAppRoot(project, parent);
      return getDirectlyIncludedFilesForSmallIde(project, relativePath, appRoot);
    }
    return ContainerUtil.emptyList();
  }

  @NotNull
  private static List<ErlangFile> getDirectlyIncludedFilesForSmallIde(@NotNull Project project, @NotNull String includeStringPath, @Nullable VirtualFile otpAppRoot) {
    if (otpAppRoot == null) return ContainerUtil.emptyList();
    VirtualFile otpIncludeDirectory = otpAppRoot.findChild("include");
    ErlangFile relativeToOtpIncludeDirectory = getRelativeErlangFile(project, includeStringPath, otpIncludeDirectory);
    if (relativeToOtpIncludeDirectory != null) return ContainerUtil.newSmartList(relativeToOtpIncludeDirectory);
    //we haven't found it in 'include' directory, let's try include paths listed in rebar.config
    ErlangFile rebarConfigPsi = RebarConfigUtil.getRebarConfig(project, otpAppRoot);
    if (rebarConfigPsi != null) {
      for(String includePath : ContainerUtil.reverse(RebarConfigUtil.getIncludePaths(rebarConfigPsi))) {
        VirtualFile includePathVirtualFile = VfsUtilCore.findRelativeFile(includePath, otpAppRoot);
        ErlangFile includedFile = getRelativeErlangFile(project, includeStringPath, includePathVirtualFile);
        if (includedFile != null) return ContainerUtil.newSmartList(includedFile);
      }
    }
    return ContainerUtil.emptyList();
  }

  @Nullable
  public static VirtualFile getContainingOtpAppRoot(@NotNull Project project, @Nullable final VirtualFile file) {
    if (file == null) return null;
    List<VirtualFile> allOtpAppRoots = ErlangApplicationIndex.getAllApplicationDirectories(project, GlobalSearchScope.allScope(project));
    List<VirtualFile> containingOtpAppRoots = ContainerUtil.filter(allOtpAppRoots, appRoot -> VfsUtilCore.isAncestor(appRoot, file, true));
    //sort it in order to have longest path first
    ContainerUtil.sort(containingOtpAppRoots, (o1, o2) -> o2.getPath().length() - o1.getPath().length());
    return ContainerUtil.getFirstItem(containingOtpAppRoots);
  }

  public static void addDeclaredBehaviourModuleNames(@NotNull ErlangFile file, @NotNull Set<String> behaviourNames) {
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      ContainerUtil.addIfNotNull(getName(behaviour), behaviourNames);
    }
  }

  public static void extractParseTransforms(@NotNull ErlangListExpression list, @NotNull Set<String> parseTransforms) {
    for (ErlangExpression expr : list.getExpressionList()) {
      if (expr instanceof ErlangTupleExpression) {
        extractParseTransforms((ErlangTupleExpression) expr, parseTransforms);
      }
    }
  }

  public static void extractParseTransforms(@NotNull ErlangTupleExpression tuple, @NotNull Set<String> parseTransforms) {
    List<ErlangExpression> expressionList = tuple.getExpressionList();
    if (expressionList.size() != 2) return;
    ErlangExpression first = expressionList.get(0);
    if (!"parse_transform".equals(getAtomName(first instanceof ErlangMaxExpression ? (ErlangMaxExpression) first : null))) return;
    ErlangExpression second = expressionList.get(1);
    String parseTransformModuleName = getAtomName(second instanceof ErlangMaxExpression ? (ErlangMaxExpression) second : null);
    ContainerUtil.addIfNotNull(parseTransformModuleName, parseTransforms);
  }

  @Nullable
  public static String getAtomName(@Nullable ErlangMaxExpression expression) {
    return expression != null ? getAtomName(expression.getQAtom()) : null;
  }

  @Nullable
  private static String getAtomName(@Nullable ErlangQAtom qAtom) {
    ErlangAtom atom = qAtom != null ? qAtom.getAtom() : null;
    return atom != null ? atom.getName() : null;
  }

  @Nullable
  private static ErlangFile getRelativeErlangFile(@NotNull Project project, @NotNull String relativePath, @Nullable VirtualFile parent) {
    VirtualFile relativeFile = VfsUtilCore.findRelativeFile(relativePath, parent);
    if (relativeFile == null) return null;
    PsiFile file = PsiManager.getInstance(project).findFile(relativeFile);
    return file instanceof ErlangFile ? (ErlangFile) file : null;
  }

  @NotNull
  static List<ErlangRecordDefinition> getErlangRecordFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangRecordDefinition> fromIncludes = ContainerUtil.newArrayList();
    for (ErlangFile file : getIncludedFiles(containingFile)) {
      if (!forCompletion) {
        ContainerUtil.addIfNotNull(fromIncludes, file.getRecord(name));
      }
      else {
        fromIncludes.addAll(file.getRecords());
      }
    }
    return fromIncludes;
  }

  @NotNull
  static List<ErlangFunction> getErlangFunctionsFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, @NotNull String name, int arity) {
    List<ErlangFunction> fromIncludes = ContainerUtil.newArrayList();
    for (ErlangFile file : getIncludedFiles(containingFile)) {
      if (!forCompletion) {
        ContainerUtil.addIfNotNull(fromIncludes, file.getFunction(name, arity));
      }
      else {
        fromIncludes.addAll(file.getFunctions());
      }
    }
    return fromIncludes;
  }

  @NotNull
  static List<ErlangImportFunction> getImportsFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, @NotNull String name, int arity) {
    List<ErlangImportFunction> fromIncludes = ContainerUtil.newArrayList();
    for (ErlangFile file : getIncludedFiles(containingFile)) {
      if (!forCompletion) {
        ContainerUtil.addIfNotNull(fromIncludes, file.getImportedFunction(name, arity));
      }
      else {
        fromIncludes.addAll(file.getImportedFunctions());
      }
    }
    return fromIncludes;
  }

  @NotNull
  static List<ErlangMacrosDefinition> getErlangMacrosFromIncludes(@NotNull ErlangFile containingFile,
                                                                  boolean forCompletion,
                                                                  @NotNull String name) {
    List<ErlangMacrosDefinition> fromIncludes = ContainerUtil.newArrayList();
    for (ErlangFile file : getIncludedFiles(containingFile)) {
      if (!forCompletion) {
        ContainerUtil.addIfNotNull(fromIncludes, file.getMacros(name));
      }
      else {
        fromIncludes.addAll(file.getMacroses());
      }
    }
    return fromIncludes;
  }

  @NotNull
  static List<ErlangTypeDefinition> getErlangTypeFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, @NotNull String name) {
    List<ErlangTypeDefinition> fromIncludes = ContainerUtil.newArrayList();
    for (ErlangFile file : getIncludedFiles(containingFile)) {
      if (!forCompletion) {
        ContainerUtil.addIfNotNull(fromIncludes, file.getType(name));
      }
      else {
        fromIncludes.addAll(file.getTypes());
      }
    }
    return fromIncludes;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangAtom atom) {
    PsiElement name = atom.getAtomName();
    return name != null ? name : atom;
  }

  @NotNull
  public static ErlangAtom setName(@NotNull ErlangAtom atom, @NotNull String newName) {
    String text = toAtomName(newName);
    assert text != null;
    ErlangAtom newAtom = ErlangElementFactory.createAtomFromText(atom.getProject(), text);
    return (ErlangAtom) atom.replace(newAtom);
  }

  @Nullable
  public static String toAtomName(@NotNull String maybeUnquotedAtomName) {
    ThreeState t = atomNameRequiresQuotes(maybeUnquotedAtomName);
    return t == ThreeState.YES ? '\'' + maybeUnquotedAtomName + '\'' : t == ThreeState.NO ? maybeUnquotedAtomName : null;
  }

  public static boolean needQuotation(@NotNull String atomName) {
    return atomNameRequiresQuotes(atomName) == ThreeState.YES;
  }

  @NotNull
  private static ThreeState atomNameRequiresQuotes(@NotNull String atomName) {
    //check if it's a regular atom
    Matcher matcher = ATOM_PATTERN.matcher(atomName);
    if (matcher.matches()) {
      return ThreeState.NO;
    }
    //check if it's a quoted atom name
    matcher = QUOTED_ATOM_NAME.matcher(atomName);
    if (matcher.matches()) {
      return ThreeState.YES;
    }
    //check if it's already quoted
    matcher = QUOTED_ATOM_NAME.matcher(StringUtil.unquoteString(atomName, '\''));
    if (matcher.matches()) {
      return ThreeState.NO;
    }
    return ThreeState.UNSURE;
  }

  @NotNull
  public static String getName(@NotNull ErlangAtom atom) {
    return atom.getNameIdentifier().getText();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangMacrosDefinition o) {
    ErlangMacrosName macrosName = o.getMacrosName();
    return macrosName != null ? getNameIdentifier(macrosName) : o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangTypeDefinition o) {
    ErlangQAtom qAtom = o.getQAtom();
    return qAtom != null ? getNameIdentifier(qAtom) : o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangSpecification o) {
    ErlangFunTypeSigs sigs = o.getFunTypeSigs();
    return sigs != null ? getNameIdentifier(sigs.getSpecFun().getQAtom()) : o;
  }

  public static int getTextOffset(@NotNull ErlangTypeDefinition o) {
    if (o.getQAtom() == null) return 0;
    return getNameIdentifier(o).getTextOffset();
  }

  public static int getTextOffset(@NotNull ErlangMacrosDefinition o) {
    if (o.getMacrosName() == null) return 0;
    return getNameIdentifier(o).getTextOffset();
  }

  @NotNull
  public static String getName(@NotNull ErlangMacrosDefinition o) {
    return getNameImpl(o);
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangMacrosDefinition o, @NotNull String newName) {
    ErlangMacrosName macrosName = o.getMacrosName();
    if (macrosName != null) {
      setName(macrosName, newName);
    }
    return o;
  }

  public static void setName(@NotNull ErlangMacrosName macroName, @NotNull String newName) {
    PsiElement replacement = createMacroNameReplacement(macroName.getProject(), newName);
    if (macroName.getAtom() != null) {
      macroName.getAtom().replace(replacement);
    }
    else if (macroName.getVar() != null) {
      macroName.getVar().replace(replacement);
    }
    else {
      throw new AssertionError("Unexpected PSI structure");
    }
  }

  @NotNull
  private static PsiElement createMacroNameReplacement(@NotNull Project project, @NotNull String newName) {
    ErlangMacrosName macroName = null;
    try {
      macroName = (ErlangMacrosName) ErlangElementFactory.createMacrosFromText(project, newName);
    } catch (Exception ignore) {
    }
    if (macroName == null) {
      try {
        macroName = (ErlangMacrosName) ErlangElementFactory.createMacrosFromText(project, '\'' + newName + '\'');
      } catch (Exception ignore) {
      }
    }
    if (macroName != null) {
      if (macroName.getAtom() != null) {
        return macroName.getAtom();
      }
      else if (macroName.getVar() != null) {
        return macroName.getVar();
      }
    }
    throw new AssertionError("Cannot create macro name replacement");
  }

  @NotNull
  public static String getName(@NotNull ErlangBehaviour o) {
    String fromStub = getNameFromStub(o);
    if (fromStub != null) return fromStub;

    ErlangModuleRef moduleRef = o.getModuleRef();
    ErlangQAtom atom = moduleRef != null ? moduleRef.getQAtom() : null;
    return atom == null ? "" : getName(atom);
  }

  @NotNull
  private static List<ErlangFunction> getExternalFunctionForCompletion(@NotNull Project project,
                                                                       @NotNull String moduleName) {
    List<ErlangFunction> result = ContainerUtil.newArrayList();
    List<ErlangFile> erlangModules = ErlangModuleIndex.getFilesByName(project, moduleName, GlobalSearchScope.allScope(project));
    for (ErlangFile file : erlangModules) {
      result.addAll(file.getExportedFunctions());
    }
    return result;
  }

  public static boolean inFunction(@Nullable PsiElement position) {
    return PsiTreeUtil.getParentOfType(position, ErlangFunction.class) != null;
  }

  @NotNull
  public static String getName(@NotNull ErlangTypedExpr o) {
    return getNameIdentifier(o).getText();
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangTypedExpr o, String newName) {
    ErlangQAtom qAtom = o.getQAtom();
    renameQAtom(qAtom, newName);
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangTypedExpr o) {
    return getNameIdentifier(o.getQAtom());
  }

  @NotNull
  public static String getName(@NotNull ErlangAtomAttribute o) {
    return o.getAtomName().getText();
  }

  public static int getTextOffset(@NotNull ErlangTypedExpr o) {
    return o.getNameIdentifier().getTextOffset();
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangSpecFun o) {
    ErlangQAtom atom = o.getQAtom();
    ErlangModuleRef moduleRef = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    Integer arity = getArity(o);

    if (arity != null) {
      return new ErlangFunctionReferenceImpl(atom, moduleRef == null ? null : moduleRef.getQAtom(), arity);
    }
    return null;
  }

  @Nullable
  private static Integer getArity(@NotNull ErlangSpecFun o) {
    PsiElement integer = o.getInteger();
    Integer arity = null;
    if (integer != null) arity = getArity(integer);
    ErlangTypeSig sigs = PsiTreeUtil.getNextSiblingOfType(o, ErlangTypeSig.class);
    if (arity == null && sigs != null) arity = sigs.getFunType().getFunTypeArguments().getTypeList().size();
    return arity;
  }

  @Nullable
  public static ErlangFunTypeSigs getSignature(@Nullable ErlangSpecification o) {
    if (o == null) return null;
    ErlangFunTypeSigsBraces sigsBraces = o.getFunTypeSigsBraces();
    if (sigsBraces != null) {
      return sigsBraces.getFunTypeSigs();
    }
    return o.getFunTypeSigs();
  }

  @NotNull
  public static ItemPresentation getPresentation(@NotNull final ErlangFunction o) {
    return new ItemPresentation() {
      @Nullable
      @Override
      public String getPresentableText() {
        return createFunctionPresentation(o);
      }

      @Nullable
      @Override
      public String getLocationString() {
        return o.getContainingFile().getName();
      }

      @Nullable
      @Override
      public Icon getIcon(boolean b) {
        return o.getIcon(0);
      }
    };
  }

  @NotNull
  public static String createFunctionPresentation(@NotNull ErlangAtomWithArityExpression function) {
    return createFunctionPresentation(getName(function.getQAtom()), getArity(function.getInteger()));
  }

  @NotNull
  public static String createFunctionClausePresentation(@NotNull ErlangFunctionClause clause) {
    return createFunctionPresentation(getName(clause.getQAtom()), calculateFunctionClauseArity(clause));
  }

  @NotNull
  public static String createFunctionPresentation(@NotNull ErlangFunction function) {
    return createFunctionPresentation(function.getName(), function.getArity());
  }

  @NotNull
  public static String createFunctionPresentation(@NotNull ErlangImportFunction function) {
    return createFunctionPresentation(getName(function), getArity(function));
  }

  @NotNull
  public static String createFunctionPresentation(@NotNull String functionName, int arity) {
    return toAtomName(functionName) + "/" + arity;
  }

  @NotNull
  public static String getQualifiedFunctionName(@NotNull ErlangFunction function) {
    PsiFile file = function.getContainingFile();
    ErlangFile erlangFile = file instanceof ErlangFile ? (ErlangFile) file : null;
    ErlangModule module = erlangFile != null ? erlangFile.getModule() : null;
    return module != null ? module.getName() + ":" + function.getName() : function.getName();
  }

  @NotNull
  public static String createFunctionPresentationFromCallbackSpec(@NotNull ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = getFunTypeSigs(spec);
    String funName = getCallbackSpecName(spec);

    List<ErlangTypeSig> typeSigList = funTypeSigs != null ? funTypeSigs.getTypeSigList() : null;
    ErlangTypeSig first = ContainerUtil.getFirstItem(typeSigList);
    int arity = first != null ? first.getFunType().getFunTypeArguments().getTypeList().size() : 0;

    return funName + "/" + arity;
  }

  @NotNull
  public static String createTypePresentation(@NotNull ErlangTypeDefinition type) {
    return type.getName() + "/" + getArity(type);
  }

  @NotNull
  @SuppressWarnings("UnusedParameters")
  public static Icon getIcon(@NotNull ErlangFunction o, int flags) {
    return ErlangIcons.FUNCTION;
  }

  public static boolean isRecursiveCall(PsiElement element, ErlangFunction function) {
    return Comparing.equal(PsiTreeUtil.getParentOfType(element, ErlangFunction.class), function);
  }

  public static boolean isEunitTestFile(@NotNull ErlangFile file) {
    VirtualFile virtualFile = file.getVirtualFile();
    String withoutExtension = virtualFile != null ? virtualFile.getNameWithoutExtension() : "";
    return (StringUtil.endsWith(withoutExtension, "_test") || StringUtil.endsWith(withoutExtension, "_tests")) && isEunitImported(file);
  }

  public static boolean isEunitTestFunction(@NotNull ErlangFunction function) {
    String name = function.getName();
    return isEunitTestFunctionName(name) || isEunitTestGeneratorFunctionName(name);
  }

  private static boolean isEunitTestFunctionName(@NotNull String functionName) {
    return StringUtil.endsWith(functionName, "_test");
  }

  public static boolean isEunitTestGeneratorFunctionName(@NotNull String functionName) {
    return StringUtil.endsWith(functionName, "_test_");
  }

  public static boolean isEunitImported(@NotNull ErlangFile file) {
    return isEunitDirectlyImported(file) ||
           ContainerUtil.find(getIncludedFiles(file), ErlangPsiImplUtil::isEunitDirectlyImported) != null;
  }

  private static boolean isEunitDirectlyImported(@NotNull ErlangFile file) {
    List<ErlangIncludeLib> includes = file.getIncludeLibs();
    for (ErlangIncludeLib include : includes) {
      ErlangIncludeString string = include.getIncludeStringSafe();
      if (string != null) {
        String includeFilePath = StringUtil.unquoteString(string.getText());
        return StringUtil.equals(includeFilePath, "eunit/include/eunit.hrl");
      }
    }
    return false;
  }

  @NotNull
  public static SearchScope getUseScope(@NotNull ErlangQVarImpl o) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(o, ErlangFunction.class, true);
    if (function != null) {
      return new LocalSearchScope(function);
    }
    return ResolveScopeManager.getElementUseScope(o);
  }

  @NotNull
  public static String getName(@NotNull ErlangTypeDefinition o) {
    return getNameImpl(o);
  }

  @Nullable
  public static ErlangSpecification findSpecification(@Nullable ErlangFunction function) {
    ErlangFile file = ObjectUtils.tryCast(function != null ? function.getContainingFile() : null, ErlangFile.class);
    if (file != null) {
      String name = function.getName();
      int arity = function.getArity();
      for (ErlangSpecification spec : file.getSpecifications()) {
        if (name.equals(spec.getName()) && arity == spec.getArity()) return spec;
      }
    }
    return null;
  }

  public static boolean notFromPreviousFunction(@NotNull PsiElement spec, @Nullable ErlangFunction prevFunction) {
    return prevFunction == null || spec.getTextOffset() > prevFunction.getTextOffset();
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean isValidHost(@NotNull ErlangStringLiteral o) {
    return true;
  }

  @NotNull
  public static ErlangStringLiteral updateText(@NotNull ErlangStringLiteral o, @NotNull String text) {
    ErlangExpression expression = ErlangElementFactory.createExpressionFromText(o.getProject(), text);
    return (ErlangStringLiteralImpl)o.replace(expression);
  }

  @NotNull
  public static ErlangStringLiteralEscaper createLiteralTextEscaper(@NotNull ErlangStringLiteral o) {
    return new ErlangStringLiteralEscaper(o);
  }

  @Nullable
  public static ErlangQAtom getQAtom(@Nullable ErlangColonQualifiedExpression colonQualifier) {
    ErlangExpression firstExpression = colonQualifier == null ? null : ContainerUtil.getFirstItem(colonQualifier.getExpressionList());
    return firstExpression instanceof ErlangMaxExpression ? ((ErlangMaxExpression) firstExpression).getQAtom() : null;
  }

  @Nullable
  public static String getCallbackSpecName(@NotNull ErlangCallbackSpec spec) {
    ErlangQAtom atom = getCallbackAtom(spec);
    return atom != null ? atom.getText() : null;
  }

  public static int getCallbackSpecArity(@NotNull ErlangCallbackSpec spec) {
    ErlangFunTypeSigs sigs = spec.getFunTypeSigs();
    ErlangSpecFun fun = sigs != null ? sigs.getSpecFun() : null;
    Integer arity = fun != null ? getArity(fun) : null;
    return arity != null ? arity : -1;
  }

  @NotNull
  public static String getExportFunctionName(@NotNull ErlangExportFunction exportFunction) {
    return getNameIdentifier(exportFunction.getQAtom()).getText();
  }

  @Nullable
  private static ErlangQAtom getCallbackAtom(@NotNull ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = getFunTypeSigs(spec);
    ErlangSpecFun specFun = funTypeSigs != null ? funTypeSigs.getSpecFun() : null;
    return specFun != null ? specFun.getQAtom() : null;
  }

  @Nullable
  private static ErlangFunTypeSigs getFunTypeSigs(@NotNull ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = spec.getFunTypeSigs();
    if (funTypeSigs == null) {
      ErlangFunTypeSigsBraces braces = spec.getFunTypeSigsBraces();
      funTypeSigs = braces != null ? braces.getFunTypeSigs() : null;
    }
    return funTypeSigs;
  }

  @NotNull
  public static List<ErlangType> getCallBackSpecArguments(@NotNull ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = getFunTypeSigs(spec);
    List<ErlangTypeSig> typeSigList = funTypeSigs != null ? funTypeSigs.getTypeSigList() : ContainerUtil.emptyList();
    ErlangTypeSig typeSig = ContainerUtil.getFirstItem(typeSigList);
    ErlangFunType funType = typeSig != null ? typeSig.getFunType() : null;
    ErlangFunTypeArguments arguments = funType != null ? funType.getFunTypeArguments() : null;
    return arguments != null ? arguments.getTypeList() : ContainerUtil.emptyList();
  }

  public static boolean isPrivateFunction(@NotNull PsiFile containingFile, @NotNull ErlangFunction function) {
    boolean exportAll = containingFile instanceof ErlangFile && ((ErlangFile) containingFile).isExportedAll();
    if (exportAll) return false;
    return containingFile instanceof ErlangFile && !((ErlangFile) containingFile).getExportedFunctions().contains(function);
  }

  public static int getExpressionPrecedence(@Nullable PsiElement element) {
    if (element instanceof ErlangCatchExpression) return 0;
    if (element instanceof ErlangAssignmentExpression) return 1;
    if (element instanceof ErlangSendExpression) return 2;
    if (element instanceof ErlangOrelseExpression) return 3;
    if (element instanceof ErlangAndalsoExpression) return 4;
    if (element instanceof ErlangCompOpExpression) return 5;
    if (element instanceof ErlangListOpExpression) return 6;
    if (element instanceof ErlangAdditiveExpression) return 7;
    if (element instanceof ErlangMultiplicativeExpression) return 8;
    if (element instanceof ErlangPrefixExpression) return 9;
    if (element instanceof ErlangColonQualifiedExpression) return 10;
    if (element instanceof ErlangFunctionCallExpression
      || element instanceof ErlangGlobalFunctionCallExpression
      || element instanceof ErlangGenericFunctionCallExpression
      || element instanceof ErlangAnonymousCallExpression
      || element instanceof ErlangRecordExpression
      || element instanceof ErlangQualifiedExpression) return 11;
    if (element instanceof ErlangMaxExpression
      || element instanceof ErlangTupleExpression
      || element instanceof ErlangListExpression
      || element instanceof ErlangCaseExpression
      || element instanceof ErlangIfExpression
      || element instanceof ErlangListComprehension
      || element instanceof ErlangReceiveExpression
      || element instanceof ErlangFunExpression
      || element instanceof ErlangTryExpression
      || element instanceof ErlangBinaryExpression
      || element instanceof ErlangBeginEndExpression
      ) return 12;
    if (element instanceof ErlangParenthesizedExpression) return 13;
    return -1;
  }

  @NotNull
  public static ErlangExpression getOutermostParenthesizedExpression(@NotNull ErlangExpression expression) {
    while (expression.getParent() instanceof ErlangParenthesizedExpression) {
      ErlangParenthesizedExpression parent = (ErlangParenthesizedExpression) expression.getParent();
      if (!expression.isEquivalentTo(parent.getExpression())) break;
      expression = parent;
    }

    return expression;
  }

  @Nullable
  public static ErlangExpression getNotParenthesizedExpression(@Nullable ErlangExpression expression) {
    while (expression instanceof ErlangParenthesizedExpression) {
      expression = ((ErlangParenthesizedExpression) expression).getExpression();
    }
    return expression;
  }

  @NotNull
  public static ErlangExpression wrapWithParentheses(@NotNull ErlangExpression expression) {
    return ErlangElementFactory.createExpressionFromText(expression.getProject(), "(" + expression.getText() + ")");
  }

  @Nullable
  public static ErlangFunExpression findFunExpression(@NotNull ErlangFunction function, final int funExpressionNumber) {
    final Ref<ErlangFunExpression> result = new Ref<>();
    final Ref<Integer> currentFunExpressionNumber = new Ref<>(0);
    function.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitFunExpression(@NotNull ErlangFunExpression funExpression) {
        if (funExpressionNumber == currentFunExpressionNumber.get()) {
          result.set(funExpression);
        }
        currentFunExpressionNumber.set(currentFunExpressionNumber.get() + 1);
        super.visitFunExpression(funExpression);
      }
    });
    return result.get();
  }

  public static PsiElementPattern.Capture<ErlangQAtom> secondAtomInIsRecord() {
    return psiElement(ErlangQAtom.class).with(inIsRecord(1));
  }

  @NotNull
  public static <T extends PsiElement >ErlangFunctionCallArgument<T> inIsRecord(int position) {
    return new ErlangFunctionCallArgument<>("erlang", "is_record", 2, position);
  }

  public static boolean isExported(@NotNull ErlangFunction o) {
    ErlangFunctionStub stub = o.getStub();
    if (stub != null) return stub.isExported();

    PsiFile file = o.getContainingFile();
    String signature = o.getName() + "/" + o.getArity();
    return file instanceof ErlangFile && ((ErlangFile) file).isExported(signature);
  }

  public static boolean isOptional(@NotNull ErlangCallbackSpec spec) {
    ErlangCallbackSpecStub stub = spec.getStub();
    if (stub != null) return stub.isOptional();

    String specName = getCallbackSpecName(spec);
    if (specName == null) {
      return false;
    }

    ErlangFile file = ObjectUtils.assertNotNull(ObjectUtils.tryCast(spec.getContainingFile(), ErlangFile.class));
    int specArity = getCallBackSpecArguments(spec).size();

    for (ErlangCallbackFunction callback : file.getOptionalCallbacks()) {
      ErlangAtom functionNameAtom = callback.getQAtom().getAtom();
      String functionName = functionNameAtom != null ? getName(functionNameAtom) : null;
      if (specName.equals(functionName) && specArity == getArity(callback.getInteger())) {
        return true;
      }
    }
    return false;
  }

  @Nullable
  public static ErlangIncludeString getIncludeStringSafe(@NotNull ErlangInclude o) {
    ErlangIncludeStub stub = o.getStub();
    if (stub != null) return stub.getIncludeString();
    return o.getIncludeString();
  }

  @Nullable
  public static ErlangIncludeString getIncludeStringSafe(@NotNull ErlangIncludeLib o) {
    ErlangIncludeLibStub stub = o.getStub();
    if (stub != null) return stub.getIncludeString();
    return o.getIncludeString();
  }

  @NotNull
  public static ErlangFunctionReferenceImpl createFunctionReference(@NotNull ErlangQAtom atom) {
    ErlangFunctionCallExpression call = PsiTreeUtil.getParentOfType(atom, ErlangFunctionCallExpression.class);
    if (call != null) {
      int position = getPositionInFunctionCall(call, atom);
      List<ErlangExpression> expressions = call.getArgumentList().getExpressionList();
      if (position > 0 && position + 1 < expressions.size()) {
        ErlangMaxExpression module = ObjectUtils.tryCast(expressions.get(position - 1), ErlangMaxExpression.class);
        ErlangListExpression list = ObjectUtils.tryCast(expressions.get(position + 1), ErlangListExpression.class);
        ErlangQAtom moduleAtom = module != null ? module.getQAtom() : null;
        int arity = list != null ? list.getExpressionList().size() : -1;
        return new ErlangFunctionReferenceImpl(atom, moduleAtom, arity);
      }
    }
    return new ErlangFunctionReferenceImpl(atom, null, -1);
  }

  public static boolean isWhitespaceOrComment(@NotNull PsiElement element) {
    return isWhitespaceOrComment(element.getNode());
  }

  public static boolean isWhitespaceOrComment(@NotNull ASTNode node) {
    IElementType elementType = node.getElementType();
    return ErlangParserDefinition.WS.contains(elementType) ||
      ErlangParserDefinition.COMMENTS.contains(elementType);
  }

  public static boolean is(@Nullable PsiElement element, IElementType type) {
    return element != null && element.getNode().getElementType() == type;
  }

  @NotNull
  public static TextRange getTextRangeForReference(@NotNull ErlangQAtom qAtom) {
    return rangeInParent(qAtom.getTextRange(), getNameIdentifier(qAtom).getTextRange());
  }

  @NotNull
  public static TextRange getTextRangeForReference(@NotNull ErlangMacrosName macroName) {
    return rangeInParent(macroName.getTextRange(), getNameIdentifier(macroName).getTextRange());
  }

  @NotNull
  private static TextRange rangeInParent(@NotNull TextRange parent, @NotNull TextRange child) {
    int start = child.getStartOffset() - parent.getStartOffset();
    return TextRange.create(start, start + child.getLength());
  }

  @NotNull
  private static String getNameImpl(@NotNull ErlangNamedElement namedElement) {
    if (namedElement instanceof StubBasedPsiElement) {
      String fromStub = getNameFromStub((StubBasedPsiElement)namedElement);
      if (fromStub != null) return fromStub;
    }
    PsiElement nameIdentifier = namedElement.getNameIdentifier();
    return nameIdentifier != null ? nameIdentifier.getText() : "";
  }

  @Nullable
  private static String getNameFromStub(@NotNull StubBasedPsiElement element) {
    NamedStubBase<?> stub = ObjectUtils.tryCast(element.getStub(), NamedStubBase.class);
    return stub != null ? StringUtil.notNullize(stub.getName()) : null;
  }

  public static boolean fromTheSameCaseExpression(@NotNull PsiElement origin, @NotNull PsiElement element) {
    if (element instanceof ErlangQVar && Comparing.equal(element.getText(), element.getText())) {
      ErlangCompositeElement cr2 = PsiTreeUtil.getParentOfType(element, ErlangCrClause.class);
      ErlangCompositeElement cr1 = PsiTreeUtil.getParentOfType(origin, ErlangCrClause.class);
      if (cr1 != null && cr2 != null) {
        ErlangCaseExpression ce1 = PsiTreeUtil.getParentOfType(element, ErlangCaseExpression.class);
        ErlangCaseExpression ce2 = PsiTreeUtil.getParentOfType(origin, ErlangCaseExpression.class);
        if (Comparing.equal(ce1, ce2)) return true;
      }
    }
    return false;
  }

  public static class ErlangFunctionCallArgumentBase<T extends PsiElement> extends PatternCondition<T> {
    protected ErlangFunctionCallArgumentBase(@Nullable String debugMethodName) {
      super(debugMethodName);
    }

    @Override
    public boolean accepts(@NotNull T element, ProcessingContext context) {
      ErlangFunctionCallExpression call = PsiTreeUtil.getParentOfType(element, ErlangFunctionCallExpression.class);
      int position = call != null ? getPositionInFunctionCall(call, element) : -1;
      return call != null && position != -1 && acceptsArgument(call, position);
    }

    protected boolean acceptsArgument(@NotNull ErlangFunctionCallExpression call, int position) {
      return true;
    }
  }

  public static class ErlangFunctionCallQAtomArgument extends ErlangFunctionCallArgumentBase<ErlangQAtom> {
    protected ErlangFunctionCallQAtomArgument(@Nullable String debugMethodName) {
      super(debugMethodName);
    }

    @Override
    protected final boolean acceptsArgument(@NotNull ErlangFunctionCallExpression call, int position) {
      ErlangExpression arg = call.getArgumentList().getExpressionList().get(position);
      return arg instanceof ErlangMaxExpression &&
             ((ErlangMaxExpression) arg).getQAtom() != null &&
             acceptsAtomArgument(call, position);
    }

    protected boolean acceptsAtomArgument(@NotNull ErlangFunctionCallExpression call, int position) {
      return true;
    }
  }

  public static class ErlangFunctionCallArgument<T extends PsiElement> extends ErlangFunctionCallArgumentBase<T> {
    @NotNull
    private final String myFunName;
    @NotNull
    private final String myModule;
    private final int myArity;
    private final int myPosition;

    public ErlangFunctionCallArgument(@NotNull String module, @NotNull String funName, int arity, int position) {
      super("functionCallArgument");
      myFunName = funName;
      myModule = module;
      myArity = arity;
      myPosition = position;
    }

    @Override
    public boolean acceptsArgument(@NotNull ErlangFunctionCallExpression call, int position) {
      if (position != myPosition) return false;

      ErlangFunctionReferenceImpl reference = (ErlangFunctionReferenceImpl)call.getReference();
      ResolveResult[] variants = reference != null ? reference.multiResolve(true) : ResolveResult.EMPTY_ARRAY;
      for (ResolveResult variant : variants) {
        if (isExpectedFunction(ObjectUtils.tryCast(variant.getElement(), ErlangFunction.class))) {
          return true;
        }
      }
      return false;
    }

    private boolean isExpectedFunction(@Nullable ErlangFunction function) {
      PsiFile file = function != null ? function.getContainingFile() : null;
      ErlangFile containingFile = ObjectUtils.tryCast(file, ErlangFile.class);
      ErlangModule module = containingFile != null ? containingFile.getModule() : null;
      String moduleName = module != null ? module.getName() : null;
      return myModule.equals(moduleName) && myFunName.equals(function.getName()) && myArity == getArity(function);
    }
  }

  public static class ErlangFunctionCallModuleArgument extends ErlangFunctionCallQAtomArgument {
    public ErlangFunctionCallModuleArgument() {
      super("functionCallModuleArgument");
    }

    @Override
    public boolean acceptsAtomArgument(@NotNull ErlangFunctionCallExpression call, int position) {
      return isModuleParameter(call, position);
    }

    static boolean isModuleParameter(@NotNull ErlangFunctionCallExpression call, int position) {
      ErlangFunTypeSigs signature = getSignature(findSpecification(resolveToFunction(call)));
      if (signature == null) return false;

      for (ErlangTypeSig typeSig : signature.getTypeSigList()) {
        List<ErlangType> types = typeSig.getFunType().getFunTypeArguments().getTypeList();
        if (types.size() <= position) continue;
        ErlangType type = types.get(position);
        ErlangQVar typeVar = type.getQVar();
        if (typeVar != null) type = getType(typeVar, typeSig.getTypeSigGuard());
        if (isModule(type)) return true;
      }
      return false;
    }

    @Nullable
    private static ErlangType getType(@NotNull ErlangQVar typeVar, @Nullable ErlangTypeSigGuard sigGuard) {
      if (sigGuard == null) return null;
      for (ErlangTypeGuard guard : sigGuard.getTypeGuardList()) {
        for (ErlangType topType : guard.getTypeList()) {
          ErlangQVar var = topType.getQVar();
          if (var != null && typeVar.getName().equals(var.getName()) && topType.getType() != null) {
            return topType.getType();
          }
        }
      }
      return null;
    }

    private static boolean isModule(@Nullable ErlangType type) {
      ErlangTypeRef ref = type != null ? type.getTypeRef() : null;
      ErlangAtom atom = ref != null ? ref.getQAtom().getAtom() : null;
      return atom != null && "module".equals(atom.getName());
    }
  }

  public static class ErlangFunctionCallFunctionArgument extends ErlangFunctionCallQAtomArgument {
    public ErlangFunctionCallFunctionArgument() {
      super("functionCallFunctionArgument");
    }

    @Override
    public boolean acceptsAtomArgument(@NotNull ErlangFunctionCallExpression call, int position) {
      int moduleArgPosition = position - 1;
      List<ErlangExpression> expressions = call.getArgumentList().getExpressionList();
      ErlangExpression moduleArg = position < 1 ? null : expressions.get(moduleArgPosition);
      ErlangMaxExpression moduleExpr = ObjectUtils.tryCast(moduleArg, ErlangMaxExpression.class);
      ErlangQAtom moduleAtom = moduleExpr != null ? moduleExpr.getQAtom() : null;
      return moduleAtom != null && ErlangFunctionCallModuleArgument.isModuleParameter(call, moduleArgPosition);
    }
  }
}