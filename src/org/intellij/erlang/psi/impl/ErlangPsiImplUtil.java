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
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.*;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.patterns.PatternCondition;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.*;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.ResolveScopeManager;
import com.intellij.psi.impl.source.resolve.reference.ReferenceProvidersRegistry;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.Function;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.*;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.rebar.util.RebarConfigUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.*;

import static com.intellij.patterns.PlatformPatterns.psiElement;

public class ErlangPsiImplUtil {
  public static final Set<String> KNOWN_MACROS = ContainerUtil.set("MODULE", "MODULE_NAME", "FILE", "LINE", "MACHINE");
  public static final Set<String> BUILT_IN_TYPES = ContainerUtil.set(
    "any", "atom", "boolean", "byte", "char", "float", "integer", "iolist", "list", "maybe_improper_list", "mfa",
    "module", "neg_integer", "no_return", "node", "non_neg_integer", "none", "nonempty_string", "number", "pid", "port",
    "pos_integer", "ref", "string", "term", "timeout"
  );
  public static final Key<LanguageConsoleImpl> ERLANG_CONSOLE = Key.create("ERLANG_CONSOLE");

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
  public static PsiReference getReference(@NotNull ErlangQAtom o) {
    return ArrayUtil.getFirstElement(ReferenceProvidersRegistry.getReferencesFromProviders(o));
  }

  public static Pair<List<ErlangTypedExpr>, List<ErlangQAtom>> getRecordFields(PsiElement element) {
    List<ErlangTypedExpr> result = new ArrayList<ErlangTypedExpr>(0);
    List<ErlangQAtom> atoms = new ArrayList<ErlangQAtom>(0);
    ErlangRecordExpression recordExpression = PsiTreeUtil.getParentOfType(element, ErlangRecordExpression.class);
    PsiReference reference = recordExpression != null ? recordExpression.getReference() : null;
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
  private static void processRecordFields(ErlangMacros macros, List<ErlangQAtom> atoms) {
    PsiReference psiReference = macros.getReference();
    PsiElement macrosDefinition = psiReference != null ? psiReference.resolve() : null;
    if (macrosDefinition instanceof ErlangMacrosDefinition) {
      ErlangMacrosBody macrosBody = ((ErlangMacrosDefinition) macrosDefinition).getMacrosBody();
      List<ErlangExpression> expressionList = macrosBody != null ? macrosBody.getExpressionList() : ContainerUtil.<ErlangExpression>emptyList();
      for (ErlangExpression ee : expressionList) {
        if (ee instanceof ErlangMaxExpression){
          ErlangQAtom qAtom = ((ErlangMaxExpression) ee).getQAtom();
          ContainerUtil.addIfNotNull(atoms, qAtom);
        }
        else if (ee instanceof ErlangAssignmentExpression) {
          ErlangExpression left = ((ErlangAssignmentExpression) ee).getLeft();
          if (left instanceof ErlangMaxExpression){
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
    return new ErlangAtomBasedReferenceImpl<ErlangQAtom>(atom, TextRange.from(0, atom.getTextLength()), atom.getText()) {
      @Override
      public PsiElement resolve() {
        Pair<List<ErlangTypedExpr>, List<ErlangQAtom>> recordFields = getRecordFields(myElement);
        for (ErlangTypedExpr field : recordFields.first) {
          if (field.getName().equals(myReferenceName)) return field;
        }
        for (ErlangQAtom qAtom : recordFields.second) {
          PsiElement aa = qAtom.getAtom();
          if (aa != null) {
            if (myReferenceName.equals(aa.getText())) return qAtom;
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
  public static PsiReference getReference(@NotNull final ErlangIncludeString o) {
    final PsiElement parent = o.getParent();
    if (o.getTextLength() >= 2) {
      return new PsiReferenceBase<PsiElement>(o, TextRange.from(1, o.getTextLength() - 2)) {
        @Override
        public PsiElement resolve() {
          List<ErlangFile> files = parent instanceof ErlangInclude ? getDirectlyIncludedFiles((ErlangInclude) parent) : getDirectlyIncludedFiles((ErlangIncludeLib) parent);
          return ContainerUtil.getFirstItem(files);
        }

        @Override
        public PsiElement handleElementRename(String newName) throws IncorrectOperationException {
          PsiElement resolve = resolve();
          if (resolve instanceof ErlangFile) {
            PsiElement st;
            try {
              String fileName = ((ErlangFile) resolve).getName();
              String newIncludeString = StringUtil.unquoteString(o.getString().getText()).replace(fileName, newName);
              st = ErlangElementFactory.createStringFromText(o.getProject(), newIncludeString);
            } catch (Exception e) {
              st = null;
            }

            if (st != null) {
              o.getString().replace(st);
            }
          }
          return o;
        }

        @NotNull
        @Override
        public Object[] getVariants() {
          return ArrayUtil.EMPTY_OBJECT_ARRAY;
        }
      };
    }
    return null;
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

    return new ErlangFunctionReferenceImpl<ErlangQAtom>(
      nameAtom, moduleAtom, TextRange.from(0, nameAtom.getTextLength()),
      nameAtom.getText(), o.getArgumentList().getExpressionList().size());
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
    return new ErlangFunctionReferenceImpl<ErlangQAtom>(nameAtom, isModule ? null : moduleAtom, TextRange.from(0, nameAtom.getTextLength()),
      nameAtom.getText(), getArity(arity));
  }

  private static boolean isModule(@Nullable ErlangModuleRef moduleReference) {
    if (moduleReference == null) return false;
    return moduleReference.getQAtom().getText().equals("?MODULE");
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangExportFunction o) {
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl<ErlangQAtom>(o.getQAtom(), null, TextRange.from(0, o.getQAtom().getTextLength()),
      o.getQAtom().getText(), getArity(arity));
  }

  @NotNull
  public static PsiReference getReference(@NotNull ErlangImportFunction o) {
    ErlangImportDirective importDirective = PsiTreeUtil.getParentOfType(o, ErlangImportDirective.class);
    ErlangModuleRef moduleRef = importDirective != null ? importDirective.getModuleRef() : null;
    ErlangQAtom moduleRefQAtom = moduleRef != null ? moduleRef.getQAtom() : null;
    PsiElement arity = o.getInteger();
    return new ErlangFunctionReferenceImpl<ErlangQAtom>(o.getQAtom(), moduleRefQAtom, TextRange.from(0, o.getQAtom().getTextLength()), o.getQAtom().getText(), getArity(arity));
  }

  public static int getArity(@Nullable PsiElement arity) {
    return StringUtil.parseInt(arity == null ? "" : arity.getText(), -1);
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangMacros o) {
    ErlangMacrosName macrosName = o.getMacrosName();
    if (macrosName == null) return null;
    return new ErlangMacrosReferenceImpl<ErlangMacrosName>(macrosName, TextRange.from(0, macrosName.getTextLength()), macrosName.getText());
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangTypeRef o) {
    return getModuleReference(o, o.getQAtom());
  }

  @NotNull
  private static PsiReference getModuleReference(ErlangCompositeElement o, ErlangQAtom atom) {
    ErlangModuleRef moduleRef = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    return new ErlangTypeReferenceImpl<ErlangQAtom>(atom, moduleRef, TextRange.from(0, atom.getTextLength()), atom.getText());
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

  public static boolean inDefine(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangMacrosDefinition.class) != null;
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
    ErlangAssignmentExpression assignment = PsiTreeUtil.getParentOfType(psiElement, ErlangAssignmentExpression.class);
    if (assignment == null) return false;
    PsiElement parent = assignment.getParent();
    ErlangAssignmentExpression topAssignment = parent instanceof ErlangTupleExpression || parent instanceof ErlangListExpression ?
      PsiTreeUtil.getParentOfType(parent, ErlangAssignmentExpression.class) : null;
    assignment = topAssignment != null ? topAssignment : assignment;
    return PsiTreeUtil.isAncestor(assignment.getLeft(), psiElement, strict);
  }

  public static boolean isMacros(ErlangQVar o) {
    return o.getName().startsWith("?");
  }

  public static boolean isForceSkipped(ErlangQVar o) {
    return o.getName().startsWith("_");
  }

  @NotNull
  public static List<LookupElement> getFunctionLookupElements(@NotNull PsiFile containingFile, final boolean withArity, @Nullable ErlangQAtom qAtom) {
    if (containingFile instanceof ErlangFile && !ErlangParserUtil.isApplicationConfigFileType(containingFile)) {
      List<ErlangFunction> functions = new ArrayList<ErlangFunction>();

      List<LookupElement> lookupElements = ContainerUtil.newArrayList();

      Module module = ModuleUtilCore.findModuleForPsiElement(containingFile);
      Sdk sdk = module == null ? null : ModuleRootManager.getInstance(module).getSdk();
      ErlangSdkRelease release = sdk != null ? ErlangSdkType.getRelease(sdk) : null;
      if (qAtom != null) {
        String moduleName = StringUtil.unquoteString(qAtom.getText());
        functions.addAll(getExternalFunctionForCompletion(containingFile.getProject(), moduleName + ".erl"));

        if (release == null || release.needBifCompletion(moduleName)) {
          for (ErlangBifDescriptor bif : ErlangBifTable.getBifs(moduleName)) {
            lookupElements.add(createFunctionLookupElement(bif.getName(), bif.getArity(), withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY));
          }
        }
      }
      else {
        ErlangFile erlangFile = (ErlangFile) containingFile;
        functions.addAll(erlangFile.getFunctions());
        functions.addAll(getExternalFunctionForCompletion(containingFile.getProject(), "erlang.erl"));

        for (ErlangImportFunction importFunction : erlangFile.getImportedFunctions()) {
          lookupElements.add(createFunctionLookupElement(importFunction.getQAtom().getText(), getArity(importFunction.getInteger()), withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY));
        }

        if (!withArity && (release == null || release.needBifCompletion("erlang"))) {
          for (ErlangBifDescriptor bif : ErlangBifTable.getBifs("erlang")) {
            lookupElements.add(createFunctionLookupElement(bif.getName(), bif.getArity(), false, ErlangCompletionContributor.BIF_PRIORITY));
          }
        }
      }

      functions.addAll(getErlangFunctionsFromIncludes((ErlangFile) containingFile, true, "", 0));
      lookupElements.addAll(createFunctionLookupElements(functions, withArity));
      return lookupElements;
    }
    return Collections.emptyList();
  }

  public static List<LookupElement> createFunctionLookupElements(List<ErlangFunction> functions, final boolean withArity) {
    return ContainerUtil.map(functions, new Function<ErlangFunction, LookupElement>() {
      @Override
      public LookupElement fun(@NotNull final ErlangFunction function) {
        return createFunctionsLookupElement(function, withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY);
      }
    });
  }

  private static LookupElement createFunctionsLookupElement(ErlangFunction function, boolean withArity, double priority) {
    int arity = function.getArity();
    return PrioritizedLookupElement.withPriority(LookupElementBuilder.create(function)
      .withIcon(ErlangIcons.FUNCTION).withTailText("/" + arity)
      .withInsertHandler(getInsertHandler(arity, withArity)), priority);
  }

  private static LookupElement createFunctionLookupElement(@NotNull String name, int arity, boolean withArity, int priority) {
    return PrioritizedLookupElement.withPriority(LookupElementBuilder.create(name)
      .withIcon(ErlangIcons.FUNCTION).withTailText("/" + arity)
      .withInsertHandler(getInsertHandler(arity, withArity)), (double) priority);
  }

  private static InsertHandler<LookupElement> getInsertHandler(final int arity, boolean withArity) {
    return withArity ?
      new BasicInsertHandler<LookupElement>() {
        @Override
        public void handleInsert(InsertionContext context, LookupElement item) {
          final Editor editor = context.getEditor();
          final Document document = editor.getDocument();
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
        private PsiElement findNextToken(final InsertionContext context) {
          final PsiFile file = context.getFile();
          PsiElement element = file.findElementAt(context.getTailOffset());
          if (element instanceof PsiWhiteSpace) {
            element = file.findElementAt(element.getTextRange().getEndOffset());
          }
          return element;
        }

      } :
      new ParenthesesInsertHandler<LookupElement>() {
        @Override
        protected boolean placeCaretInsideParentheses(InsertionContext context, LookupElement item) {
          return arity > 0;
        }
      };
  }

  @NotNull
  public static List<LookupElement> getMacrosLookupElements(@NotNull PsiFile containingFile) {
    if (containingFile instanceof ErlangFile) {
      List<ErlangMacrosDefinition> concat = ContainerUtil.concat(((ErlangFile) containingFile).getMacroses(), getErlangMacrosesFromIncludes((ErlangFile) containingFile, true, ""));
      List<LookupElement> fromFile = ContainerUtil.map(
        concat,
        new Function<ErlangMacrosDefinition, LookupElement>() {
          @Override
          public LookupElement fun(@NotNull ErlangMacrosDefinition md) {
            return LookupElementBuilder.create(md).withIcon(ErlangIcons.MACROS);
          }
        });
      List<LookupElement> stdMacros = new ArrayList<LookupElement>();
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
        new Function<ErlangRecordDefinition, LookupElement>() {
          @Override
          public LookupElement fun(@NotNull ErlangRecordDefinition rd) {
            return LookupElementBuilder.create(rd).withIcon(ErlangIcons.RECORD);
          }
        });
    }
    return Collections.emptyList();
  }

  @NotNull
  public static List<LookupElement> getTypeLookupElements(@NotNull PsiFile containingFile, boolean addBuiltInTypes, final boolean withArity) {
    if (containingFile instanceof ErlangFile) {
      List<ErlangTypeDefinition> types = ((ErlangFile) containingFile).getTypes();

      final ParenthesesInsertHandler<LookupElement> handler = new ParenthesesInsertHandler<LookupElement>() {
        @Override
        protected boolean placeCaretInsideParentheses(InsertionContext context, LookupElement item) {
          return false;
        }
      };

      List<LookupElement> builtInTypes = addBuiltInTypes ? ContainerUtil.map(BUILT_IN_TYPES, new Function<String, LookupElement>() {
        @Override
        public LookupElement fun(String s) {
          return LookupElementBuilder.create(s).withIcon(ErlangIcons.TYPE).withInsertHandler(handler);
        }
      }) : ContainerUtil.<LookupElement>emptyList();

      List<LookupElement> foundedTypes = ContainerUtil.map(
        types,
        new Function<ErlangTypeDefinition, LookupElement>() {
          @Override
          public LookupElement fun(@NotNull final ErlangTypeDefinition rd) {
            return LookupElementBuilder.create(rd).withIcon(ErlangIcons.TYPE).withInsertHandler(getInsertHandler(calculateTypeArity(rd), withArity));
          }
        });
      return ContainerUtil.concat(foundedTypes, builtInTypes);
    }
    return Collections.emptyList();
  }

  private static int calculateTypeArity(@NotNull ErlangTypeDefinition rd) {
    ErlangArgumentDefinitionList argumentDefinitionList = rd.getArgumentDefinitionList();
    if (argumentDefinitionList == null) return 0;
    return argumentDefinitionList.getArgumentDefinitionList().size();
  }

  private static int calculateFunctionClauseArity(@NotNull ErlangFunctionClause clause) {
    ErlangArgumentDefinitionList argumentDefinitionList = clause.getArgumentDefinitionList();
    return argumentDefinitionList.getArgumentDefinitionList().size();
  }

  @NotNull
  public static String getName(@NotNull ErlangFunction o) {
    PsiElement atom = o.getAtomName().getAtom();
    if (atom != null) {
      return atom.getText();
    }
    //noinspection ConstantConditions
    return o.getAtomName().getMacros().getText();
  }

  @NotNull
  public static String getName(@NotNull ErlangQVar o) {
    return o.getText();
  }

  public static int getArity(@NotNull ErlangFunction o) {
    return o.getFunctionClauseList().get(0).getArgumentDefinitionList().getArgumentDefinitionList().size();
  }

  @NotNull
  public static String getName(@NotNull ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    if (atom == null) return "";
    return atom.getText();
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangRecordDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    return atom != null ? atom : o;
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
  public static PsiElement getNameIdentifier(@NotNull ErlangFunction o) {
    return o.getAtomName();
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangRecordExpression o) {
    ErlangRecordRef recordRef = o.getRecordRef();
    return recordRef != null ? recordRef.getReference() : null;
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangRecordRef o) {
    return createRecordRef(o.getQAtom());
  }

  public static ErlangRecordReferenceImpl<ErlangQAtom> createRecordRef(@NotNull ErlangQAtom atom) {
    return new ErlangRecordReferenceImpl<ErlangQAtom>(atom, TextRange.from(0, atom.getMacros() == null ? atom.getTextLength() : 1), atom.getText());
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangModuleRef o) {
    ErlangQAtom atom = o.getQAtom();
    String name = StringUtil.unquoteString(atom.getText());
    return new ErlangModuleReferenceImpl<ErlangQAtom>(atom, TextRange.from(0, atom.getTextLength()), name);
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangFunction o, @NotNull String newName) {
    for (ErlangFunctionClause clause : o.getFunctionClauseList()) {
      PsiElement atom = clause.getQAtom().getAtom();
      if (atom != null) {
        atom.replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
      }
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
    ErlangQAtom qAtom = o.getQAtom();
    if (qAtom != null) {
      PsiElement atom = qAtom.getAtom();
      if (atom != null) {
        atom.replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
      }
    }
    return o;
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangTypeDefinition o, @NotNull String newName) {
    ErlangQAtom qAtom = o.getQAtom();
    if (qAtom != null) {
      PsiElement atom = qAtom.getAtom();
      if (atom != null) {
        atom.replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
      }
    }
    return o;
  }

  @NotNull
  public static String getName(@NotNull ErlangModule o) {
    ErlangQAtom atom = o.getQAtom();
    return atom == null ? "" : atom.getText();
  }

  @NotNull
  public static PsiElement setName(@NotNull ErlangModule o, String newName) {
    VirtualFile virtualFile = o.getContainingFile().getVirtualFile();
    if (virtualFile != null) {
      try {
        String ext = FileUtilRt.getExtension(virtualFile.getName());
        virtualFile.rename(o, StringUtil.replace(newName, "'", "") + "." + ext);

        ErlangQAtom qAtom = o.getQAtom();
        if (qAtom != null) {
          PsiElement atom = qAtom.getAtom();
          if (atom != null) {
            atom.replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
          }
        }
      } catch (Exception ignored) {
      }
    }
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(@NotNull ErlangModule o) {
    ErlangQAtom atom = o.getQAtom();
    return atom == null ? o : atom;
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
    return o.getQAtom();
  }

  public static int getTextOffset(@NotNull ErlangFunctionCallExpression o) {
    return o.getQAtom().getTextOffset();
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangListComprehension o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processDeclarationRecursive(o, processor, state);
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean processDeclarations(@NotNull ErlangModule o, @NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent, @NotNull PsiElement place) {
    return processDeclarationRecursive(o, processor, state);
  }

  private static boolean processDeclarationRecursive(ErlangCompositeElement o, PsiScopeProcessor processor, ResolveState state) {
    Queue<ErlangCompositeElement> queue = new LinkedList<ErlangCompositeElement>();
    queue.add(o);
    while (!queue.isEmpty()) {
      ErlangCompositeElement top = queue.remove();
      if (!processor.execute(top, state)) return false;
      queue.addAll(PsiTreeUtil.getChildrenOfTypeAsList(top, ErlangCompositeElement.class));
    }
    return true;
  }

  @Nullable
  public static ErlangModule getModule(PsiFile file) {
    if (file instanceof ErlangFile) {
      List<ErlangAttribute> attributes = PsiTreeUtil.getChildrenOfTypeAsList(file, ErlangAttribute.class);
      for (ErlangAttribute attribute : attributes) {
        ErlangModule module = attribute.getModule();
        if (module != null) {
          return module;
        }
      }
    }
    return null;
  }

  static boolean inModule(PsiElement psiElement) {
    return PsiTreeUtil.getParentOfType(psiElement, ErlangModule.class) != null;
  }

  @NotNull
  public static Collection<ErlangFile> getIncludedFiles(@NotNull ErlangFile file) {
    HashSet<ErlangFile> includedFiles = new HashSet<ErlangFile>();
    addIncludedFiles(file, includedFiles);
    return includedFiles;
  }

  private static void addIncludedFiles(@NotNull ErlangFile erlangFile, Set<ErlangFile> alreadyAdded) {
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
    List<ErlangFile> files = new ArrayList<ErlangFile>();
    for (ErlangInclude include : erlangFile.getIncludes()) {
      files.addAll(getDirectlyIncludedFiles(include));
    }
    for (ErlangIncludeLib includeLib : erlangFile.getIncludeLibs()) {
      files.addAll(getDirectlyIncludedFiles(includeLib));
    }
    return files;
  }

  @NotNull
  public static List<ErlangFile> getDirectlyIncludedFiles(@NotNull ErlangIncludeLib includeLib) {
    ErlangIncludeString includeString = includeLib.getIncludeString();
    String[] split = includeString != null ? StringUtil.unquoteString(includeString.getText()).split("/") : null;

    if (split != null && split.length >= 2) {
      String libName = split[0];
      final String relativePath = StringUtil.join(split, 1, split.length, "/");
      final Project project = includeLib.getProject();
      VirtualFile appDir = ErlangApplicationIndex.getApplicationDirectoryByName(libName, GlobalSearchScope.allScope(project));
      ErlangFile includedFile = getRelativeErlangFile(project, relativePath, appDir);
      if (includedFile != null) {
        return ContainerUtil.newSmartList(includedFile);
      }
    }
    //either include_lib does not specify a library, or it was not found, falling back to 'include' behaviour.
    return getDirectlyIncludedFiles(includeString);
  }

  @NotNull
  public static List<ErlangFile> getDirectlyIncludedFiles(@NotNull ErlangInclude include) {
    ErlangIncludeString includeString = include.getIncludeString();
    return getDirectlyIncludedFiles(includeString);
  }

  @NotNull
  public static List<ErlangFile> getDirectlyIncludedFiles(@Nullable ErlangIncludeString includeString) {
    if (includeString == null) return ContainerUtil.emptyList();
    ErlangFile containingPsiFile = (ErlangFile) includeString.getContainingFile();
    VirtualFile containingVirtualFile = containingPsiFile.getOriginalFile().getVirtualFile();
    VirtualFile parent = containingVirtualFile != null ? containingVirtualFile.getParent() : null;
    String relativePath = StringUtil.unquoteString(includeString.getText());
    Project project = containingPsiFile.getProject();
    ErlangFile relativeToDirectParent = getRelativeErlangFile(project, relativePath, parent);
    if (relativeToDirectParent != null) return ContainerUtil.newSmartList(relativeToDirectParent);
    //relative to direct parent include file was not found
    //let's search in include directories
    if (containingVirtualFile != null) {
      Module module = ModuleUtilCore.findModuleForFile(containingVirtualFile, project);
      ErlangFacet erlangFacet = module != null ? ErlangFacet.getFacet(module) : null;
      if (erlangFacet != null) {
        for (String includePath : erlangFacet.getConfiguration().getIncludePaths()) {
          VirtualFile includeDir = LocalFileSystem.getInstance().findFileByPath(includePath);
          ErlangFile includedFile = getRelativeErlangFile(project, relativePath, includeDir);
          if (includedFile != null) return ContainerUtil.newSmartList(includedFile);
        }
      }
    }
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
        VirtualFile includePathVirtualFile = VfsUtil.findRelativeFile(includePath, otpAppRoot);
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
    List<VirtualFile> containingOtpAppRoots = ContainerUtil.filter(allOtpAppRoots, new Condition<VirtualFile>() {
      @Override
      public boolean value(VirtualFile appRoot) {
        return VfsUtilCore.isAncestor(appRoot, file, true);
      }
    });
    //sort it in order to have longest path first
    ContainerUtil.sort(containingOtpAppRoots, new Comparator<VirtualFile>() {
      @Override
      public int compare(VirtualFile o1, VirtualFile o2) {
        return o2.getPath().length() - o1.getPath().length();
      }
    });
    return ContainerUtil.getFirstItem(containingOtpAppRoots);
  }

  @NotNull
  public static Set<String> getImplementedBehaviourModuleNames(@NotNull ErlangFile file) {
    final Set<String> behaviours = new HashSet<String>();
    addDeclaredBehaviourModuleNames(file, behaviours);
    for (ErlangFile erlangFile : getIncludedFiles(file)) {
      addDeclaredBehaviourModuleNames(erlangFile, behaviours);
    }
    return behaviours;
  }

  private static void addDeclaredBehaviourModuleNames(@NotNull ErlangFile file, @NotNull Set<String> behaviourNames) {
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      ContainerUtil.addIfNotNull(getName(behaviour), behaviourNames);
    }
  }

  @NotNull
  public static Set<String> getAppliedParseTransformModuleNames(@NotNull ErlangFile file) {
    Set<String> parseTransforms = new HashSet<String>();
    addDeclaredParseTransforms(file, parseTransforms);
    for (ErlangFile f : getIncludedFiles(file)) {
      addDeclaredParseTransforms(f, parseTransforms);
    }
    return parseTransforms;
  }

  private static void addDeclaredParseTransforms(@NotNull ErlangFile file, @NotNull Set<String> parseTransforms) {
    for (ErlangAttribute attribute : file.getAttributes()) {
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      ErlangQAtom qAtom = null != atomAttribute ? atomAttribute.getQAtom() : null;
      PsiElement atom = null != qAtom ? qAtom.getAtom() : null;
      String attributeName = null != atom ? atom.getText() : null;
      ErlangAttrVal attrVal = atomAttribute != null ? atomAttribute.getAttrVal() : null;
      if (!"compile".equals(attributeName) || attrVal == null) continue;

      for (ErlangExpression expression : attrVal.getExpressionList()) {
        //TODO support macros
        if (expression instanceof ErlangListExpression) {
          extractParseTransforms((ErlangListExpression) expression, parseTransforms);
        }
        if (expression instanceof ErlangTupleExpression) {
          extractParseTransforms((ErlangTupleExpression) expression, parseTransforms);
        }
      }
    }
  }

  private static void extractParseTransforms(@NotNull ErlangListExpression list, @NotNull Set<String> parseTransforms) {
    for (ErlangExpression expr : list.getExpressionList()) {
      if (expr instanceof ErlangTupleExpression) {
        extractParseTransforms((ErlangTupleExpression) expr, parseTransforms);
      }
    }
  }

  private static void extractParseTransforms(@NotNull ErlangTupleExpression tuple, @NotNull Set<String> parseTransforms) {
    List<ErlangExpression> expressionList = tuple.getExpressionList();
    if (expressionList.size() != 2) return;
    ErlangExpression first = expressionList.get(0);
    if (!"parse_transform".equals(getAtomName(first instanceof ErlangMaxExpression ? (ErlangMaxExpression) first : null))) return;
    ErlangExpression second = expressionList.get(1);
    String parseTransformModuleName = getAtomName(second instanceof ErlangMaxExpression ? (ErlangMaxExpression) second : null);
    ContainerUtil.addIfNotNull(parseTransformModuleName, parseTransforms);
  }

  @Nullable
  private static String getAtomName(@Nullable ErlangMaxExpression expression) {
    ErlangQAtom qAtom = expression != null ? expression.getQAtom() : null;
    PsiElement atom = qAtom != null ? qAtom.getAtom() : null;
    return atom != null ? atom.getText() : null;
  }

  @Nullable
  private static ErlangFile getRelativeErlangFile(@NotNull Project project, @NotNull String relativePath, @Nullable VirtualFile parent) {
    VirtualFile relativeFile = VfsUtil.findRelativeFile(relativePath, parent);
    if (relativeFile == null) return null;
    PsiFile file = PsiManager.getInstance(project).findFile(relativeFile);
    return file instanceof ErlangFile ? (ErlangFile) file : null;
  }

  @NotNull
  static List<ErlangRecordDefinition> getErlangRecordFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangRecordDefinition> fromIncludes = new ArrayList<ErlangRecordDefinition>();
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
    List<ErlangFunction> fromIncludes = new ArrayList<ErlangFunction>();
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
  static List<ErlangMacrosDefinition> getErlangMacrosesFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangMacrosDefinition> fromIncludes = new ArrayList<ErlangMacrosDefinition>();
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
  static List<ErlangTypeDefinition> getErlangTypeFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangTypeDefinition> fromIncludes = new ArrayList<ErlangTypeDefinition>();
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
  public static PsiElement getNameIdentifier(ErlangMacrosDefinition o) {
    ErlangMacrosName macrosName = o.getMacrosName();
    if (macrosName == null) return o;
    return macrosName;
  }

  @NotNull
  public static PsiElement getNameIdentifier(ErlangTypeDefinition o) {
    ErlangQAtom atom = o.getQAtom();
    if (atom == null) return o;
    return atom;
  }

  public static int getTextOffset(ErlangTypeDefinition o) {
    if (o.getQAtom() == null) return 0;
    return getNameIdentifier(o).getTextOffset();
  }

  public static int getTextOffset(ErlangMacrosDefinition o) {
    if (o.getMacrosName() == null) return 0;
    return getNameIdentifier(o).getTextOffset();
  }

  @NotNull
  public static String getName(ErlangMacrosDefinition o) {
    return o.getNameIdentifier().getText();
  }

  @NotNull
  public static PsiElement setName(ErlangMacrosDefinition o, String newName) {
    ErlangMacrosName macrosName = o.getMacrosName();
    if (macrosName != null) {
      macrosName.replace(ErlangElementFactory.createMacrosFromText(o.getProject(), newName));
    }
    return o;
  }

  @NotNull
  public static String getName(ErlangBehaviour o) {
    ErlangModuleRef moduleRef = o.getModuleRef();
    ErlangQAtom atom = moduleRef != null ? moduleRef.getQAtom() : null;
    return atom == null ? "" : atom.getText();
  }

  @NotNull
  public static List<ErlangFunction> getExternalFunctionForCompletion(@NotNull Project project, @NotNull String moduleFileName) {
    PsiFile[] files = FilenameIndex.getFilesByName(project, moduleFileName, GlobalSearchScope.allScope(project));
    List<ErlangFunction> result = new ArrayList<ErlangFunction>();
    for (PsiFile file : files) {
      if (file instanceof ErlangFile) {
        result.addAll(((ErlangFile) file).getExportedFunctions());
      }
    }
    return result;
  }

  public static boolean inFunction(PsiElement position) {
    return PsiTreeUtil.getParentOfType(position, ErlangFunction.class) != null;
  }

  @NotNull
  public static String getName(ErlangTypedExpr o) {
    return o.getNameIdentifier().getText();
  }

  public static PsiElement setName(ErlangTypedExpr o, String newName) {
    ErlangQAtom qAtom = o.getQAtom();
    PsiElement atom = qAtom.getAtom();
    if (atom != null) {
      atom.replace(ErlangElementFactory.createQAtomFromText(o.getProject(), newName));
    }
    return o;
  }

  @NotNull
  public static PsiElement getNameIdentifier(ErlangTypedExpr o) {
    return o.getQAtom();
  }

  public static int getTextOffset(ErlangTypedExpr o) {
    return o.getNameIdentifier().getTextOffset();
  }

  @Nullable
  public static PsiReference getReference(ErlangSpecFun o) {
    ErlangQAtom atom = o.getQAtom();
    ErlangModuleRef moduleRef = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    Integer arity = getArity(o);

    if (arity != null) {
      return new ErlangFunctionReferenceImpl<ErlangQAtom>(atom, moduleRef == null ? null : moduleRef.getQAtom(),
        TextRange.from(0, atom.getTextLength()), atom.getText(), arity);
    }
    return null;
  }

  @Nullable
  public static Integer getArity(@NotNull ErlangSpecFun o) {
    PsiElement integer = o.getInteger();
    Integer arity = null;
    if (integer != null) arity = getArity(integer);
    ErlangTypeSig sigs = PsiTreeUtil.getNextSiblingOfType(o, ErlangTypeSig.class);
    if (arity == null && sigs != null) arity = sigs.getFunType().getFunTypeArguments().getTopTypeList().size();
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
  public static ItemPresentation getPresentation(final ErlangFunction o) {
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
  public static String createFunctionClausePresentation(@Nullable ErlangFunctionClause clause) {
    if (clause == null) return "";
    return clause.getQAtom().getText() + "/" + calculateFunctionClauseArity(clause);
  }

  @NotNull
  public static String createFunctionPresentation(@NotNull ErlangFunction function) {
    return function.getName() + "/" + function.getArity();
  }
  @NotNull
  public static String getQualifiedFunctionName(@NotNull ErlangFunction function) {
    PsiFile containingFile = function.getContainingFile();
    ErlangModule module = getModule(containingFile);
    return module != null ? module.getName() + ":" + function.getName() : function.getName();
  }

  @NotNull
  public static String createFunctionPresentationFromCallbackSpec(@NotNull ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = getFunTypeSigs(spec);
    String funName = getCallbackSpecName(spec);

    List<ErlangTypeSig> typeSigList = funTypeSigs != null ? funTypeSigs.getTypeSigList() : null;
    ErlangTypeSig first = ContainerUtil.getFirstItem(typeSigList);
    int arity = first != null ? first.getFunType().getFunTypeArguments().getTopTypeList().size() : 0;

    return funName + "/" + arity;
  }

  @NotNull
  public static String createTypePresentation(@NotNull ErlangTypeDefinition type) {
    return type.getName() + "/" + calculateTypeArity(type);
  }

  @NotNull
  @SuppressWarnings("UnusedParameters")
  public static Icon getIcon(@NotNull ErlangFunction o, int flags) {
    return ErlangIcons.FUNCTION;
  }

  public static boolean isRecursiveCall(PsiElement element, ErlangFunction function) {
    return Comparing.equal(PsiTreeUtil.getParentOfType(element, ErlangFunction.class), function);
  }

  public static boolean isEunitTestFile(ErlangFile file) {
    VirtualFile virtualFile = file.getVirtualFile();
    String withoutExtension = virtualFile != null ? virtualFile.getNameWithoutExtension() : "";
    return (StringUtil.endsWith(withoutExtension, "_test") || StringUtil.endsWith(withoutExtension, "_tests")) && isEunitImported(file);
  }

  public static boolean isEunitTestFunction(@NotNull ErlangFunction function) {
    String name = function.getName();
    return (StringUtil.endsWith(name, "_test") || StringUtil.endsWith(name, "_test_"));
  }

  public static boolean isEunitImported(ErlangFile file) {
    return isEunitDirectlyImported(file) ||
      ContainerUtil.find(getIncludedFiles(file), new Condition<ErlangFile>() {
        @Override
        public boolean value(ErlangFile includedFile) {
          return isEunitDirectlyImported(includedFile);
        }
      }) != null;
  }

  private static boolean isEunitDirectlyImported(ErlangFile file) {
    List<ErlangIncludeLib> includes = file.getIncludeLibs();
    for (ErlangIncludeLib include : includes) {
      ErlangIncludeString string = include.getIncludeString();
      if (string != null) {
        String includeFilePath = StringUtil.unquoteString(string.getText());
        return StringUtil.equals(includeFilePath, "eunit/include/eunit.hrl");
      }
    }
    return false;
  }

  @NotNull
  public static SearchScope getUseScope(ErlangQVarImpl o) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(o, ErlangFunction.class, true);
    if (function != null) {
      return new LocalSearchScope(function);
    }
    return ResolveScopeManager.getElementUseScope(o);
  }

  @NotNull
  public static String getName(ErlangTypeDefinition o) {
    return o.getNameIdentifier().getText();
  }

  @Nullable
  public static ErlangSpecification getSpecification(@Nullable ErlangFunction function) { // todo: use ref search
    if (function == null) return null;
    PsiElement prevSibling = function.getPrevSibling();
    while (!(prevSibling instanceof ErlangFunction) && !(prevSibling instanceof ErlangAttribute) && prevSibling != null) {
      prevSibling = prevSibling.getPrevSibling();
    }
    if (prevSibling instanceof ErlangFunction) return null;

    return PsiTreeUtil.getChildOfType(prevSibling, ErlangSpecification.class);
  }

  public static boolean notFromPreviousFunction(@NotNull PsiElement spec, @Nullable ErlangFunction prevFunction) {
    return (prevFunction == null || (spec.getTextOffset() > prevFunction.getTextOffset()));
  }

  @SuppressWarnings("UnusedParameters")
  public static boolean isValidHost(@NotNull ErlangStringLiteral o) {
    return true;
  }

  public static ErlangStringLiteral updateText(@NotNull ErlangStringLiteral o, @NotNull String text) {
    final ErlangExpression expression = ErlangElementFactory.createExpressionFromText(o.getProject(), text);
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

  @Nullable
  private static ErlangQAtom getCallbackAtom(ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = getFunTypeSigs(spec);
    ErlangSpecFun specFun = funTypeSigs != null ? funTypeSigs.getSpecFun() : null;
    return specFun != null ? specFun.getQAtom() : null;
  }

  @Nullable
  public static ErlangFunTypeSigs getFunTypeSigs(ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = spec.getFunTypeSigs();
    if (funTypeSigs == null) {
      ErlangFunTypeSigsBraces braces = spec.getFunTypeSigsBraces();
      funTypeSigs = braces != null ? braces.getFunTypeSigs() : null;
    }
    return funTypeSigs;
  }

  @NotNull
  public static List<ErlangTopType> getCallBackSpecArguments(ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = getFunTypeSigs(spec);
    List<ErlangTypeSig> typeSigList = funTypeSigs != null ? funTypeSigs.getTypeSigList() : ContainerUtil.<ErlangTypeSig>emptyList();
    ErlangTypeSig typeSig = ContainerUtil.getFirstItem(typeSigList);
    ErlangFunType funType = typeSig != null ? typeSig.getFunType() : null;
    ErlangFunTypeArguments arguments = funType != null ? funType.getFunTypeArguments() : null;
    return arguments != null ? arguments.getTopTypeList() : ContainerUtil.<ErlangTopType>emptyList();
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
    final Ref<ErlangFunExpression> result = new Ref<ErlangFunExpression>();
    final Ref<Integer> currentFunExpressionNumber = new Ref<Integer>(0);
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

  public static <T extends PsiElement >ErlangFunctionCallParameter<T> inIsRecord(int position) {
    return new ErlangFunctionCallParameter<T>("is_record", position);
  }

  public static class ErlangFunctionCallParameter<T extends PsiElement> extends PatternCondition<T> {
    private final String myFunName;
    private final int myPosition;

    public ErlangFunctionCallParameter(@NotNull String funName, int position) {
      super("functionCallParameter");
      myFunName = funName;
      myPosition = position;
    }

    @Override
    public boolean accepts(@NotNull T element, ProcessingContext context) {
      ErlangExpression expr = PsiTreeUtil.getParentOfType(element, ErlangExpression.class, false);
      if (expr == null) return false;
      PsiElement list = expr.getParent();
      if (list instanceof ErlangArgumentList) {
        PsiElement funCall = list.getParent();
        List<ErlangExpression> expressions = ((ErlangArgumentList) list).getExpressionList();
        if (!(expressions.size() > myPosition && expressions.get(myPosition) == expr)) return false;
        if (funCall instanceof ErlangFunctionCallExpression) {
          return ((ErlangFunctionCallExpression) funCall).getQAtom().getText().equals(myFunName);
        }
      }
      return false;
    }
  }
}