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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.*;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.formatter.FormatterUtil;
import com.intellij.psi.impl.PsiManagerEx;
import com.intellij.psi.impl.ResolveScopeManager;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.FilenameIndex;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.Function;
import com.intellij.util.IncorrectOperationException;
import com.intellij.util.PathUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.*;
import org.intellij.erlang.bif.ErlangBifDescriptor;
import org.intellij.erlang.bif.ErlangBifTable;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.IOException;
import java.util.*;

public class ErlangPsiImplUtil {
  public static final Set<String> KNOWN_MACROS = ContainerUtil.set("MODULE", "MODULE_NAME", "FILE", "LINE", "MACHINE");
  public static final Set<String> BUILT_IN_TYPES = ContainerUtil.set("term", "boolean", "byte", "char",
    "non_neg_integer", "pos_integer", "neg_integer", "number", "integer", "float",
    "list", "any", "maybe_improper_list", "string", "char", "nonempty_string",
    "iolist", "module", "atom", "mfa", "node", "timeout", "no_return", "none"
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
    if (o.getTextLength() >= 2 && parent instanceof ErlangInclude) {
      return new PsiReferenceBase<PsiElement>(o, TextRange.from(1, o.getTextLength() - 2)) {
        @Override
        public PsiElement resolve() {
          return ContainerUtil.getFirstItem(filesFromInclude((ErlangInclude) parent));
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
    ErlangQAtom atom = o.getQAtom();
    ErlangModuleRef moduleRef = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    return new ErlangTypeReferenceImpl<ErlangQAtom>(atom, moduleRef, TextRange.from(0, atom.getTextLength()), atom.getText());
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangExportType o) {
    ErlangQAtom atom = o.getQAtom();
    ErlangModuleRef moduleRef = PsiTreeUtil.getPrevSiblingOfType(o, ErlangModuleRef.class);
    return new ErlangTypeReferenceImpl<ErlangQAtom>(atom, moduleRef, TextRange.from(0, atom.getTextLength()), atom.getText());
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
    ErlangAssignmentExpression assignment = PsiTreeUtil.getParentOfType(psiElement, ErlangAssignmentExpression.class);
    if (assignment == null) return false;
    return PsiTreeUtil.isAncestor(assignment.getLeft(), psiElement, true);
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

      if (qAtom != null) {
        String moduleName = qAtom.getText();
        functions.addAll(getExternalFunctionForCompletion(containingFile.getProject(), moduleName + ".erl"));

        Sdk sdk = ProjectRootManager.getInstance(containingFile.getProject()).getProjectSdk();
        ErlangSdkRelease release = sdk != null ? ErlangSdkType.getRelease(sdk) : null;
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

        if (!withArity) {
          for (ErlangBifDescriptor bif : ErlangBifTable.getBifs("erlang")) {
            lookupElements.add(createFunctionLookupElement(bif.getName(), bif.getArity(), withArity, ErlangCompletionContributor.BIF_PRIORITY));
          }
        }
      }

      functions.addAll(getErlangFunctionsFromIncludes((ErlangFile) containingFile, true, "", 0));

      lookupElements.addAll(ContainerUtil.map(functions, new Function<ErlangFunction, LookupElement>() {
        @Override
        public LookupElement fun(@NotNull final ErlangFunction function) {
          return createFunctionLookupElement(function, withArity, ErlangCompletionContributor.MODULE_FUNCTIONS_PRIORITY);
        }
      }));

      return lookupElements;
    }
    return Collections.emptyList();
  }

  private static LookupElement createFunctionLookupElement(ErlangFunction function, boolean withArity, double priority) {
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
    ErlangQAtom atom = o.getQAtom();
    return new ErlangRecordReferenceImpl<ErlangQAtom>(atom, TextRange.from(0, atom.getMacros() == null ? atom.getTextLength() : 1), atom.getText());
  }

  @Nullable
  public static PsiReference getReference(@NotNull ErlangModuleRef o) {
    ErlangQAtom atom = o.getQAtom();
    return new ErlangModuleReferenceImpl<ErlangQAtom>(atom, TextRange.from(0, atom.getTextLength()), atom.getText());
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
      } catch (IOException ignored) {
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
  static List<ErlangRecordDefinition> getErlangRecordFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangRecordDefinition> fromIncludes = new ArrayList<ErlangRecordDefinition>();
    for (ErlangInclude include : containingFile.getIncludes()) {
      List<ErlangFile> files = filesFromInclude(include);
      for (ErlangFile file : files) {
        if (!forCompletion) {
          ErlangRecordDefinition recordFromIncludeFile = file.getRecord(name);
          fromIncludes.addAll(recordFromIncludeFile == null ? ContainerUtil.<ErlangRecordDefinition>emptyList() : ContainerUtil.list(recordFromIncludeFile));
        }
        else {
          fromIncludes.addAll(file.getRecords());
        }
      }
    }
    return fromIncludes;
  }

  @NotNull
  static List<ErlangFile> filesFromInclude(@NotNull ErlangInclude include) {
    return filesFromIncludeInner(include, new HashSet<ErlangFile>());
  }

  @NotNull
  private static List<ErlangFile> filesFromIncludeInner(@NotNull ErlangInclude include, Set<ErlangFile> alreadyAdded) {
    PsiElement string = include.getIncludeString();
    PsiFile containingFile = include.getContainingFile();

    if (string != null) {
      String includeFilePath = string.getText().replaceAll("\"", "");
      List<ErlangFile> result = new ArrayList<ErlangFile>();
      List<ErlangFile> concat = ContainerUtil.concat(justAppend(containingFile, includeFilePath), findByWildCard(containingFile, includeFilePath));
      int beforeSize = alreadyAdded.size();
      for (ErlangFile erlangFile : concat) {
        if (!alreadyAdded.contains(erlangFile)) {
          result.add(erlangFile);
          alreadyAdded.add(erlangFile);
        }
      }
      if (beforeSize == alreadyAdded.size()) return result;
      for (ErlangFile erlangFile : concat) {
        for (ErlangInclude i : erlangFile.getIncludes()) {
          List<ErlangFile> erlangFiles = filesFromIncludeInner(i, alreadyAdded);
          result.addAll(erlangFiles);
        }
      }
      return result;
    }
    return Collections.emptyList();
  }

  @NotNull
  static List<ErlangFunction> getErlangFunctionsFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, @NotNull String name, int arity) {
    List<ErlangFunction> fromIncludes = new ArrayList<ErlangFunction>();
    for (ErlangInclude include : containingFile.getIncludes()) {
      List<ErlangFile> files = filesFromInclude(include);
      for (ErlangFile file : files) {
        if (!forCompletion) {
          ContainerUtil.addAllNotNull(fromIncludes, file.getFunction(name, arity));
        }
        else {
          fromIncludes.addAll(file.getFunctions());
        }
      }
    }
    return fromIncludes;
  }

  @NotNull
  static List<ErlangMacrosDefinition> getErlangMacrosesFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangMacrosDefinition> fromIncludes = new ArrayList<ErlangMacrosDefinition>();
    for (ErlangInclude include : containingFile.getIncludes()) {
      List<ErlangFile> files = filesFromInclude(include);
      for (ErlangFile file : files) {
        if (!forCompletion) {
          ContainerUtil.addIfNotNull(fromIncludes, file.getMacros(name));
        }
        else {
          fromIncludes.addAll(file.getMacroses());
        }
      }
    }
    return fromIncludes;
  }
  
  @NotNull
  static List<ErlangTypeDefinition> getErlangTypeFromIncludes(@NotNull ErlangFile containingFile, boolean forCompletion, String name) {
    List<ErlangTypeDefinition> fromIncludes = new ArrayList<ErlangTypeDefinition>();
    for (ErlangInclude include : containingFile.getIncludes()) {
      List<ErlangFile> files = filesFromInclude(include);
      for (ErlangFile file : files) {
        if (!forCompletion) {
          ContainerUtil.addIfNotNull(fromIncludes, file.getType(name));
        }
        else {
          fromIncludes.addAll(file.getTypes());
        }
      }
    }
    return fromIncludes;
  }

  @NotNull
  private static List<ErlangFile> findByWildCard(@NotNull PsiFile containingFile, @NotNull final String includeFilePath) {
    List<ErlangFile> erlangFiles = new ArrayList<ErlangFile>();
    List<String> split = StringUtil.split(includeFilePath, "/");
    String last = ContainerUtil.iterateAndGetLastItem(split);
    if (last != null) {
      PsiFile[] filesByName = FilenameIndex.getFilesByName(containingFile.getProject(), last,
        GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.allScope(containingFile.getProject()), ErlangFileType.HEADER, ErlangFileType.MODULE));
      List<PsiFile> filter = ContainerUtil.filter(filesByName, new Condition<PsiFile>() {
        @Override
        public boolean value(PsiFile psiFile) {
          VirtualFile virtualFile = psiFile.getVirtualFile();
          if (virtualFile == null) return false;
          String canonicalPath = virtualFile.getCanonicalPath();
          if (canonicalPath == null) return false;
          return canonicalPath.replaceAll("-[\\d\\.\\w-]+/", "/").endsWith(includeFilePath);
        }
      });

      for (PsiFile file : filter) {
        if (file instanceof ErlangFile) {
          erlangFiles.add((ErlangFile) file);
        }
      }
    }
    return erlangFiles;
  }

  @NotNull
  public static List<ErlangFile> justAppend(@NotNull PsiFile containingFile, @NotNull String includeFilePath) {
    List<ErlangFile> erlangFiles = new ArrayList<ErlangFile>();
    VirtualFile virtualFile = containingFile.getOriginalFile().getVirtualFile();
    VirtualFile parent = virtualFile != null ? virtualFile.getParent() : null;
    if (parent == null) return ContainerUtil.emptyList();
    String localPath = PathUtil.getLocalPath(parent);
    String globalPath = localPath + "/" + includeFilePath;

    VirtualFile fileByUrl = LocalFileSystem.getInstance().findFileByPath(globalPath);
    if (fileByUrl == null) return ContainerUtil.emptyList();
    PsiFile file = ((PsiManagerEx) PsiManager.getInstance(containingFile.getProject())).getFileManager().findFile(fileByUrl);
    if (file instanceof ErlangFile) {
      erlangFiles.add((ErlangFile) file);
    }
    return erlangFiles;
  }

  public static PsiElement getNameIdentifier(ErlangMacrosDefinition o) {
    ErlangMacrosName macrosName = o.getMacrosName();
    if (macrosName == null) return o;
    return macrosName;
  }

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

  public static String getName(ErlangMacrosDefinition o) {
    return o.getNameIdentifier().getText();
  }

  public static PsiElement setName(ErlangMacrosDefinition o, String newName) {
    ErlangMacrosName macrosName = o.getMacrosName();
    if (macrosName != null) {
      macrosName.replace(ErlangElementFactory.createMacrosFromText(o.getProject(), newName));
    }
    return o;
  }

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
  public static ErlangFunTypeSigs getSignature(@NotNull ErlangSpecification o) {
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
  public static String createFunctionPresentationFromCallbackSpec(@NotNull ErlangCallbackSpec callbackSpec) {
    ErlangFunTypeSigs funTypeSigs = callbackSpec.getFunTypeSigs();

    ErlangSpecFun specFun = funTypeSigs != null ? funTypeSigs.getSpecFun() : null;
    ErlangQAtom funNameAtom = specFun != null ? specFun.getQAtom() : null;
    String funName = funNameAtom != null ? funNameAtom.getText() : "";

    List<ErlangTypeSig> typeSigList = funTypeSigs != null ? funTypeSigs.getTypeSigList() : null;
    ErlangTypeSig typeSig = typeSigList != null && !typeSigList.isEmpty() ? typeSigList.get(0) : null;
    int arity = typeSig != null ? typeSig.getFunType().getFunTypeArguments().getTopTypeList().size() : 0;

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

  public static boolean isEunitImported(ErlangFile file) {
    List<ErlangInclude> includes = file.getIncludes();
    for (ErlangInclude include : includes) {
      ErlangIncludeString string = include.getIncludeString();
      if (string != null) {
        String includeFilePath = StringUtil.unquoteString(string.getText());
        boolean isEunit = StringUtil.equals(includeFilePath, "eunit/include/eunit.hrl") || StringUtil.equals(includeFilePath, "eunit.hrl");
        if (isEunit) return true;
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
    ErlangFunTypeSigs funTypeSigs = spec.getFunTypeSigs();
    ErlangSpecFun specFun = funTypeSigs != null ? funTypeSigs.getSpecFun() : null;
    ErlangQAtom atom = specFun != null ? specFun.getQAtom() : null;
    return atom != null ? atom.getText() : null;
  }

  @NotNull
  public static List<ErlangTopType> getCallBackSpecArguments(ErlangCallbackSpec spec) {
    ErlangFunTypeSigs funTypeSigs = spec.getFunTypeSigs();
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
}