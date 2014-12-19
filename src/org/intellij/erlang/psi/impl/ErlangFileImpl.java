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

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubTree;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.*;
import com.intellij.util.*;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.MultiMap;
import gnu.trove.THashMap;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.parser.ErlangParserUtil;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.stubs.ErlangCallbackSpecStub;
import org.intellij.erlang.stubs.ErlangFileStub;
import org.intellij.erlang.stubs.types.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class ErlangFileImpl extends PsiFileBase implements ErlangFile, PsiNameIdentifierOwner {
  public ErlangFileImpl(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, ErlangLanguage.INSTANCE);
  }

  @Nullable
  @Override
  public ErlangModule getModule() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return ArrayUtil.getFirstElement(stub.getChildrenByType(ErlangTypes.ERL_MODULE, ErlangModuleStubElementType.ARRAY_FACTORY));
    }

    List<ErlangAttribute> attributes = PsiTreeUtil.getChildrenOfTypeAsList(this, ErlangAttribute.class);
    for (ErlangAttribute attribute : attributes) {
      ErlangModule module = attribute.getModule();
      if (module != null) {
        return module;
      }
    }
    return null;
  }

  @Override
  public PsiElement setName(@NotNull String name) throws IncorrectOperationException {
    String nameWithoutExtension = FileUtil.getNameWithoutExtension(name);

    for (ErlangAttribute moduleAttributes : getAttributes()) {
      ErlangModule module = moduleAttributes.getModule();
      if (module != null) {
        // todo: use module with dependencies scope
        if (!DumbService.isDumb(getProject())) {
          Query<PsiReference> search = ReferencesSearch.search(module, GlobalSearchScope.allScope(module.getProject()));
          for (PsiReference psiReference : search) {
            psiReference.handleElementRename(nameWithoutExtension);
          }
        }
        module.setName(nameWithoutExtension);
      }
    }

    return super.setName(name);
  }

  private CachedValue<List<ErlangRule>> myRulesValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangRule>>() {
      @Override
      public Result<List<ErlangRule>> compute() {
        return Result.create(calcRules(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangFunction>> myFunctionValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangFunction>>() {
      @Override
      public Result<List<ErlangFunction>> compute() {
        return Result.create(calcFunctions(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Set<ErlangFunction>> myExportedFunctionValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Set<ErlangFunction>>() {
      @Override
      public Result<Set<ErlangFunction>> compute() {
        return Result.create(calcExportedFunctions(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangAttribute>> myAttributeValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangAttribute>>() {
      @Override
      public Result<List<ErlangAttribute>> compute() {
        return Result.create(calcAttributes(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangRecordDefinition>> myRecordValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangRecordDefinition>>() {
      @Override
      public Result<List<ErlangRecordDefinition>> compute() {
        return Result.create(calcRecords(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangInclude>> myIncludeValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangInclude>>() {
      @Override
      public Result<List<ErlangInclude>> compute() {
        return Result.create(calcIncludes(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangIncludeLib>> myIncludeLibValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangIncludeLib>>() {
      @Override
      public Result<List<ErlangIncludeLib>> compute() {
        return Result.create(calcIncludeLibs(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<MultiMap<String, ErlangFunction>> myFunctionsMap =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<MultiMap<String, ErlangFunction>>() {
      @Override
      public Result<MultiMap<String, ErlangFunction>> compute() {
        MultiMap<String, ErlangFunction> map = new MultiMap<String, ErlangFunction>();
        for (ErlangFunction function : getFunctions()) {
          map.putValue(function.getName(), function);
        }
        return Result.create(map, ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Map<String, ErlangRecordDefinition>> myRecordsMap =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangRecordDefinition>>() {
      @Override
      public Result<Map<String, ErlangRecordDefinition>> compute() {
        Map<String, ErlangRecordDefinition> map = new THashMap<String, ErlangRecordDefinition>();
        for (ErlangRecordDefinition record : getRecords()) {
          String recordName = record.getName();
          if (!map.containsKey(recordName)) {
            map.put(recordName, record);
          }
        }
        return Result.create(map, ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangMacrosDefinition>> myMacrosValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangMacrosDefinition>>() {
      @Override
      public Result<List<ErlangMacrosDefinition>> compute() {
        return Result.create(calcMacroses(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Map<String, ErlangMacrosDefinition>> myMacrosesMap =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangMacrosDefinition>>() {
      @Override
      public Result<Map<String, ErlangMacrosDefinition>> compute() {
        Map<String, ErlangMacrosDefinition> map = new THashMap<String, ErlangMacrosDefinition>();
        for (ErlangMacrosDefinition macros : getMacroses()) {
          String macrosName = ErlangPsiImplUtil.getName(macros);
          if (!map.containsKey(macrosName)) {
            map.put(macrosName, macros);
          }
        }
        return Result.create(map, ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangTypeDefinition>> myTypeValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangTypeDefinition>>() {
      @Override
      public Result<List<ErlangTypeDefinition>> compute() {
        return Result.create(calcTypes(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Map<String, ErlangTypeDefinition>> myTypeMap =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangTypeDefinition>>() {
      @Override
      public Result<Map<String, ErlangTypeDefinition>> compute() {
        Map<String, ErlangTypeDefinition> map = new THashMap<String, ErlangTypeDefinition>();
        for (ErlangTypeDefinition type : getTypes()) {
          String mName = type.getName();
          if (!map.containsKey(mName)) {
            map.put(mName, type);
          }
        }
        return Result.create(map, ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Map<String, ErlangCallbackSpec>> myCallbackMap =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangCallbackSpec>>() {
      @Nullable
      @Override
      public Result<Map<String, ErlangCallbackSpec>> compute() {
        return Result.create(calcCallbacks(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangBehaviour>> myBehavioursValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangBehaviour>>() {
      @Override
      public Result<List<ErlangBehaviour>> compute() {
        return Result.create(calcBehaviours(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<List<ErlangSpecification>> mySpecificationsValue =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangSpecification>>() {
      @Override
      public Result<List<ErlangSpecification>> compute() {
        return Result.create(calcSpecifications(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Boolean> myExportAll =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Boolean>() {
      @Override
      public Result<Boolean> compute() {
        return Result.create(calcExportAll(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Boolean> myNoAutoImportAll =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Boolean>() {
      @Override
      public Result<Boolean> compute() {
        return Result.create(calcNoAutoImportAll(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Set<String>> myExportedFunctionsSignatures =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Set<String>>() {
      @Override
      public Result<Set<String>> compute() {
        return Result.create(calcExportedSignatures(), ErlangFileImpl.this);
      }
    }, false);
  private CachedValue<Set<String>> myNoAutoImportFunctionsSignatures =
    CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Set<String>>() {
      @Override
      public Result<Set<String>> compute() {
        return Result.create(calcNoAutoImportSignatures(), ErlangFileImpl.this);
      }
    }, false);

  @NotNull
  @Override
  public FileType getFileType() {
    return ErlangFileType.MODULE;
  }

  @Nullable
  public ErlangFileStub getStub() {
    StubElement stub = super.getStub();
    if (stub == null) return null;
    return (ErlangFileStub) stub;
  }

  @Override
  public boolean isExported(@NotNull String signature) {
    if (isExportedAll()) return true;
    return myExportedFunctionsSignatures.getValue().contains(signature);
  }

  @Override
  public boolean isNoAutoImport(@NotNull String name, int arity) {
    if (isNoAutoImportAll()) return true;
    return myNoAutoImportFunctionsSignatures.getValue().contains(name + "/" + arity);
  }

  @NotNull
  private Set<String> calcExportedSignatures() {
    Set<String> result = ContainerUtil.newHashSet();
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangExport export = attribute.getExport();
      ErlangExportFunctions exportFunctions = export != null ? export.getExportFunctions() : null;
      if (exportFunctions == null) continue;
      List<ErlangExportFunction> list = exportFunctions.getExportFunctionList();
      for (ErlangExportFunction exportFunction : list) {
        PsiElement integer = exportFunction.getInteger();
        if (integer == null) continue;
        String s = ErlangPsiImplUtil.getExportFunctionName(exportFunction) + "/" + integer.getText();
        result.add(s);
      }
    }
    return result;
  }

  @NotNull
  private List<ErlangExpression> getCompileDirectiveExpressions() {
    List<ErlangExpression> result = ContainerUtil.newArrayList();
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      if (atomAttribute == null) continue;
      if (!"compile".equals(atomAttribute.getQAtom().getText())) continue;
      if (atomAttribute.getAttrVal() == null) continue;
      result.addAll(atomAttribute.getAttrVal().getExpressionList());
    }
    return result;
  }

  @NotNull
  private Set<String> calcNoAutoImportSignatures() {
    Set<String> result = ContainerUtil.newHashSet();
    for (ErlangExpression expression : getCompileDirectiveExpressions()) {
      if (expression instanceof ErlangListExpression) {
        for (ErlangExpression tuple : ((ErlangListExpression) expression).getExpressionList()) {
          if (tuple instanceof ErlangTupleExpression) {
            result.addAll(getNoAutoImportFunctionSignaturesFromTuple((ErlangTupleExpression) tuple));
          }
        }
      }
      else if (expression instanceof ErlangTupleExpression) {
        result.addAll(getNoAutoImportFunctionSignaturesFromTuple((ErlangTupleExpression) expression));
      }
    }
    return result;
  }

  @NotNull
  private static Set<String> getNoAutoImportFunctionSignaturesFromTuple(@Nullable ErlangTupleExpression tupleExpression) {
    final Set<String> result = ContainerUtil.newHashSet();
    if (tupleExpression == null || tupleExpression.getExpressionList().size() != 2) return result;
    ErlangExpression first = ContainerUtil.getFirstItem(tupleExpression.getExpressionList());
    ErlangExpression second = ContainerUtil.getLastItem(tupleExpression.getExpressionList());
    if (!(first instanceof ErlangMaxExpression)
      || !(second instanceof ErlangListExpression)
      || !"no_auto_import".equals(ErlangPsiImplUtil.getAtomName((ErlangMaxExpression) first)))
      return result;
    second.accept(new PsiRecursiveElementWalkingVisitor() {
      @Override
      public void visitElement(PsiElement element) {
        if (element instanceof ErlangAtomWithArityExpression) {
          result.add(ErlangPsiImplUtil.createFunctionPresentation((ErlangAtomWithArityExpression) element));
        }
        else {
          super.visitElement(element);
        }
      }
    });
    return result;
  }

  @Override
  public boolean isExportedAll() {
    //TODO do we use stubs?
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return stub.isExportAll();
    }
    return myExportAll.getValue();
  }

  private boolean containsCompileDirectiveWithOption(@NotNull String option) {
    for (ErlangExpression expression : getCompileDirectiveExpressions()) {
      if (expression instanceof ErlangListExpression) {
        for (ErlangExpression e : ((ErlangListExpression) expression).getExpressionList()) {
          if (e instanceof ErlangMaxExpression
            && option.equals(ErlangPsiImplUtil.getAtomName((ErlangMaxExpression) e))) {
            return true;
          }
        }
      }
      else if (expression instanceof ErlangMaxExpression
        && option.equals(ErlangPsiImplUtil.getAtomName((ErlangMaxExpression) expression))) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean isNoAutoImportAll() {
    return myNoAutoImportAll.getValue();
  }

  private boolean calcNoAutoImportAll() {
    return containsCompileDirectiveWithOption("no_auto_import");
  }

  private boolean calcExportAll() {
    return containsCompileDirectiveWithOption("export_all");
  }

  @NotNull
  @Override
  public List<ErlangRule> getRules() {
    return myRulesValue.getValue();
  }

  @NotNull
  @Override
  public List<ErlangAttribute> getAttributes() {
    return myAttributeValue.getValue();
  }

  @Nullable
  @Override
  public ErlangCallbackSpec getCallbackByName(@NotNull String fullName) {
    return getCallbackMap().get(fullName);
  }

  @NotNull
  @Override
  public Map<String, ErlangCallbackSpec> getCallbackMap() {
    //TODO do we use stubs?
    ErlangFileStub stub = getStub();
    if (stub != null) {
      Map<String, ErlangCallbackSpec> callbacksMap = new LinkedHashMap<String, ErlangCallbackSpec>();
      for (StubElement child : stub.getChildrenStubs()) {
        if (child instanceof ErlangCallbackSpecStub) {
          String name = ((ErlangCallbackSpecStub) child).getName();
          int arity = ((ErlangCallbackSpecStub) child).getArity();
          callbacksMap.put(name + "/" + arity, ((ErlangCallbackSpecStub) child).getPsi());
        }
      }
      return callbacksMap;
    }

    return myCallbackMap.getValue();
  }

  @NotNull
  private Map<String, ErlangCallbackSpec> calcCallbacks() {
    Map<String, ErlangCallbackSpec> callbacksMap = new LinkedHashMap<String, ErlangCallbackSpec>();

    for (ErlangAttribute a : getAttributes()) {
      ErlangCallbackSpec spec = a.getCallbackSpec();
      if (spec != null) {
        String name = ErlangPsiImplUtil.getCallbackSpecName(spec);
        int arity = ErlangPsiImplUtil.getCallBackSpecArguments(spec).size();
        callbacksMap.put(name + "/" + arity, spec);
      }
    }
    return callbacksMap;
  }
  
  @NotNull
  @Override
  public List<ErlangFunction> getFunctions() {
    return myFunctionValue.getValue();
  }

  @NotNull
  @Override
  public Collection<ErlangFunction> getExportedFunctions() {
    return myExportedFunctionValue.getValue();
  }

  private Set<ErlangFunction> calcExportedFunctions() {
    if (isExportedAll()) {
      return ContainerUtil.newHashSet(getFunctions());
    }
    final Set<ErlangFunction> result = new HashSet<ErlangFunction>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangFunction) {
          if (((ErlangFunction) psiElement).isExported()) {
            result.add((ErlangFunction) psiElement);
          }
        }
        return true;
      }
    });
    return result;
  }

  @Nullable
  @Override
  public ErlangFunction getFunction(@NotNull String name, int argsCount) {
    MultiMap<String, ErlangFunction> value = myFunctionsMap.getValue();
    return getFunctionFromMap(value, name, argsCount);
  }

  @Override
  @NotNull
  public Collection<ErlangFunction> getFunctionsByName(@NotNull String name) {
    return myFunctionsMap.getValue().get(name);
  }

  @Nullable
  private static ErlangFunction getFunctionFromMap(MultiMap<String, ErlangFunction> value, String name, final int argsCount) {
    Collection<ErlangFunction> candidates = value.get(name);

    return ContainerUtil.getFirstItem(ContainerUtil.filter(candidates, new Condition<ErlangFunction>() {
      @Override
      public boolean value(ErlangFunction erlangFunction) {
        return erlangFunction.getArity() == argsCount;
      }
    }));
  }

  @NotNull
  @Override
  public List<ErlangRecordDefinition> getRecords() {
    return myRecordValue.getValue();
  }

  @NotNull
  @Override
  public List<ErlangTypeDefinition> getTypes() {
    return myTypeValue.getValue();
  }

  private List<ErlangTypeDefinition> calcTypes() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_TYPE_DEFINITION, ErlangTypeDefinitionElementType.ARRAY_FACTORY);
    }

    final List<ErlangTypeDefinition> result = new ArrayList<ErlangTypeDefinition>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangTypeDefinition) {
          result.add((ErlangTypeDefinition) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  private static <E extends PsiElement> List<E> getChildrenByType(ErlangFileStub stub, IElementType elementType, ArrayFactory<E> f) {
    return Arrays.asList(stub.getChildrenByType(elementType, f));
  }

  @Override
  public ErlangTypeDefinition getType(@NotNull String name) {
    return myTypeMap.getValue().get(name);
  }

  @NotNull
  @Override
  public List<ErlangMacrosDefinition> getMacroses() {
    return myMacrosValue.getValue();
  }

  private List<ErlangMacrosDefinition> calcMacroses() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_MACROS_DEFINITION, ErlangMacrosDefinitionElementType.ARRAY_FACTORY);
    }

    final List<ErlangMacrosDefinition> result = new ArrayList<ErlangMacrosDefinition>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangMacrosDefinition) {
          result.add((ErlangMacrosDefinition) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  @Override
  public ErlangMacrosDefinition getMacros(@NotNull String name) {
    return myMacrosesMap.getValue().get(name);
  }

  private List<ErlangRecordDefinition> calcRecords() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_RECORD_DEFINITION, ErlangRecordDefinitionElementType.ARRAY_FACTORY);
    }

    final List<ErlangRecordDefinition> result = new ArrayList<ErlangRecordDefinition>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangRecordDefinition) {
          result.add((ErlangRecordDefinition) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  @NotNull
  @Override
  public List<ErlangInclude> getIncludes() {
    return myIncludeValue.getValue();
  }

  @NotNull
  @Override
  public List<ErlangIncludeLib> getIncludeLibs() {
    return myIncludeLibValue.getValue();
  }

  private List<ErlangInclude> calcIncludes() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_INCLUDE, ErlangIncludeElementType.ARRAY_FACTORY);
    }

    final List<ErlangInclude> result = new ArrayList<ErlangInclude>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangInclude) {
          result.add((ErlangInclude) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  private List<ErlangIncludeLib> calcIncludeLibs() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_INCLUDE_LIB, ErlangIncludeLibElementType.ARRAY_FACTORY);
    }

    final List<ErlangIncludeLib> result = new ArrayList<ErlangIncludeLib>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangIncludeLib) {
          result.add((ErlangIncludeLib) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  @NotNull
  @Override
  public List<ErlangBehaviour> getBehaviours() {
    return myBehavioursValue.getValue();
  }

  private List<ErlangBehaviour> calcBehaviours() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_BEHAVIOUR, ErlangBehaviourStubElementType.ARRAY_FACTORY);
    }

    final List<ErlangBehaviour> result = new ArrayList<ErlangBehaviour>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangAttribute && ((ErlangAttribute) psiElement).getBehaviour() != null) {
          result.add(((ErlangAttribute) psiElement).getBehaviour());
        }
        return true;
      }
    });
    return result;
  }

  @NotNull
  @Override
  public List<ErlangSpecification> getSpecifications() {
    return mySpecificationsValue.getValue();
  }

  private List<ErlangSpecification> calcSpecifications() {
    final ArrayList<ErlangSpecification> result = new ArrayList<ErlangSpecification>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangAttribute && ((ErlangAttribute) psiElement).getSpecification() != null) {
          result.add(((ErlangAttribute) psiElement).getSpecification());
        }
        return true;
      }
    });
    return result;
  }

  @Override
  public ErlangRecordDefinition getRecord(String name) {
    return myRecordsMap.getValue().get(name);
  }

  @NotNull
  @Override
  public ArrayList<ErlangImportFunction> getImportedFunctions() {
    ArrayList<ErlangImportFunction> result = new ArrayList<ErlangImportFunction>();
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangImportDirective importDirective = attribute.getImportDirective();
      if (importDirective != null) {
        ErlangImportFunctions importFunctions = importDirective.getImportFunctions();
        if (importFunctions != null) {
          List<ErlangImportFunction> importFunctionList = importFunctions.getImportFunctionList();
          for (ErlangImportFunction importFunction : importFunctionList) {
            result.add(importFunction);
          }
        }
      }
    }
    return result;
  }

  private List<ErlangFunction> calcFunctions() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_FUNCTION, ErlangFunctionStubElementType.ARRAY_FACTORY);
    }

    final List<ErlangFunction> result = new ArrayList<ErlangFunction>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangFunction) {
          result.add((ErlangFunction) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  private List<ErlangAttribute> calcAttributes() {
    final List<ErlangAttribute> result = new ArrayList<ErlangAttribute>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangAttribute) {
          result.add((ErlangAttribute) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  private List<ErlangRule> calcRules() {
    final List<ErlangRule> result = new ArrayList<ErlangRule>();
    processChildrenDummyAware(this, new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        if (psiElement instanceof ErlangRule) {
          result.add((ErlangRule) psiElement);
        }
        return true;
      }
    });
    return result;
  }

  @Override
  public void addDeclaredParseTransforms(@NotNull Set<String> parseTransforms) {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      String fromStub = stub.getParseTransforms();
      List<String> split = StringUtil.split(fromStub, ",");
      parseTransforms.addAll(split);
      return;
    }
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      ErlangQAtom qAtom = null != atomAttribute ? atomAttribute.getQAtom() : null;
      ErlangAtom atom = null != qAtom ? qAtom.getAtom() : null;
      String attributeName = null != atom ? atom.getName() : null;
      ErlangAttrVal attrVal = atomAttribute != null ? atomAttribute.getAttrVal() : null;
      if (!"compile".equals(attributeName) || attrVal == null) continue;

      for (ErlangExpression expression : attrVal.getExpressionList()) {
        //TODO support macros
        if (expression instanceof ErlangListExpression) {
          ErlangPsiImplUtil.extractParseTransforms((ErlangListExpression) expression, parseTransforms);
        }
        if (expression instanceof ErlangTupleExpression) {
          ErlangPsiImplUtil.extractParseTransforms((ErlangTupleExpression) expression, parseTransforms);
        }
      }
    }
  }

  private static boolean processChildrenDummyAware(ErlangFileImpl file, final Processor<PsiElement> processor) {
    StubTree stubTree = file.getStubTree();
    if (stubTree != null) {
      List<StubElement<?>> plainList = stubTree.getPlainList();
      for (StubElement<?> stubElement : plainList) {
        PsiElement psi = stubElement.getPsi();
        if (!processor.process(psi)) return false;
      }
      return true;
    }

    return new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        for (PsiElement child = psiElement.getFirstChild(); child != null; child = child.getNextSibling()) {
          if (child instanceof ErlangParserUtil.DummyBlock) {
            if (!process(child)) return false;
          }
          else if (!processor.process(child)) return false;
        }
        return true;
      }
    }.process(file);
  }

  @Nullable
  @Override
  public PsiElement getNameIdentifier() {
    return null; // hack for inplace rename: InplaceRefactoring#getVariable()
  }
}
