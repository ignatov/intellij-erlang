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
import com.intellij.ide.scratch.ScratchUtil;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.*;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.MultiMap;
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

import static java.util.Collections.*;
import static org.intellij.erlang.psi.impl.ErlangPsiImplUtil.*;

public class ErlangFileImpl extends PsiFileBase implements ErlangFile, PsiNameIdentifierOwner {
  private final CachedValue<ErlangModule> myModuleValue =
    createCachedValue(new ValueProvider<>() {
      @Nullable
      @Override
      protected ErlangModule computeValue() {
        return calcModule();
      }
    });
  private final CachedValue<List<ErlangRule>> myRulesValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      protected List<ErlangRule> computeValue() {
        return unmodifiableList(calcRules());
      }
    });
  private final CachedValue<List<ErlangFunction>> myFunctionValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangFunction> computeValue() {
        return unmodifiableList(calcFunctions());
      }
    });
  private final CachedValue<List<ErlangImportFunction>> myImportValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangImportFunction> computeValue() {
        return unmodifiableList(calcImports());
      }
    });
  private final CachedValue<Set<ErlangFunction>> myExportedFunctionValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Set<ErlangFunction> computeValue() {
        return unmodifiableSet(calcExportedFunctions());
      }
    });
  private final CachedValue<List<ErlangAttribute>> myAttributeValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangAttribute> computeValue() {
        return unmodifiableList(calcAttributes());
      }
    });
  private final CachedValue<List<ErlangRecordDefinition>> myRecordValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangRecordDefinition> computeValue() {
        return unmodifiableList(calcRecords());
      }
    });
  private final CachedValue<List<ErlangInclude>> myIncludeValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangInclude> computeValue() {
        return unmodifiableList(calcIncludes());
      }
    });
  private final CachedValue<List<ErlangIncludeLib>> myIncludeLibValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangIncludeLib> computeValue() {
        return unmodifiableList(calcIncludeLibs());
      }
    });
  private final CachedValue<MultiMap<String, ErlangFunction>> myFunctionsMap =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public MultiMap<String, ErlangFunction> computeValue() {
        MultiMap<String, ErlangFunction> map = new MultiMap<>();
        for (ErlangFunction function : getFunctions()) {
          map.putValue(function.getName(), function);
        }
        return map;
      }
    });
  private final CachedValue<MultiMap<String, ErlangImportFunction>> myImportsMap =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public MultiMap<String, ErlangImportFunction> computeValue() {
        MultiMap<String, ErlangImportFunction> map = new MultiMap<>();
        for (ErlangImportFunction importFunction : getImportedFunctions()) {
          map.putValue(ErlangPsiImplUtil.getName(importFunction), importFunction);
        }
        return map;
      }
    });
  private final CachedValue<Map<String, ErlangRecordDefinition>> myRecordsMap =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Map<String, ErlangRecordDefinition> computeValue() {
        Map<String, ErlangRecordDefinition> map = new HashMap<>();
        for (ErlangRecordDefinition record : getRecords()) {
          String recordName = record.getName();
          if (!map.containsKey(recordName)) {
            map.put(recordName, record);
          }
        }
        return map;
      }
    });
  private final CachedValue<List<ErlangMacrosDefinition>> myMacrosValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangMacrosDefinition> computeValue() {
        return unmodifiableList(calcMacroses());
      }
    });
  private final CachedValue<Map<String, ErlangMacrosDefinition>> myMacrosesMap =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Map<String, ErlangMacrosDefinition> computeValue() {
        Map<String, ErlangMacrosDefinition> map = new HashMap<>();
        for (ErlangMacrosDefinition macros : getMacroses()) {
          String macrosName = ErlangPsiImplUtil.getName(macros);
          if (!map.containsKey(macrosName)) {
            map.put(macrosName, macros);
          }
        }
        return map;
      }
    });
  private final CachedValue<List<ErlangTypeDefinition>> myTypeValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangTypeDefinition> computeValue() {
        return unmodifiableList(calcTypes());
      }
    });
  private final CachedValue<Map<String, ErlangTypeDefinition>> myTypeMap =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Map<String, ErlangTypeDefinition> computeValue() {
        Map<String, ErlangTypeDefinition> map = new HashMap<>();
        for (ErlangTypeDefinition type : getTypes()) {
          String mName = type.getName();
          if (!map.containsKey(mName)) {
            map.put(mName, type);
          }
        }
        return map;
      }
    });
  private final CachedValue<Map<String, ErlangCallbackSpec>> myCallbackMap =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Map<String, ErlangCallbackSpec> computeValue() {
        return unmodifiableMap(calcCallbacks());
      }
    });
  private final CachedValue<List<ErlangBehaviour>> myBehavioursValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangBehaviour> computeValue() {
        return unmodifiableList(calcBehaviours());
      }
    });
  private final CachedValue<Collection<ErlangCallbackFunction>> myOptionalCallbacks =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Collection<ErlangCallbackFunction> computeValue() {
        return unmodifiableCollection(calcOptionalCallbacks());
      }
    });
  private final CachedValue<List<ErlangSpecification>> mySpecificationsValue =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public List<ErlangSpecification> computeValue() {
        return unmodifiableList(calcSpecifications());
      }
    });
  private final CachedValue<Boolean> myExportAll =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Boolean computeValue() {
        return calcExportAll();
      }
    });
  private final CachedValue<Boolean> myNoAutoImportAll =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Boolean computeValue() {
        return calcNoAutoImportAll();
      }
    });
  private final CachedValue<Set<String>> myExportedFunctionsSignatures =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Set<String> computeValue() {
        return unmodifiableSet(calcExportedSignatures());
      }
    });
  private final CachedValue<Set<String>> myNoAutoImportFunctionsSignatures =
    createCachedValue(new ValueProvider<>() {
      @NotNull
      @Override
      public Set<String> computeValue() {
        return unmodifiableSet(calcNoAutoImportSignatures());
      }
    });

  public ErlangFileImpl(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, ErlangLanguage.INSTANCE);
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

  @NotNull
  @Override
  public FileType getFileType() {
    FileType fileType = getViewProvider().getFileType();
    if(ScratchUtil.isScratch(getViewProvider().getVirtualFile())) return fileType;
    if (!(fileType instanceof ErlangFileType) && ApplicationManager.getApplication().isUnitTestMode()) {
      return getFileTypeForTests();
    }
    return fileType;
  }

  @NotNull
  private FileType getFileTypeForTests() {
    // when in unit test mode fileViewProvider gets replaced by a mock, and we don't get filetype set correctly

    assert ApplicationManager.getApplication().isUnitTestMode() :
      "This method should only be called in unit tests";

    String extension = PathUtil.getFileExtension(getName());
    for (ErlangFileType type : ErlangFileType.TYPES) {
      for (String defaultExtension : type.getDefaultExtensions()) {
        if (StringUtil.equalsIgnoreCase(defaultExtension, extension)) {
          return type;
        }
      }
    }
    return ErlangFileType.MODULE;
  }

  @Nullable
  @Override
  public ErlangFileStub getStub() {
    StubElement<?> stub = super.getStub();
    return (ErlangFileStub) stub;
  }

  @Nullable
  @Override
  public ErlangModule getModule() {
    return myModuleValue.getValue();
  }

  @Nullable
  private ErlangModule calcModule() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return ArrayUtil.getFirstElement(stub.getChildrenByType(ErlangTypes.ERL_MODULE, ErlangModuleStubElementType.ARRAY_FACTORY));
    }

    for (ErlangAttribute attribute : getAttributes()) {
      ErlangModule module = attribute.getModule();
      if (module != null) {
        return module;
      }
    }
    return null;
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
    Set<String> result = new HashSet<>();
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
    List<ErlangExpression> result = new ArrayList<>();
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      if (atomAttribute != null && "compile".equals(atomAttribute.getName()) && atomAttribute.getAttrVal() != null) {
        result.addAll(atomAttribute.getAttrVal().getExpressionList());
      }
    }
    return result;
  }

  @NotNull
  private Set<String> calcNoAutoImportSignatures() {
    Set<String> result = new HashSet<>();
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
    final Set<String> result = new HashSet<>();
    if (tupleExpression == null || tupleExpression.getExpressionList().size() != 2) return result;
    ErlangExpression first = ContainerUtil.getFirstItem(tupleExpression.getExpressionList());
    ErlangExpression second = ContainerUtil.getLastItem(tupleExpression.getExpressionList());
    if (!(first instanceof ErlangMaxExpression)
      || !(second instanceof ErlangListExpression)
      || !"no_auto_import".equals(getAtomName((ErlangMaxExpression) first))) {
      return result;
    }
    second.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitAtomWithArityExpression(@NotNull ErlangAtomWithArityExpression o) {
        result.add(createFunctionPresentation(o));
      }

      @Override
      public void visitTupleExpression(@NotNull ErlangTupleExpression o) {
        List<ErlangExpression> exprs = o.getExpressionList();
        if (exprs.size() != 2) return;

        String functionName = getAtomName(ObjectUtils.tryCast(exprs.get(0), ErlangMaxExpression.class));
        int functionArity = getArity(ObjectUtils.tryCast(exprs.get(1), ErlangMaxExpression.class));
        if (functionName == null || functionArity == -1) return;

        result.add(createFunctionPresentation(functionName, functionArity));
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
          if (e instanceof ErlangMaxExpression && option.equals(getAtomName((ErlangMaxExpression) e))) {
            return true;
          }
        }
      }
      else if (expression instanceof ErlangMaxExpression && option.equals(getAtomName((ErlangMaxExpression) expression))) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean isNoAutoImportAll() {
    return myNoAutoImportAll.getValue();
  }

  @Override
  public boolean isBehaviour() {
    ErlangFileStub stub = getStub();
    if (stub != null) return stub.isBehaviour();

    ErlangFunction behaviourInfo = getFunction("behaviour_info", 1);
    return behaviourInfo != null && behaviourInfo.isExported() || !getCallbackMap().isEmpty();
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
      Map<String, ErlangCallbackSpec> callbacksMap = new LinkedHashMap<>();
      for (StubElement<?> child : stub.getChildrenStubs()) {
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
    Map<String, ErlangCallbackSpec> callbacksMap = new LinkedHashMap<>();

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
    return ContainerUtil.map2SetNotNull(getFunctions(), f -> f.isExported() ? f : null);
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

    return ContainerUtil.getFirstItem(ContainerUtil.filter(candidates, erlangFunction -> erlangFunction.getArity() == argsCount));
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
    return calcChildren(ErlangTypeDefinition.class,
                        ErlangTypes.ERL_TYPE_DEFINITION,
                        ErlangTypeDefinitionElementType.ARRAY_FACTORY);
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
    return calcChildren(ErlangMacrosDefinition.class,
                        ErlangTypes.ERL_MACROS_DEFINITION,
                        ErlangMacrosDefinitionElementType.ARRAY_FACTORY);
  }

  @Override
  public ErlangMacrosDefinition getMacros(@NotNull String name) {
    return myMacrosesMap.getValue().get(name);
  }

  private List<ErlangRecordDefinition> calcRecords() {
    return calcChildren(ErlangRecordDefinition.class,
                        ErlangTypes.ERL_RECORD_DEFINITION,
                        ErlangRecordDefinitionElementType.ARRAY_FACTORY);
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
    return calcChildren(ErlangInclude.class,
                        ErlangTypes.ERL_INCLUDE,
                        ErlangIncludeElementType.ARRAY_FACTORY);
  }

  private List<ErlangIncludeLib> calcIncludeLibs() {
    return calcChildren(ErlangIncludeLib.class,
                        ErlangTypes.ERL_INCLUDE_LIB,
                        ErlangIncludeLibElementType.ARRAY_FACTORY);
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

    return ContainerUtil.mapNotNull(getAttributes(), ErlangAttribute::getBehaviour);
  }

  @NotNull
  @Override
  public Collection<ErlangCallbackFunction> getOptionalCallbacks() {
    return myOptionalCallbacks.getValue();
  }

  @NotNull
  private Collection<ErlangCallbackFunction> calcOptionalCallbacks() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_CALLBACK_FUNCTION,
                               ErlangCallbackFunctionStubElementType.ARRAY_FACTORY);
    }

    List<ErlangCallbackFunction> optionalCallbacks = new ArrayList<>();
    for (ErlangAttribute attr : getAttributes()) {
      ErlangOptionalCallbacks callbacks = attr.getOptionalCallbacks();
      ErlangOptionalCallbackFunctions opts = callbacks != null ? callbacks.getOptionalCallbackFunctions() : null;
      optionalCallbacks.addAll(opts != null ? opts.getCallbackFunctionList() : ContainerUtil.emptyList());
    }
    return optionalCallbacks;
  }

  @NotNull
  @Override
  public List<ErlangSpecification> getSpecifications() {
    return mySpecificationsValue.getValue();
  }

  private List<ErlangSpecification> calcSpecifications() {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      return getChildrenByType(stub, ErlangTypes.ERL_SPECIFICATION, ErlangSpecificationElementType.ARRAY_FACTORY);
    }

    return ContainerUtil.mapNotNull(getAttributes(), ErlangAttribute::getSpecification);
  }

  @Override
  public ErlangRecordDefinition getRecord(String name) {
    return myRecordsMap.getValue().get(name);
  }

  @Nullable
  public ErlangImportFunction getImportedFunction(String name, final int arity) {
    MultiMap<String, ErlangImportFunction> importsMap = myImportsMap.getValue();
    Collection<ErlangImportFunction> importFunctions = importsMap.get(name);
    return ContainerUtil.find(importFunctions, importFunction -> arity == ErlangPsiImplUtil.getArity(importFunction));
  }

  @NotNull
  @Override
  public List<ErlangImportFunction> getImportedFunctions() {
    return myImportValue.getValue();
  }

  private List<ErlangImportFunction> calcImports() {
    ArrayList<ErlangImportFunction> result = new ArrayList<>();
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangImportDirective importDirective = attribute.getImportDirective();
      ErlangImportFunctions importFunctions = importDirective != null ? importDirective.getImportFunctions() : null;
      List<ErlangImportFunction> functions = importFunctions != null ? importFunctions.getImportFunctionList() : null;
      result.addAll(ContainerUtil.notNullize(functions));
    }
    return result;
  }

  private List<ErlangFunction> calcFunctions() {
    return calcChildren(ErlangFunction.class,
                        ErlangTypes.ERL_FUNCTION,
                        ErlangFunctionStubElementType.ARRAY_FACTORY);
  }

  private List<ErlangAttribute> calcAttributes() {
    return collectChildrenDummyAware(ErlangAttribute.class);
  }

  private List<ErlangRule> calcRules() {
    return collectChildrenDummyAware(ErlangRule.class);
  }

  @Override
  public void addDeclaredParseTransforms(@NotNull Set<String> parseTransforms) {
    ErlangFileStub stub = getStub();
    if (stub != null) {
      String fromStub = stub.getParseTransforms();
      List<String> split = fromStub != null ? StringUtil.split(fromStub, ",") : ContainerUtil.emptyList();
      parseTransforms.addAll(split);
      return;
    }
    for (ErlangAttribute attribute : getAttributes()) {
      ErlangAtomAttribute atomAttribute = attribute.getAtomAttribute();
      String attributeName = null != atomAttribute ? atomAttribute.getName() : null;
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

  @Nullable
  @Override
  public PsiElement getNameIdentifier() {
    return null; // hack for inplace rename: InplaceRefactoring#getVariable()
  }

  private <T extends StubBasedPsiElement<?>> List<T> calcChildren(@NotNull Class<T> clazz,
                                                                  @NotNull IElementType elementType,
                                                                  @NotNull ArrayFactory<T> arrayFactory) {
    ErlangFileStub stub = getStub();
    return stub != null ? getChildrenByType(stub, elementType, arrayFactory) : collectChildrenDummyAware(clazz);
  }

  @NotNull
  private <T> CachedValue<T> createCachedValue(@NotNull CachedValueProvider<T> provider) {
    return CachedValuesManager.getManager(getProject()).createCachedValue(provider, false);
  }

  @NotNull
  private <T extends PsiElement> List<T> collectChildrenDummyAware(@NotNull final Class<T> clazz) {
    final List<T> result = new ArrayList<>();
    processChildrenDummyAware(this, element -> {
      if (clazz.isInstance(element)) {
        //noinspection unchecked
        result.add((T)element);
      }
      return true;
    });
    return result;
  }

  private static void processChildrenDummyAware(ErlangFileImpl file, final Processor<PsiElement> processor) {
    new Processor<PsiElement>() {
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

  @NotNull
  private static <E extends StubBasedPsiElement<?>> List<E> getChildrenByType(@NotNull ErlangFileStub stub,
                                                                              @NotNull IElementType elementType,
                                                                              @NotNull ArrayFactory<E> arrayFactory) {
    return Arrays.asList(stub.getChildrenByType(elementType, arrayFactory));
  }

  private abstract class ValueProvider<T> implements CachedValueProvider<T> {
    @NotNull
    @Override
    public final Result<T> compute() {
      return Result.create(computeValue(), ErlangFileImpl.this);
    }

    @Nullable
    protected abstract T computeValue();
  }
}
