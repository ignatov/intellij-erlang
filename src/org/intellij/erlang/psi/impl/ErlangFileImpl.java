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

package org.intellij.erlang.psi.impl;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.Processor;
import gnu.trove.THashMap;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.parser.GeneratedParserUtilBase;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author ignatov
 */
public class ErlangFileImpl extends PsiFileBase implements ErlangFile {
  public ErlangFileImpl(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, ErlangLanguage.INSTANCE);
  }

  private CachedValue<List<ErlangRule>> myRulesValue;
  private CachedValue<List<ErlangFunction>> myFunctionValue;
  private CachedValue<List<ErlangAttribute>> myAttributeValue;
  private CachedValue<List<ErlangRecordDefinition>> myRecordValue;
  private CachedValue<List<ErlangInclude>> myIncludeValue;
  private CachedValue<Map<Pair<String, Integer>, ErlangFunction>> myFunctionsMap;
  private CachedValue<Map<String, ErlangRecordDefinition>> myRecordsMap;

  @NotNull
  @Override
  public FileType getFileType() {
    return ErlangFileType.INSTANCE;
  }

  @NotNull
  @Override
  public List<ErlangRule> getRules() {
    if (myRulesValue == null) {
      myRulesValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangRule>>() {
        @Override
        public Result<List<ErlangRule>> compute() {
          return Result.create(calcRules(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myRulesValue.getValue();
  }

  @NotNull
  @Override
  public List<ErlangAttribute> getAttributes() {
    if (myAttributeValue == null) {
      myAttributeValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangAttribute>>() {
        @Override
        public Result<List<ErlangAttribute>> compute() {
          return Result.create(calcAttributes(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myAttributeValue.getValue();
  }

  @NotNull
  @Override
  public List<ErlangFunction> getFunctions() {
    if (myFunctionValue == null) {
      myFunctionValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangFunction>>() {
        @Override
        public Result<List<ErlangFunction>> compute() {
          return Result.create(calcFunctions(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myFunctionValue.getValue();
  }

  @Nullable
  @Override
  public ErlangFunction getFunction(String name, int argsCount) {
    if (myFunctionsMap == null) {
      myFunctionsMap = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<Pair<String, Integer>, ErlangFunction>>() {
        @Override
        public Result<Map<Pair<String, Integer>, ErlangFunction>> compute() {
          Map<Pair<String, Integer>, ErlangFunction> map = new THashMap<Pair<String, Integer>, ErlangFunction>();
          for (ErlangFunction function : getFunctions()) {
            String name = function.getName();
            int argsCount = function.getFunctionClauseList().get(0).getArgumentDefinitionList().size();
            Pair<String, Integer> key = new Pair<String, Integer>(name, argsCount);
            if (!map.containsKey(key)) {
              map.put(key, function);
            }
          }
          return Result.create(map, ErlangFileImpl.this);
        }
      }, false);
    }
    return myFunctionsMap.getValue().get(new Pair<String, Integer>(name, argsCount));
  }

  @NotNull
  @Override
  public List<ErlangRecordDefinition> getRecords() {
    if (myRecordValue == null) {
      myRecordValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangRecordDefinition>>() {
        @Override
        public Result<List<ErlangRecordDefinition>> compute() {
          return Result.create(calcRecords(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myRecordValue.getValue();
  }

  private List<ErlangRecordDefinition> calcRecords() {
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
    if (myIncludeValue == null) {
      myIncludeValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangInclude>>() {
        @Override
        public Result<List<ErlangInclude>> compute() {
          return Result.create(calcIncludes(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myIncludeValue.getValue();
  }

  private List<ErlangInclude> calcIncludes() {
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

  @Override
  public ErlangRecordDefinition getRecord(String name) {
    if (myRecordsMap == null) {
      myRecordsMap = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangRecordDefinition>>() {
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
    }
    return myRecordsMap.getValue().get(name);
  }

  private List<ErlangFunction> calcFunctions() {
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

  private static boolean processChildrenDummyAware(PsiElement element, final Processor<PsiElement> processor) {
    return new Processor<PsiElement>() {
      @Override
      public boolean process(PsiElement psiElement) {
        for (PsiElement child = psiElement.getFirstChild(); child != null; child = child.getNextSibling()) {
          if (child instanceof GeneratedParserUtilBase.DummyBlock) {
            if (!process(child)) return false;
          } else if (!processor.process(child)) return false;
        }
        return true;
      }
    }.process(element);
  }
}
