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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.Processor;
import gnu.trove.THashMap;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.parser.GeneratedParserUtilBase;
import org.intellij.erlang.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
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
  private CachedValue<List<ErlangMacrosDefinition>> myMacrosValue;
  private CachedValue<Map<String, ErlangMacrosDefinition>> myMacrosesMap;
  private CachedValue<List<ErlangBehaviour>> myBehavioursValue;

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
            int argsCount = function.getFunctionClauseList().get(0).getArgumentDefinitionList().getArgumentDefinitionList().size();
            Pair<String, Integer> key = Pair.create(name, argsCount);
            if (!map.containsKey(key)) {
              map.put(key, function);
            }
          }
          return Result.create(map, ErlangFileImpl.this);
        }
      }, false);
    }
    // todo: cleanup
    Map<Pair<String, Integer>, ErlangFunction> value = myFunctionsMap.getValue();
    ErlangFunction byName = value.get(Pair.create(name, argsCount));
    ErlangFunction byUnquote = byName == null ? value.get(Pair.create(StringUtil.unquoteString(name), argsCount)) : byName;
    return byUnquote == null ? value.get(Pair.create("'" + name + "'", argsCount)) : byUnquote;
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

  @NotNull
  @Override
  public List<ErlangMacrosDefinition> getMacroses() {
    if (myMacrosValue == null) {
      myMacrosValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangMacrosDefinition>>() {
        @Override
        public Result<List<ErlangMacrosDefinition>> compute() {
          return Result.create(calcMacroses(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myMacrosValue.getValue();
  }

  private List<ErlangMacrosDefinition> calcMacroses() {
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
  public ErlangMacrosDefinition getMacros(String name) {
    if (myMacrosesMap == null) {
      myMacrosesMap = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangMacrosDefinition>>() {
        @Override
        public Result<Map<String, ErlangMacrosDefinition>> compute() {
          Map<String, ErlangMacrosDefinition> map = new THashMap<String, ErlangMacrosDefinition>();
          for (ErlangMacrosDefinition macros : getMacroses()) {
            ErlangMacrosName mName = macros.getMacrosName();
            if (mName == null) continue;
            String macrosName = mName.getText();
            if (!map.containsKey(macrosName)) {
              map.put(macrosName, macros);
            }
          }
          return Result.create(map, ErlangFileImpl.this);
        }
      }, false);
    }
    // todo: cleanup
    Map<String, ErlangMacrosDefinition> value = myMacrosesMap.getValue();
    ErlangMacrosDefinition byName = value.get(name);
    ErlangMacrosDefinition byUnquote = byName == null ? value.get(StringUtil.unquoteString(name)) : byName;
    return byUnquote == null ? value.get("'" + name + "'") : byUnquote;
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

  @NotNull
  @Override
  public List<ErlangBehaviour> getBehaviours() {
    if (myBehavioursValue == null) {
      myBehavioursValue = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<List<ErlangBehaviour>>() {
        @Override
        public Result<List<ErlangBehaviour>> compute() {
          return Result.create(calcBehaviours(), ErlangFileImpl.this);
        }
      }, false);
    }
    return myBehavioursValue.getValue();
  }

  private List<ErlangBehaviour> calcBehaviours() {
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
    // todo: cleanup
    Map<String, ErlangRecordDefinition> value = myRecordsMap.getValue();
    ErlangRecordDefinition byName = value.get(name);
    ErlangRecordDefinition byUnquote = byName == null ? value.get(StringUtil.unquoteString(name)) : byName;
    return byUnquote == null ? value.get("'" + name + "'") : byUnquote;
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

  @Override
  @Nullable
  protected Icon getElementIcon(@IconFlags int flags) {
    if (!this.isValid()) return null;
    int elementFlags = (flags & ICON_FLAG_READ_STATUS) != 0 && !isWritable() ? FLAGS_LOCKED : 0;
    ErlangModule module = ErlangPsiImplUtil.getModule(this);
    Icon icon = module != null && StringUtil.endsWith(module.getName(), "_tests") ? ErlangIcons.EUNIT : getModuleType().icon;
    return createLayeredIcon(this, icon, elementFlags);
  }

  @NotNull
  public ModuleType getModuleType() {
    ModuleType type = ModuleType.REGULAR;
    for (ErlangBehaviour behaviour : this.getBehaviours()) {
      type = ModuleType.getType(behaviour.getName());
      if (type != ModuleType.REGULAR) break;
    }
    return type;
  }

  enum ModuleType {
    REGULAR(),
    OTP_APPLICATION("application", ErlangIcons.OTP_APPLICATION),
    OTP_SUPERVISOR("supervisor", ErlangIcons.OTP_SUPERVISOR),
    OTP_GEN_SERVER("gen_server", ErlangIcons.OTP_GEN_SERVER),
    OTP_GEN_FSM("gen_fsm", ErlangIcons.OTP_GEN_FSM),
    OTP_GEN_EVENT("gen_event", ErlangIcons.OTP_GEN_EVENT);
    public final String behaviourName;
    public final Icon icon;

    ModuleType(String behaviourName, Icon icon) {
      this.behaviourName = behaviourName;
      this.icon = icon;
    }

    ModuleType() {
      this.behaviourName = null;
      this.icon = ErlangIcons.FILE;
    }

    @NotNull
    public static ModuleType getType(@NotNull String behaviourName) {
      for (ModuleType type : ModuleType.values())
        if (StringUtil.equals(type.behaviourName, behaviourName)) return type;
      return REGULAR;
    }
  }
}
