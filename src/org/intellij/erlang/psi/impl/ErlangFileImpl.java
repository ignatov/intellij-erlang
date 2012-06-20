package org.intellij.erlang.psi.impl;

import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
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
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangRule;
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
  private CachedValue<Map<String, ErlangFunction>> myNamesMap;

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
  public ErlangFunction getFunction(String name) {
    if (myNamesMap == null) {
      myNamesMap = CachedValuesManager.getManager(getProject()).createCachedValue(new CachedValueProvider<Map<String, ErlangFunction>>() {
        @Override
        public Result<Map<String, ErlangFunction>> compute() {
          Map<String, ErlangFunction> map = new THashMap<String, ErlangFunction>();
          for (ErlangFunction function : getFunctions()) {
            String name = function.getAtomName().getAtom().getText(); // todo: replace with the getName()
            if (!map.containsKey(name)) {
              map.put(name, function);
            }
          }
          return Result.create(map, ErlangFileImpl.this);
        }
      }, false);
    }
    return myNamesMap.getValue().get(name);
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
