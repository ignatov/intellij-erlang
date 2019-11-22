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

package org.intellij.erlang.runconfig;

import com.intellij.execution.actions.ConfigurationContext;
import com.intellij.execution.actions.RunConfigurationProducer;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.util.Processor;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.ErlangCompositeElement;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangRecursiveVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public abstract class ErlangDebuggableRunConfigurationProducer<RunConfig extends ErlangRunConfigurationBase<?>> extends RunConfigurationProducer<RunConfig> {
  protected ErlangDebuggableRunConfigurationProducer(ConfigurationType configurationType) {
    super(configurationType);
  }

  @Override
  protected final boolean setupConfigurationFromContext(@NotNull RunConfig runConfig, @NotNull ConfigurationContext context, Ref<PsiElement> ref) {
    PsiElement location = ref.get();
    return location != null && location.isValid() &&
      setupConfigurationFromContextImpl(runConfig, context, location) &&
      setupDebugOptions(runConfig, context);
  }

  protected abstract boolean setupConfigurationFromContextImpl(@NotNull RunConfig runConfig,
                                                               @NotNull ConfigurationContext context,
                                                               @NotNull PsiElement target);

  @Override
  public final boolean isConfigurationFromContext(@NotNull RunConfig runConfig, ConfigurationContext context) {
    PsiElement location = context.getPsiLocation();
    return location != null && location.isValid() &&
      Comparing.equal(runConfig.getConfigurationModule().getModule(), context.getModule()) &&
      isConfigurationFromContextImpl(runConfig, context, location);
  }

  protected abstract boolean isConfigurationFromContextImpl(@NotNull RunConfig runConfig,
                                                            @NotNull ConfigurationContext context,
                                                            @NotNull PsiElement location);

  private boolean setupDebugOptions(@NotNull RunConfig runConfig, @NotNull ConfigurationContext context) {
    runConfig.setDebugOptions(createDefaultDebugOptions(context.getModule(), runConfig.isUseTestCodePath()));
    return true;
  }

  public static void updateDebugOptions(@NotNull ErlangRunConfigurationBase<?> runConfig) {
    ErlangRunConfigurationBase.ErlangDebugOptions debugOptions = runConfig.getDebugOptions();
    Module module = runConfig.getConfigurationModule().getModule();
    if (debugOptions.isAutoUpdateModulesNotToInterpret() && module != null) {
      debugOptions.setModulesNotToInterpret(getErlangModulesWithCallsToLoadNIF(module, runConfig.isUseTestCodePath()));
    }
  }

  @NotNull
  private static ErlangRunConfigurationBase.ErlangDebugOptions createDefaultDebugOptions(@Nullable Module module,
                                                                                         boolean includeTests) {
    ErlangRunConfigurationBase.ErlangDebugOptions debugOptions = new ErlangRunConfigurationBase.ErlangDebugOptions();
    debugOptions.setModulesNotToInterpret(getErlangModulesWithCallsToLoadNIF(module, includeTests));
    return debugOptions;
  }

  @NotNull
  private static Set<String> getErlangModulesWithCallsToLoadNIF(@Nullable final Module module, boolean includeTests) {
    if (module == null) return Collections.emptySet();

    // We want to process Erlang modules in current module, it's dependencies, and tests if includeTests is true
    GlobalSearchScope scope = GlobalSearchScope
      .moduleWithDependenciesAndLibrariesScope(module, includeTests)
      .intersectWith(GlobalSearchScope.moduleWithDependenciesScope(module));
    scope = GlobalSearchScope.getScopeRestrictedByFileTypes(scope, ErlangFileType.MODULE);

    final HashSet<String> modules = new HashSet<>();
    Processor<PsiFile> collector = psiFile -> {
      String moduleName = psiFile.getVirtualFile().getNameWithoutExtension();
      if (!modules.contains(moduleName) && containsErlangLoadNifCall(psiFile)) {
        modules.add(moduleName);
      }
      return true;
    };
    PsiSearchHelper.getInstance(module.getProject()).processAllFilesWithWord("load_nif", scope, collector, true);

    return modules;
  }

  private static boolean containsErlangLoadNifCall(PsiFile psiFile) {
    final Ref<Boolean> result = new Ref<>();
    psiFile.accept(new ErlangRecursiveVisitor() {
      @Override
      public void visitCompositeElement(@NotNull ErlangCompositeElement o) {
        if (!Boolean.TRUE.equals(result.get())) {
          super.visitCompositeElement(o);
        }
      }

      @Override
      public void visitFunctionCallExpression(@NotNull ErlangFunctionCallExpression functionCallExpression) {
        if (Boolean.TRUE.equals(result.get())) return;

        // we're looking for calls to erlang:load_nif/2
        if (!"load_nif".equals(functionCallExpression.getName()) ||
          functionCallExpression.getArgumentList().getExpressionList().size() != 2) {
          super.visitFunctionCallExpression(functionCallExpression);
          return;
        }

        PsiReference reference = functionCallExpression.getReference();
        PsiElement resolve = reference.resolve();
        if (resolve == null) {
          super.visitFunctionCallExpression(functionCallExpression);
          return;
        }

        boolean isErlangLoadNif = resolve == reference.getElement();
        if (!isErlangLoadNif) {
          VirtualFile virtualFile = resolve.getOriginalElement().getContainingFile().getVirtualFile();
          String moduleName = virtualFile != null ? virtualFile.getNameWithoutExtension() : null;
          isErlangLoadNif = "erlang".equals(moduleName);
        }

        if (isErlangLoadNif) {
          result.set(Boolean.TRUE);
          return;
        }

        super.visitFunctionCallExpression(functionCallExpression);
      }
    });
    return Boolean.TRUE.equals(result.get());
  }
}
