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

package org.intellij.erlang.editor;

import com.intellij.ide.util.projectWizard.JavaModuleBuilder;
import com.intellij.ide.util.projectWizard.ModuleBuilderListener;
import com.intellij.ide.util.projectWizard.SourcePathsBuilder;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.CompilerModuleExtension;
import com.intellij.openapi.roots.ModifiableRootModel;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import java.io.File;

/**
 * @author ignatov
 */
public class ErlangModuleBuilder extends JavaModuleBuilder implements SourcePathsBuilder, ModuleBuilderListener {
  private static final String EBIN_DIR = "ebin";
  private static final String EUNIT_DIR = ".eunit";

  @Override
  public void setupRootModel(ModifiableRootModel rootModel) throws ConfigurationException {
    addListener(this);
    super.setupRootModel(rootModel);
    final String moduleHome = getContentEntryPath();
    final CompilerModuleExtension compilerModuleExt = rootModel.getModuleExtension(CompilerModuleExtension.class);
    compilerModuleExt.inheritCompilerOutputPath(false);
    compilerModuleExt.setCompilerOutputPath(moduleHome + File.separator + EBIN_DIR);
    compilerModuleExt.setCompilerOutputPathForTests(moduleHome + File.separator + EUNIT_DIR);
  }

  @Override
  public ModuleType getModuleType() {
    return ErlangModuleType.getInstance();
  }

  @Override
  public boolean isSuitableSdk(Sdk sdk) {
    return sdk.getSdkType() == ErlangSdkType.getInstance();
  }

  @Override
  public void moduleCreated(@NotNull Module module) {
  }
}
