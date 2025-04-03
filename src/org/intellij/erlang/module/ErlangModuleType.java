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

package org.intellij.erlang.module;

import com.intellij.ide.util.projectWizard.ModuleWizardStep;
import com.intellij.ide.util.projectWizard.ProjectJdkForModuleStep;
import com.intellij.ide.util.projectWizard.WizardContext;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleTypeManager;
import com.intellij.openapi.roots.ui.configuration.ModulesProvider;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ErlangModuleType extends ModuleType<ErlangModuleBuilder> {
  private static final String MODULE_TYPE_ID = "ERLANG_MODULE";

  public ErlangModuleType() {
    super(MODULE_TYPE_ID);
  }

  public static ErlangModuleType getInstance() {
    return (ErlangModuleType) ModuleTypeManager.getInstance().findByID(MODULE_TYPE_ID);
  }

  @NotNull
  @Override
  public ErlangModuleBuilder createModuleBuilder() {
    return new ErlangModuleBuilder();
  }

  @NotNull
  @Override
  public String getName() {
    return "Erlang Module";
  }

  @NotNull
  @Override
  public String getDescription() {
    return "Erlang modules are used for developing <b>Erlang</b> applications.";
  }

  @Override
  public @NotNull Icon getNodeIcon(boolean isOpened) {
    return ErlangIcons.getErlangModuleNode();
  }

  @NotNull
  @Override
  public ModuleWizardStep @NotNull [] createWizardSteps(@NotNull WizardContext wizardContext,
                                                        @NotNull final ErlangModuleBuilder moduleBuilder,
                                                        @NotNull ModulesProvider modulesProvider) {
    return new ModuleWizardStep[]{new ProjectJdkForModuleStep(wizardContext, ErlangSdkType.getInstance()) {
      public void updateDataModel() {
        super.updateDataModel();
        moduleBuilder.setModuleJdk(getJdk());
      }
    }};
  }
}
