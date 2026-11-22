/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.utils;

import com.intellij.application.options.ModulesComboBox;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.NlsContexts;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.sdk.ErlangSystemUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ErlangUiUtil {
  private ErlangUiUtil() {
  }

  public static void installWorkingDirectoryChooser(@NotNull TextFieldWithBrowseButton on, @Nullable Project project) {
    FileChooserDescriptor descriptor = FileChooserDescriptorFactory.createSingleFolderDescriptor();
    addBrowseFolderListener(on, "Choose Working Directory", null, project, descriptor);
  }

  /**
   * Small IDEs have no Erlang module type (it needs the Java module), so any module can be chosen there.
   */
  public static void fillModules(@NotNull ModulesComboBox comboBox, @NotNull Project project) {
    if (ErlangSystemUtil.isSmallIde()) {
      comboBox.fillModules(project);
    }
    else {
      comboBox.fillModules(project, ErlangModuleType.getInstance());
    }
  }

  public static boolean isErlangModuleChoice(@NotNull Module module) {
    return ErlangSystemUtil.isSmallIde() || ModuleType.get(module) == ErlangModuleType.getInstance();
  }

  public static void addBrowseFolderListener(
    TextFieldWithBrowseButton button,
    @Nullable @NlsContexts.DialogTitle String title,
    @Nullable @NlsContexts.Label String description,
    @Nullable Project project,
    FileChooserDescriptor fileChooserDescriptor
  ) {
    button.addBrowseFolderListener(project, fileChooserDescriptor.withTitle(title).withDescription(description));
  }
}
