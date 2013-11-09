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

package org.intellij.erlang;

import com.intellij.ide.IconProvider;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.Iconable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.ErlangBehaviour;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

/**
 * @author ignatov
 */
public class ErlangIconProvider extends IconProvider implements DumbAware {
  @Override
  public Icon getIcon(@NotNull PsiElement element, @Iconable.IconFlags int flags) {
    return element instanceof ErlangFile ? getIcon((ErlangFile) element) : null;
  }

  @Nullable
  public static Icon getIcon(@NotNull ErlangFile file) {
    if (!file.isValid()) return null;
    VirtualFile virtualFile = file.getViewProvider().getVirtualFile();
    FileType fileType = virtualFile.getFileType();
    if (ErlangFileType.MODULE == fileType) {
      ErlangModule module = file.getModule();
      boolean isEunit = module != null && ErlangPsiImplUtil.isEunitTestFile(file);
      return isEunit ? ErlangIcons.EUNIT : getModuleType(file).icon;
    }
    return fileType.getIcon();
  }

  @NotNull
  public static ModuleType getModuleType(@NotNull ErlangFile file) {
    ModuleType type = ModuleType.REGULAR;
    for (ErlangBehaviour behaviour : file.getBehaviours()) {
      type = ModuleType.getType(behaviour.getName());
      if (type != ModuleType.REGULAR) break;
    }
    return type;
  }

  public enum ModuleType {
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
