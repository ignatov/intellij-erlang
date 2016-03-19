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

package org.intellij.erlang.icons;

import com.intellij.ide.IconProvider;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.Iconable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class ErlangIconProvider extends IconProvider implements DumbAware {
  @Override
  public Icon getIcon(@NotNull PsiElement o, @Iconable.IconFlags int flags) {
    if (o instanceof ErlangModule) return ErlangIcons.MODULE;
    else if (o instanceof ErlangFunction) return ErlangIcons.FUNCTION;
    else if (o instanceof ErlangRecordDefinition) return ErlangIcons.RECORD;
    else if (o instanceof ErlangQVar) return ErlangIcons.VARIABLE;
    else if (o instanceof ErlangMacrosDefinition) return ErlangIcons.MACROS;
    else if (o instanceof ErlangTypedExpr) return ErlangIcons.FIELD;
    else if (o instanceof ErlangTypeDefinition) return ErlangIcons.TYPE;
    else if (o instanceof ErlangAttribute) return ErlangIcons.ATTRIBUTE;
    else if (o instanceof ErlangQAtom) return ErlangIcons.ATOM;
    else if (o instanceof ErlangFile) return getIcon((ErlangFile) o);
    return null;
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
  private static ModuleType getModuleType(@NotNull ErlangFile file) {
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
