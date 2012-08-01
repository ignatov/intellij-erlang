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

import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.ModuleTypeManager;
import org.intellij.erlang.ErlangIcons;

import javax.swing.*;

/**
 * @author ignatov
 */
public class ErlangModuleType extends ModuleType<ErlangModuleBuilder> {
  private static final String MODULE_TYPE_ID = "ERLANG_MODULE";

  public ErlangModuleType() {
    super(MODULE_TYPE_ID);
  }

  public static ErlangModuleType getInstance() {
    return (ErlangModuleType) ModuleTypeManager.getInstance().findByID(MODULE_TYPE_ID);
  }

  @Override
  public ErlangModuleBuilder createModuleBuilder() {
    return new ErlangModuleBuilder();
  }

  @Override
  public String getName() {
    return "Erlang Module";
  }

  @Override
  public String getDescription() {
    return "Erlang modules are used for developing <b>Erlang</b> applications.";
  }

  @Override
  public Icon getBigIcon() {
    return ErlangIcons.ERLANG_BIG;
  }

  @Override
  public Icon getNodeIcon(boolean isOpened) {
    return isOpened ? ErlangIcons.ERLANG_MODULE_NODE_OPEN : ErlangIcons.ERLANG_MODULE_NODE;
  }
}
