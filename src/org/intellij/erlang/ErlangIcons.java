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

import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.LayeredIcon;
import com.intellij.util.PlatformIcons;

import javax.swing.*;

public interface ErlangIcons {
  Icon FUNCTION = PlatformIcons.FUNCTION_ICON;
  Icon ATTRIBUTE = PlatformIcons.ANNOTATION_TYPE_ICON;
  Icon FUNCTION_CLAUSE = PlatformIcons.PACKAGE_LOCAL_ICON;
  Icon RECORD = IconLoader.getIcon("/icons/braces.png");
  Icon MACROS = IconLoader.getIcon("/icons/macro.png");
  Icon MODULE = PlatformIcons.PACKAGE_ICON;
  Icon VARIABLE = PlatformIcons.VARIABLE_ICON;
  Icon FIELD = PlatformIcons.FIELD_ICON;
  Icon TYPE = IconLoader.getIcon("/icons/type.png");
  Icon CALLBACK = AllIcons.Nodes.Interface;
  Icon ERLANG_MARK = IconLoader.getIcon("/icons/erlang-mark.png");
  Icon ERLANG_MODULE_NODE = new LayeredIcon(PlatformIcons.FOLDER_ICON, ERLANG_MARK);
  Icon ERLANG_BIG = IconLoader.getIcon("/icons/erlang-big.png");
  Icon RECURSIVE_CALL = AllIcons.Gutter.RecursiveMethod;

  Icon FILE = IconLoader.getIcon("/icons/erlang-module-16.png");
  Icon OTP_SUPERVISOR = IconLoader.getIcon("/icons/otp-supervisor-16.png");
  Icon OTP_APPLICATION = IconLoader.getIcon("/icons/otp-application-16.png");
  Icon TERMS = IconLoader.getIcon("/icons/erlang-terms-16.png");
  Icon OTP_GEN_EVENT = IconLoader.getIcon("/icons/otp-gen-event-16.png");
  Icon OTP_GEN_SERVER = IconLoader.getIcon("/icons/otp-gen-server-16.png");
  Icon OTP_GEN_FSM = IconLoader.getIcon("/icons/otp-gen-fsm-16.png");
  Icon OTP_APP_RESOURCE = IconLoader.getIcon("/icons/otp-app-16.png");
  Icon EUNIT = new LayeredIcon(FILE, AllIcons.Nodes.JunitTestMark);
  Icon HEADER = IconLoader.getIcon("/icons/erlang-header-16.png");
  Icon INCLUDE_ROOT = IconLoader.getIcon("/icons/includeRoot.png");

  Icon REBAR = IconLoader.getIcon("/icons/rebar-16.png");
  Icon REBAR_MODULE_CONFLICT = IconLoader.getIcon("/icons/rebar-module-conflict-16.png");
  Icon REBAR_EUNIT = new LayeredIcon(REBAR, AllIcons.Nodes.JunitTestMark);
  Icon ERLANG_CONSOLE = IconLoader.getIcon("/icons/erlang-console.png");

  Icon DEBUGGER_PRIMITIVE_VALUE = AllIcons.Debugger.Db_primitive;
  Icon DEBUGGER_VALUE = AllIcons.Debugger.Value;
  Icon DEBUGGER_ARRAY = AllIcons.Debugger.Db_array;
}
