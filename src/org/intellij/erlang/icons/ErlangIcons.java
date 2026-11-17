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

package org.intellij.erlang.icons;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.IconLoader;
import com.intellij.ui.LayeredIcon;
import com.intellij.util.PlatformIcons;

import javax.swing.*;

public final class ErlangIcons {
  private static final ClassLoader classLoader = ErlangIcons.class.getClassLoader();

  public static final Icon FUNCTION = PlatformIcons.FUNCTION_ICON;
  public static final Icon ATTRIBUTE = PlatformIcons.ANNOTATION_TYPE_ICON;
  public static final Icon FUNCTION_CLAUSE = PlatformIcons.PACKAGE_LOCAL_ICON;
  public static final Icon RECORD = IconLoader.getIcon("/icons/braces.png", classLoader);
  public static final Icon MACROS = IconLoader.getIcon("/icons/macro.png", classLoader);
  public static final Icon MODULE = PlatformIcons.PACKAGE_ICON;
  public static final Icon VARIABLE = PlatformIcons.VARIABLE_ICON;
  public static final Icon FIELD = PlatformIcons.FIELD_ICON;
  public static final Icon TYPE = IconLoader.getIcon("/icons/type.png", classLoader);
  public static final Icon CALLBACK = AllIcons.Nodes.Interface;
  public static final Icon ATOM = IconLoader.getIcon("/icons/atom.png", classLoader);
  public static final Icon ERLANG_MARK = IconLoader.getIcon("/icons/erlang-mark.png", classLoader);
  public static final Icon ERLANG_SMALL = IconLoader.getIcon("/icons/erlang-small-16.png", classLoader);
  public static final Icon RECURSIVE_CALL = AllIcons.Gutter.RecursiveMethod;

  public static final Icon FILE = IconLoader.getIcon("/icons/erlang-module-16.png", classLoader);
  public static final Icon OTP_SUPERVISOR = IconLoader.getIcon("/icons/otp-supervisor-16.png", classLoader);
  public static final Icon OTP_APPLICATION = IconLoader.getIcon("/icons/otp-application-16.png", classLoader);
  public static final Icon TERMS = IconLoader.getIcon("/icons/erlang-terms-16.png", classLoader);
  public static final Icon OTP_GEN_EVENT = IconLoader.getIcon("/icons/otp-gen-event-16.png", classLoader);
  public static final Icon OTP_GEN_SERVER = IconLoader.getIcon("/icons/otp-gen-server-16.png", classLoader);
  public static final Icon OTP_GEN_FSM = IconLoader.getIcon("/icons/otp-gen-fsm-16.png", classLoader);
  public static final Icon OTP_GEN_STATEM = FILE; // todo: add a new icon
  public static final Icon OTP_APP_RESOURCE = IconLoader.getIcon("/icons/otp-app-16.png", classLoader);
  public static final Icon HEADER = IconLoader.getIcon("/icons/erlang-header-16.png", classLoader);
  public static final Icon INCLUDE_ROOT = IconLoader.getIcon("/icons/includeRoot.png", classLoader);

  public static final Icon REBAR = IconLoader.getIcon("/icons/rebar-16.png", classLoader);
  public static final Icon REBAR_MODULE_CONFLICT = AllIcons.Actions.Cancel;
  public static final Icon CONSOLE = IconLoader.getIcon("/icons/console.png", classLoader);
  public static final Icon DEBUGGER_PRIMITIVE_VALUE = AllIcons.Debugger.Db_primitive;
  public static final Icon DEBUGGER_VALUE = AllIcons.Debugger.Value;
  public static final Icon DEBUGGER_ARRAY = AllIcons.Debugger.Db_array;

  // These icons use layered implementations and must be initialized lazily to avoid initialization issues
  private static class Lazy {
    private static final Icon ERLANG_MODULE_NODE = LayeredIcon.layeredIcon(new Icon[]{PlatformIcons.FOLDER_ICON, ERLANG_MARK});
    private static final Icon EUNIT = new LayeredIcon(FILE, AllIcons.Nodes.JunitTestMark);
    private static final Icon REBAR_EUNIT = new LayeredIcon(REBAR, AllIcons.Nodes.JunitTestMark);
    private static final Icon ERLANG_CONSOLE = new LayeredIcon(CONSOLE, ERLANG_MARK);
    private static final Icon REMOTE_NODE = new LayeredIcon(AllIcons.RunConfigurations.Remote, ERLANG_MARK);
  }

  public static Icon getErlangModuleNode() {
    return Lazy.ERLANG_MODULE_NODE;
  }

  public static Icon getEunit() {
    return Lazy.EUNIT;
  }

  public static Icon getRebarEunit() {
    return Lazy.REBAR_EUNIT;
  }

  public static Icon getErlangConsole() {
    return Lazy.ERLANG_CONSOLE;
  }

  public static Icon getRemoteNode() {
    return Lazy.REMOTE_NODE;
  }

  private ErlangIcons() {
  }
}
