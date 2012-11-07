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

package org.intellij.erlang;

import com.intellij.openapi.util.IconLoader;
import com.intellij.util.PlatformIcons;

import javax.swing.*;

/**
 * @author ignatov
 */
public interface ErlangIcons {
  Icon FUNCTION = PlatformIcons.FUNCTION_ICON;
  Icon ATTRIBUTE = PlatformIcons.ANNOTATION_TYPE_ICON;
  Icon FUNCTION_CLAUSE = PlatformIcons.PACKAGE_LOCAL_ICON;
  Icon RECORD = IconLoader.getIcon("/icons/braces.png");
  Icon MACROS = IconLoader.getIcon("/icons/macro.png");
  Icon MODULE = PlatformIcons.PACKAGE_ICON;
  Icon VARIABLE = PlatformIcons.VARIABLE_ICON;
  Icon FIELD = PlatformIcons.FIELD_ICON;
  Icon ERLANG_MODULE_NODE = IconLoader.getIcon("/icons/erlang-module-with-icon.png");
  Icon ERLANG_MODULE_NODE_OPEN = IconLoader.getIcon("/icons/erlang-module-with-icon-open.png");
  Icon ERLANG_BIG = IconLoader.getIcon("/icons/erlang-big.png");
  Icon RECURSIVE_CALL = IconLoader.getIcon("/icons/recursiveMethod.png"); // todo: use AllIcons.Gutter.RecursiveMethod after LEDA migration

  Icon FILE = IconLoader.getIcon("/icons/erlang-module-16.png");
  Icon OTP_SUPERVISOR = IconLoader.getIcon("/icons/otp-supervisor-16.png");
  Icon OTP_APPLICATION = IconLoader.getIcon("/icons/otp-application-16.png");
  Icon TERMS = IconLoader.getIcon("/icons/erlang-terms-16.png");
  Icon OTP_GEN_EVENT = IconLoader.getIcon("/icons/otp-gen-event-16.png");
  Icon OTP_GEN_SERVER = IconLoader.getIcon("/icons/otp-gen-server-16.png");
  Icon OTP_GEN_FSM = IconLoader.getIcon("/icons/otp-gen-fsm-16.png");
  Icon OTP_APP_RESOURCE = IconLoader.getIcon("/icons/otp-app-16.png");
  Icon EUNIT = IconLoader.getIcon("/icons/erlang-eunit-16.png");
  Icon HEADER = IconLoader.getIcon("/icons/erlang-header-16.png");
}
