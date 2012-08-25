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
  Icon FILE = IconLoader.getIcon("/icons/erlang-16-7-2.png");
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
}
