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
  Icon VARIABLE = PlatformIcons.VARIABLE_ICON;
}
