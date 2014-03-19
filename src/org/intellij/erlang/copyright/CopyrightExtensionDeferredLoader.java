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

package org.intellij.erlang.copyright;

import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManager;
import com.intellij.ide.plugins.cl.PluginClassLoader;
import com.intellij.openapi.components.ApplicationComponent;
import com.intellij.openapi.extensions.ExtensionPoint;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.fileTypes.FileTypeExtensionPoint;
import com.intellij.util.PathUtil;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.net.URL;

public class CopyrightExtensionDeferredLoader implements ApplicationComponent {
  private static final String COPYRIGHTS_PROVIDER_CLASS_NAME = "org.intellij.erlang.copyright.UpdateErlangCopyrightsProvider";

  @Override
  public void initComponent() {
    IdeaPluginDescriptor plugin = PluginManager.getPlugin(PluginId.getId("com.intellij.copyright"));
    if (plugin != null && plugin.isEnabled() && addThisPluginsJarPathTo(plugin)) {
      FileTypeExtensionPoint fileTypeEp = new FileTypeExtensionPoint();
      fileTypeEp.filetype = "Erlang";
      fileTypeEp.implementationClass = COPYRIGHTS_PROVIDER_CLASS_NAME;
      fileTypeEp.setPluginDescriptor(plugin);
      ExtensionPoint<Object> copyrightEP = Extensions.getRootArea().getExtensionPoint("com.intellij.copyright.updater");
      copyrightEP.registerExtension(fileTypeEp);
    }
  }

  @Override
  public void disposeComponent() {

  }

  @NotNull
  @Override
  public String getComponentName() {
    return "Erlang.CopyrightExtensionDeferredLoader";
  }

  private static boolean addThisPluginsJarPathTo(IdeaPluginDescriptor copyrightPlugin) {
    ClassLoader copyrightPluginClassLoader = copyrightPlugin.getPluginClassLoader();
    if (!(copyrightPluginClassLoader instanceof PluginClassLoader)) return false;
    try {
      URL pluginJarUrl = new File(PathUtil.getJarPathForClass(CopyrightExtensionDeferredLoader.class)).toURI().toURL();
      ((PluginClassLoader) copyrightPluginClassLoader).addURL(pluginJarUrl);
      Class<?> aClass = copyrightPluginClassLoader.loadClass(COPYRIGHTS_PROVIDER_CLASS_NAME);
      if (aClass == null) return false;
    } catch (Exception e) {
      //noinspection CallToPrintStackTrace
      e.printStackTrace();
      return false;
    }
    return true;
  }
}
