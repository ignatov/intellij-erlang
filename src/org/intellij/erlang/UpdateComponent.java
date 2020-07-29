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

package org.intellij.erlang;

import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.PermanentInstallationID;
import com.intellij.openapi.components.BaseComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.EditorFactory;
import com.intellij.openapi.editor.event.EditorFactoryEvent;
import com.intellij.openapi.editor.event.EditorFactoryListener;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.io.HttpRequests;
import org.jdom.JDOMException;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.concurrent.TimeUnit;

public class UpdateComponent implements BaseComponent, Disposable {
  private static final String PLUGIN_ID = "org.jetbrains.erlang";
  private static final String KEY = "erlang.last.update.timestamp";
  private static final Logger LOG = Logger.getInstance(UpdateComponent.class);
  private final EditorFactoryListener myListener = new EditorFactoryListener() {
    @Override
    public void editorCreated(@NotNull EditorFactoryEvent event) {
      Document document = event.getEditor().getDocument();
      VirtualFile file = FileDocumentManager.getInstance().getFile(document);
      if (file != null && file.getFileType() instanceof ErlangFileType) {
        checkForUpdates();
      }
    }
  };

  @Override
  public void initComponent() {
    if (!ApplicationManager.getApplication().isUnitTestMode()) {
      EditorFactory.getInstance().addEditorFactoryListener(myListener, this);
    }
  }

  @NotNull
  @SuppressWarnings("ConstantConditions")
  private static IdeaPluginDescriptor getPlugin() {
    return PluginManagerCore.getPlugin(PluginId.getId(PLUGIN_ID));
  }

  private static void checkForUpdates() {
    PropertiesComponent propertiesComponent = PropertiesComponent.getInstance();
    long lastUpdate = propertiesComponent.getLong(KEY, 0);
    if (lastUpdate == 0 || System.currentTimeMillis() - lastUpdate > TimeUnit.DAYS.toMillis(1)) {
      ApplicationManager.getApplication().executeOnPooledThread(() -> {
        try {
          String buildNumber = ApplicationInfo.getInstance().getBuild().asString();
          String pluginVersion = getPlugin().getVersion();
          String pluginId = getPlugin().getPluginId().getIdString();
          String os = URLEncoder.encode(SystemInfo.OS_NAME + " " + SystemInfo.OS_VERSION, CharsetToolkit.UTF8);
          String uid = PermanentInstallationID.get();
          final String url =
            "https://plugins.jetbrains.com/plugins/list" +
            "?pluginId=" + pluginId +
            "&build=" + buildNumber +
            "&pluginVersion=" + pluginVersion +
            "&os=" + os +
            "&uuid=" + uid;
          PropertiesComponent.getInstance().setValue(KEY, String.valueOf(System.currentTimeMillis()));
          HttpRequests.request(url).connect(
            request -> {
              try {
                JDOMUtil.load(request.getReader());
                LOG.info((request.isSuccessful() ? "Successful" : "Unsuccessful") + " update: " + url);
              }
              catch (JDOMException e) {
                LOG.warn(e);
              }
              return null;
            }
          );
        }
        catch (IOException e) {
          LOG.warn(e);
        }
      });
    }
  }

  @Override
  public void disposeComponent() {
  }

  @NotNull
  @Override
  public String getComponentName() {
    return getClass().getName();
  }

  @Override
  public void dispose() {
    disposeComponent();
  }
}