/*
 * Copyright 2013 Sergey Ignatov
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

package org.intellij.erlang.documentation;

import com.intellij.codeInsight.documentation.PlatformDocumentationUtil;
import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ResourceUtil;
import com.intellij.util.net.HttpConfigurable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Collections;
import java.util.List;

abstract class AbstractSdkDocProvider implements ElementDocProvider {
  static final String HTTP_STYLE;
  static {
    final String css;
    try {
       css = ResourceUtil.loadText(ResourceUtil.getResource(
         AbstractSdkDocProvider.class, "/documentation", "erlang-sdk-doc.css"));
    } catch (IOException e) {
      throw (AssertionError) (new AssertionError().initCause(e));
    }
    HTTP_STYLE = "<style type=\"text/css\">\n" + css + "</style>\n";
  }

  @NotNull private final List<String> myExternalDocUrls;

  protected AbstractSdkDocProvider(@NotNull Project project, @NotNull VirtualFile virtualFile, @NotNull String inDocRef) {
    myExternalDocUrls = getUrls(project, virtualFile, inDocRef);
  }

  @NotNull
  @Override
  public List<String> getExternalDocUrls() {
    return myExternalDocUrls;
  }

  @Nullable
  @Override
  public String getDocText() {
    for (String urlString : myExternalDocUrls) {
      final BufferedReader reader = createReader(urlString);
      if (reader == null) {
        continue;
      }
      try {
        final String retrievedHtml = retrieveDoc(reader);
        if (retrievedHtml != null) {
          return decorateRetrievedHtml(retrievedHtml);
        }
      } finally {
        try {
          reader.close();
        } catch (IOException e) { // Ignore
        }
      }
    }
    return null;
  }

  @Nullable
  private String retrieveDoc(@NotNull BufferedReader reader) {
    try {
      String line;
      boolean functionDocFound = false;
      while ((line = reader.readLine()) != null) {
        if (isDocBegin(line)) {
          functionDocFound = true;
          break;
        }
      }
      if (!functionDocFound) {
        return null;
      }
      final StringBuilder builder = new StringBuilder(1024);
      builder.append(line);
      while ((line = reader.readLine()) != null && !isDocEnd(line)) {
        builder.append(line).append("\n");
      }
      return builder.toString();
    } catch (IOException e) { // Ignore
    } finally {
      try {
        reader.close();
      } catch (IOException e) { // Ignore
      }
    }
    return null;
  }

  protected abstract boolean isDocEnd(@NotNull String line);

  protected abstract boolean isDocBegin(@NotNull String line);

  @NotNull
  private static List<String> getUrls(@NotNull Project project, @NotNull VirtualFile virtualFile, @NotNull String inDocRef) {
    final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(project).getFileIndex();
    final List<OrderEntry> orderEntries = fileIndex.getOrderEntriesForFile(virtualFile);
    for (OrderEntry orderEntry : orderEntries) {
      final String[] docRootUrls = JavadocOrderRootType.getUrls(orderEntry);
      final String sdkHttpDocRelPath = httpDocRelPath(virtualFile);
      final List<String> docUrls = PlatformDocumentationUtil.getHttpRoots(docRootUrls, sdkHttpDocRelPath + inDocRef);
      if (docUrls != null) {
        return docUrls;
      }
    }
    return Collections.emptyList();
  }

  @Nullable
  private static BufferedReader createReader(@NotNull String urlString) {
    try {
      final URL url = BrowserUtil.getURL(urlString);
      if (url == null) {
        return null;
      }
      if (url.getProtocol().equals("http")) {
        return createHttpReader(url);
      }
    } catch (Exception e) { // Ignore
    }
    return null;
  }

  @Nullable
  private static String httpDocRelPath(@NotNull VirtualFile virtualFile) {
    final String appDirName = virtualFile.getParent().getParent().getName();
    final String prefix;
    if (appDirName.startsWith("erts")) {
      prefix = "";
    }
    else {
      prefix = "lib/";
    }
    return prefix + appDirName + "/doc/html/" + virtualFile.getNameWithoutExtension() + ".html";
  }

  @NotNull
  private static BufferedReader createHttpReader(@NotNull URL url) throws IOException {
    final HttpConfigurable httpConfigurable = HttpConfigurable.getInstance();
    httpConfigurable.prepareURL(url.toString());
    final URLConnection urlConnection = url.openConnection();
    final String contentEncoding = urlConnection.getContentEncoding();
    final InputStream inputStream = urlConnection.getInputStream();
    //noinspection IOResourceOpenedButNotSafelyClosed
    final InputStreamReader inputStreamReader = contentEncoding != null
      ? new InputStreamReader(inputStream, contentEncoding)
      : new InputStreamReader(inputStream);
    return new BufferedReader(inputStreamReader);
  }

  @NotNull
  private static String decorateRetrievedHtml(@NotNull String retrievedHtml) {
    return "<html>\n" + HTTP_STYLE + "<body>\n" + retrievedHtml + "</body></html>\n";
  }
}
