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
import java.util.ArrayList;
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

  @NotNull private final Project myProject;
  @NotNull private final VirtualFile myVirtualFile;
  @Nullable private List<OrderEntry> myOrderEntries;
  @Nullable private List<String> myExternalDocUrls;

  protected AbstractSdkDocProvider(@NotNull Project project, @NotNull VirtualFile virtualFile) {
    myProject = project;
    myVirtualFile = virtualFile;
  }

  @NotNull
  @Override
  public List<String> getExternalDocUrls() {
    if (myExternalDocUrls == null) {
      myExternalDocUrls = getHttpUrls(getOrderEntries(), myVirtualFile, getInDocRef());
    }
    return myExternalDocUrls;
  }

  @Nullable
  @Override
  public String getDocText() {
    final List<String> fileUrls = getFileUrls(getOrderEntries(), myVirtualFile);
    final List<String> httpUrls = getExternalDocUrls();
    final List<String> urls = new ArrayList<String>(fileUrls.size() + httpUrls.size());
    urls.addAll(fileUrls);
    urls.addAll(httpUrls);
    for (String urlString : urls) {
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

  @NotNull
  private List<OrderEntry> getOrderEntries() {
    if (myOrderEntries == null) {
      final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();
      myOrderEntries = fileIndex.getOrderEntriesForFile(myVirtualFile);
    }
    return myOrderEntries;
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

  @NotNull
  protected abstract String getInDocRef();

  protected abstract boolean isDocEnd(@NotNull String line);

  protected abstract boolean isDocBegin(@NotNull String line);

  @NotNull
  private static List<String> getHttpUrls(@NotNull List<OrderEntry> orderEntries,
                                          @NotNull VirtualFile virtualFile,
                                          @NotNull String inDocRef) {
    for (OrderEntry orderEntry : orderEntries) {
      final String[] docRootUrls = JavadocOrderRootType.getUrls(orderEntry);
      final String sdkHttpDocRelPath = httpDocRelPath(virtualFile);
      final List<String> httpUrls = PlatformDocumentationUtil.getHttpRoots(
        docRootUrls, sdkHttpDocRelPath + inDocRef);
      if (httpUrls != null) {
        return httpUrls;
      }
    }
    return Collections.emptyList();
  }

  @NotNull
  private static List<String> getFileUrls(@NotNull List<OrderEntry> orderEntries,
                                          @NotNull VirtualFile virtualFile) {
    List<String> fileUrls = null;
    for (OrderEntry orderEntry : orderEntries) {
      final VirtualFile[] docRootFiles = orderEntry.getFiles(JavadocOrderRootType.getInstance());
      final String sdkHttpDocRelPath = httpDocRelPath(virtualFile);
      for (VirtualFile docRootFile : docRootFiles) {
        if (docRootFile.isInLocalFileSystem()) {
          if (fileUrls == null) {
            fileUrls = new ArrayList<String>();
          }
          fileUrls.add(docRootFile.getUrl() + "/" + sdkHttpDocRelPath);
        }
      }
    }
    return fileUrls != null ? fileUrls : Collections.<String>emptyList();
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
      else if (url.getProtocol().equals("file")) {
        return createFileReader(url);
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

  @Nullable
  private static BufferedReader createFileReader(@NotNull URL url) {
    try {
      final InputStreamReader stream = new InputStreamReader(url.openStream());
      return new BufferedReader(stream);
    } catch (IOException e) {
      return null;
    }
  }

  @NotNull
  private static String decorateRetrievedHtml(@NotNull String retrievedHtml) {
    return "<html>\n" + HTTP_STYLE + "<body>\n" + retrievedHtml + "</body></html>\n";
  }
}
