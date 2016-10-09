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

package org.intellij.erlang.documentation;

import com.intellij.codeInsight.documentation.PlatformDocumentationUtil;
import com.intellij.ide.BrowserUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.CharsetToolkit;
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
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.codeInsight.documentation.DocumentationManagerProtocol.PSI_ELEMENT_PROTOCOL;

abstract class ErlangSdkDocProviderBase implements ElementDocProvider {
  private static final Pattern PATTERN_HREF = Pattern.compile("<a href=\"(.*?)\">");
  private static final Pattern PATTERN_EVALUATED_LINK = Pattern.compile("javascript:erlhref\\('.*?','.*?','(.*?)'\\);");
  private static final Pattern PATTERN_EXTERNAL_LINK = Pattern.compile("(.*)\\.html#(.*)");

  static final String HTTP_STYLE;
  static {
    String css;
    try {
       css = ResourceUtil.loadText(ResourceUtil.getResource(
         ErlangSdkDocProviderBase.class, "/documentation", "erlang-sdk-doc.css"));
    } catch (IOException e) {
      throw (AssertionError) new AssertionError().initCause(e);
    }
    HTTP_STYLE = "<style type=\"text/css\">\n" + css + "</style>\n";
  }

  @NotNull private final Project myProject;
  @NotNull private final VirtualFile myVirtualFile;
  @Nullable private List<OrderEntry> myOrderEntries;
  @Nullable private List<String> myExternalDocUrls;

  protected ErlangSdkDocProviderBase(@NotNull Project project, @NotNull VirtualFile virtualFile) {
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
    List<String> fileUrls = getFileUrls(getOrderEntries(), myVirtualFile);
    List<String> httpUrls = getExternalDocUrls();
    List<String> urls = new ArrayList<>(fileUrls.size() + httpUrls.size());
    urls.addAll(fileUrls);
    urls.addAll(httpUrls);
    for (String urlString : urls) {
      BufferedReader reader = createReader(urlString);
      if (reader == null) {
        continue;
      }
      try {
        String retrievedHtml = retrieveDoc(reader);
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
      ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();
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
      StringBuilder builder = new StringBuilder(1024);
      appendCorrectedLine(builder, line);
      while ((line = reader.readLine()) != null && !isDocEnd(line)) {
        appendCorrectedLine(builder, line);
        builder.append("\n");
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
  private String appendCorrectedLine(@NotNull StringBuilder builder, @NotNull String line) {
    Matcher matcher = PATTERN_HREF.matcher(line);
    int lastCopiedChar = 0;
    while (matcher.find()) {
      MatchResult matchResult = matcher.toMatchResult();
      builder.append(line.substring(lastCopiedChar, matchResult.start()));
      String linkHref = matchResult.group(1);
      String convertedLink = convertLink(linkHref);
      builder.append("<a href=\"")
        .append(convertedLink)
        .append("\">");
      lastCopiedChar = matchResult.end();
    }
    builder.append(line.substring(lastCopiedChar, line.length()));
    return line;
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
      String[] docRootUrls = JavadocOrderRootType.getUrls(orderEntry);
      String sdkHttpDocRelPath = httpDocRelPath(virtualFile);
      List<String> httpUrls = PlatformDocumentationUtil.getHttpRoots(
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
      VirtualFile[] docRootFiles = orderEntry.getFiles(JavadocOrderRootType.getInstance());
      String sdkHttpDocRelPath = httpDocRelPath(virtualFile);
      for (VirtualFile docRootFile : docRootFiles) {
        if (docRootFile.isInLocalFileSystem()) {
          if (fileUrls == null) {
            fileUrls = new ArrayList<>();
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
      URL url = BrowserUtil.getURL(urlString);
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
    String appDirName = virtualFile.getParent().getParent().getName();
    String prefix;
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
    HttpConfigurable httpConfigurable = HttpConfigurable.getInstance();
    httpConfigurable.prepareURL(url.toString());
    URLConnection urlConnection = url.openConnection();
    String contentEncoding = urlConnection.getContentEncoding();
    InputStream inputStream = urlConnection.getInputStream();
    //noinspection IOResourceOpenedButNotSafelyClosed
    InputStreamReader inputStreamReader = contentEncoding != null
      ? new InputStreamReader(inputStream, contentEncoding)
      : new InputStreamReader(inputStream);
    return new BufferedReader(inputStreamReader);
  }

  @Nullable
  private static BufferedReader createFileReader(@NotNull URL url) {
    try {
      //noinspection IOResourceOpenedButNotSafelyClosed
      InputStreamReader stream = new InputStreamReader(url.openStream(), CharsetToolkit.UTF8_CHARSET);
      return new BufferedReader(stream);
    } catch (IOException e) {
      return null;
    }
  }

  @NotNull
  private static String decorateRetrievedHtml(@NotNull String retrievedHtml) {
    return "<html>\n" + HTTP_STYLE + "<body>\n" + retrievedHtml + "</body></html>\n";
  }

  @NotNull
  private String convertLink(@NotNull String href) {
    Matcher evaluatedLinkMatcher = PATTERN_EVALUATED_LINK.matcher(href);
    String concreteHref = evaluatedLinkMatcher.matches() ? evaluatedLinkMatcher.group(1) : href;
    Matcher externalLinkMatcher = PATTERN_EXTERNAL_LINK.matcher(concreteHref);
    if (externalLinkMatcher.matches()) {
      return PSI_ELEMENT_PROTOCOL + externalLinkMatcher.group(1) + "#" + externalLinkMatcher.group(2);
    }
    if (concreteHref.charAt(0) == '#') {
      return PSI_ELEMENT_PROTOCOL + myVirtualFile.getNameWithoutExtension() + concreteHref;
    }
    if (concreteHref.endsWith(".html")) {
      return PSI_ELEMENT_PROTOCOL + concreteHref.substring(0, concreteHref.length() - 5);
    }
    return href;
  }
}
