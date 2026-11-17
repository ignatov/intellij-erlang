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
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VfsUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.ResourceUtil;
import com.intellij.util.io.HttpRequests;
import com.intellij.util.io.URLUtil;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.intellij.codeInsight.documentation.DocumentationManagerProtocol.PSI_ELEMENT_PROTOCOL;

abstract class ErlangSdkDocProviderBase implements ElementDocProvider {
  private static final Logger LOG = Logger.getInstance(ErlangSdkDocProviderBase.class);
  private static final Pattern PATTERN_HREF = Pattern.compile("<a href=\"([^\"]*)\">");
  private static final Pattern PATTERN_EVALUATED_LINK = Pattern.compile("javascript:erlhref\\('.*?','.*?','(.*?)'\\);");
  private static final Pattern PATTERN_EXTERNAL_LINK = Pattern.compile("(?:.*/)?(.*)\\.html#(.*)");
  private static final Pattern PATTERN_MODERN_DETAIL_BEGIN = Pattern.compile("^<section class=\"detail\" id=\"(.*?)\">$");
  private static final Pattern PATTERN_MODERN_SECTION_END = Pattern.compile("^(\\s*)</section>$");
  private static final Pattern PATTERN_MODERN_DOCSTRING = Pattern.compile(
    "(?s)^(.*?)<section class=\"docstring\">\\s*(.*?)\\s*</section>\\s*$");
  private static final Pattern PATTERN_MODERN_SIGNATURE = Pattern.compile("(?s)<h1 class=\"signature\"[^>]*>.*?</h1>");
  private static final Pattern PATTERN_MODERN_NOTE = Pattern.compile("(?s)<span class=\"note\">.*?</span>");
  private static final Pattern PATTERN_MODERN_DEPRECATED = Pattern.compile("(?s)<div class=\"deprecated\">.*?</div>");

  static final String HTTP_STYLE;
  static {
    String css;
    try {
       css = ResourceUtil.loadText(ResourceUtil.getResourceAsStream(
         ErlangSdkDocProviderBase.class.getClassLoader(), "/documentation", "erlang-sdk-doc.css"));
    } catch (IOException e) {
      throw new AssertionError(e);
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
      myExternalDocUrls = getHttpUrls(getOrderEntries(), myVirtualFile, getInDocRef(), getModernInDocRef(),
                                      ErlangSdkType.getRelease(myProject));
    }
    return myExternalDocUrls;
  }

  @Nullable
  @Override
  public String getDocText() {
    List<String> urls = ReadAction.computeBlocking(() -> {
      List<String> fileUrls = getFileUrls(getOrderEntries(), myVirtualFile);
      List<String> httpUrls = getExternalDocUrls();
      List<String> result = new ArrayList<>(fileUrls.size() + httpUrls.size());
      result.addAll(fileUrls);
      result.addAll(httpUrls);
      return result;
    });
    for (String urlString : urls) {
      ProgressManager.checkCanceled();
      String retrievedHtml = fetchDoc(urlString);
      if (retrievedHtml != null) {
        return decorateRetrievedHtml(retrievedHtml);
      }
    }
    return null;
  }

  @Nullable
  private String fetchDoc(@NotNull String urlString) {
    try {
      String protocol = URLUtil.containsScheme(urlString) ? urlString.substring(0, urlString.indexOf(URLUtil.SCHEME_SEPARATOR)) : null;
      if (protocol != null && isHttpProtocol(protocol)) {
        return HttpRequests.request(urlString)
          .productNameAsUserAgent()
          .connect(request -> retrieveDoc(request.getReader()));
      }
      if (protocol == null || protocol.equals(URLUtil.FILE_PROTOCOL)) {
        Path path = Path.of(protocol == null ? urlString : VfsUtilCore.urlToPath(urlString));
        if (!Files.isRegularFile(path)) {
          return null;
        }
        return retrieveDoc(new BufferedReader(new InputStreamReader(Files.newInputStream(path), StandardCharsets.UTF_8)));
      }
    }
    catch (IOException | InvalidPathException e) {
      LOG.debug("Cannot fetch Erlang SDK documentation from " + urlString, e);
    }
    return null;
  }

  @NotNull
  private List<OrderEntry> getOrderEntries() {
    if (myOrderEntries == null) {
      myOrderEntries = ReadAction.computeBlocking((() -> {
        ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();
        return fileIndex.getOrderEntriesForFile(myVirtualFile);
      }));
    }
    return myOrderEntries;
  }

  @Nullable
  String retrieveDoc(@NotNull BufferedReader reader) throws IOException {
    try (reader) {
      String line;
      while ((line = reader.readLine()) != null) {
        ProgressManager.checkCanceled();
        if (isModernDocBegin(line)) {
          return retrieveModernDoc(reader, line);
        }
        if (isDocBegin(line)) {
          return retrieveLegacyDoc(reader, line);
        }
      }
      return null;
    }
  }

  @NotNull
  private String retrieveLegacyDoc(@NotNull BufferedReader reader, @NotNull String firstLine) throws IOException {
    StringBuilder builder = new StringBuilder(1024);
    appendCorrectedLine(builder, firstLine);
    String line;
    while ((line = reader.readLine()) != null && !isDocEnd(line)) {
      ProgressManager.checkCanceled();
      appendCorrectedLine(builder, line);
      builder.append("\n");
    }
    return builder.toString();
  }

  /**
   * Modern (ex_doc based, OTP 27+) documentation keeps each entry in a {@code <section>} element.
   * The entry ends with a {@code </section>} line indented the same way as the opening tag.
   */
  @NotNull
  private String retrieveModernDoc(@NotNull BufferedReader reader, @NotNull String firstLine) throws IOException {
    String indent = firstLine.substring(0, firstLine.length() - firstLine.stripLeading().length());
    StringBuilder builder = new StringBuilder(1024);
    String line;
    while ((line = reader.readLine()) != null) {
      ProgressManager.checkCanceled();
      Matcher endMatcher = PATTERN_MODERN_SECTION_END.matcher(line);
      if (endMatcher.matches() && endMatcher.group(1).equals(indent) || isModernDocInterrupted(line)) {
        break;
      }
      appendCorrectedLine(builder, line);
      builder.append("\n");
    }
    return prepareModernDoc(builder.toString());
  }

  protected boolean isModernDocBegin(@NotNull String line) {
    return false;
  }

  /**
   * Safety net for malformed pages: stop at the next documentation entry or at the page footer.
   */
  private static boolean isModernDocInterrupted(@NotNull String line) {
    return PATTERN_MODERN_DETAIL_BEGIN.matcher(line).matches() || line.stripLeading().startsWith("<footer");
  }

  @NotNull
  protected String prepareModernDoc(@NotNull String doc) {
    return doc;
  }

  static boolean isModernDetailBegin(@NotNull String line, @NotNull Pattern idPattern) {
    Matcher matcher = PATTERN_MODERN_DETAIL_BEGIN.matcher(line);
    return matcher.matches() && idPattern.matcher(matcher.group(1)).matches();
  }

  /**
   * Turns a modern documentation entry ({@code <section class="detail">}) into a signature,
   * optional notes and deprecation message, followed by the entry docstring.
   * Page chrome such as anchor links and "View Source" buttons is dropped.
   */
  @NotNull
  static String prepareModernDetail(@NotNull String doc) {
    Matcher docstringMatcher = PATTERN_MODERN_DOCSTRING.matcher(doc);
    if (!docstringMatcher.matches()) {
      return doc;
    }
    String header = docstringMatcher.group(1);
    Matcher signatureMatcher = PATTERN_MODERN_SIGNATURE.matcher(header);
    if (!signatureMatcher.find()) {
      return doc;
    }
    StringBuilder builder = new StringBuilder(doc.length());
    builder.append(signatureMatcher.group()).append("\n");
    Matcher noteMatcher = PATTERN_MODERN_NOTE.matcher(header);
    while (noteMatcher.find()) {
      builder.append(noteMatcher.group()).append("\n");
    }
    Matcher deprecatedMatcher = PATTERN_MODERN_DEPRECATED.matcher(header);
    if (deprecatedMatcher.find()) {
      builder.append(deprecatedMatcher.group()).append("\n");
    }
    builder.append(docstringMatcher.group(2)).append("\n");
    return builder.toString();
  }

  private void appendCorrectedLine(@NotNull StringBuilder builder, @NotNull String line) {
    Matcher matcher = PATTERN_HREF.matcher(line);
    int lastCopiedChar = 0;
    while (matcher.find()) {
      MatchResult matchResult = matcher.toMatchResult();
      builder.append(line, lastCopiedChar, matchResult.start());
      String linkHref = matchResult.group(1);
      String convertedLink = convertLink(linkHref);
      builder.append("<a href=\"")
        .append(convertedLink)
        .append("\">");
      lastCopiedChar = matchResult.end();
    }
    builder.append(line.substring(lastCopiedChar));
  }

  @NotNull
  protected abstract String getInDocRef();

  @NotNull
  protected String getModernInDocRef() {
    return getInDocRef();
  }

  protected abstract boolean isDocEnd(@NotNull String line);

  protected abstract boolean isDocBegin(@NotNull String line);

  @NotNull
  private static List<String> getHttpUrls(@NotNull List<OrderEntry> orderEntries,
                                          @NotNull VirtualFile virtualFile,
                                          @NotNull String legacyInDocRef,
                                          @NotNull String modernInDocRef,
                                          @NotNull ErlangSdkRelease release) {
    for (OrderEntry orderEntry : orderEntries) {
      String[] docRootUrls = JavadocOrderRootType.getUrls(orderEntry);
      List<String> httpUrls = new ArrayList<>();
      for (String docRootUrl : docRootUrls) {
        if (isOfficialErlangDocumentationUrl(docRootUrl)) {
          httpUrls.add(ErlangSdkType.getDefaultDocumentationUrl(release) + "/" +
                       modernHttpDocRelPath(virtualFile) + modernInDocRef);
        }
        else {
          List<String> customUrls = PlatformDocumentationUtil.getHttpRoots(
            new String[]{docRootUrl}, legacyHttpDocRelPath(virtualFile) + legacyInDocRef);
          if (customUrls != null) {
            httpUrls.addAll(customUrls);
          }
        }
      }
      if (!httpUrls.isEmpty()) {
        return httpUrls;
      }
    }
    return Collections.emptyList();
  }

  static boolean isOfficialErlangDocumentationUrl(@NotNull String url) {
    return url.startsWith("http://erlang.org/") || url.startsWith("https://erlang.org/") ||
           url.startsWith("http://www.erlang.org/") || url.startsWith("https://www.erlang.org/");
  }

  @NotNull
  private static List<String> getFileUrls(@NotNull List<OrderEntry> orderEntries,
                                          @NotNull VirtualFile virtualFile) {
    List<String> fileUrls = null;
    for (OrderEntry orderEntry : orderEntries) {
      VirtualFile[] docRootFiles = orderEntry.getFiles(JavadocOrderRootType.getInstance());
      String sdkHttpDocRelPath = legacyHttpDocRelPath(virtualFile);
      for (VirtualFile docRootFile : docRootFiles) {
        if (docRootFile.isInLocalFileSystem()) {
          if (fileUrls == null) {
            fileUrls = new ArrayList<>();
          }
          fileUrls.add(docRootFile.getUrl() + "/" + sdkHttpDocRelPath);
        }
      }
    }
    return fileUrls != null ? fileUrls : Collections.emptyList();
  }

  static boolean isHttpProtocol(@NotNull String protocol) {
    return protocol.equalsIgnoreCase("http") || protocol.equalsIgnoreCase("https");
  }

  @NotNull
  private static String legacyHttpDocRelPath(@NotNull VirtualFile virtualFile) {
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
  private static String modernHttpDocRelPath(@NotNull VirtualFile virtualFile) {
    String appDirName = virtualFile.getParent().getParent().getName();
    String appName = getApplicationName(appDirName);
    return "apps/" + appName + "/" + virtualFile.getNameWithoutExtension() + ".html";
  }

  @NotNull
  static String getApplicationName(@NotNull String appDirName) {
    return appDirName.replaceFirst("-\\d.*$", "");
  }

  @NotNull
  private static String decorateRetrievedHtml(@NotNull String retrievedHtml) {
    return "<html>\n" + HTTP_STYLE + "<body>\n" + retrievedHtml + "</body></html>\n";
  }

  @NotNull
  private String convertLink(@NotNull String href) {
    return convertLink(href, myVirtualFile.getNameWithoutExtension());
  }

  @NotNull
  static String convertLink(@NotNull String href, @NotNull String currentModuleName) {
    if (href.isEmpty()) return href;

    Matcher evaluatedLinkMatcher = PATTERN_EVALUATED_LINK.matcher(href);
    String concreteHref = evaluatedLinkMatcher.matches() ? evaluatedLinkMatcher.group(1) : href;
    if (concreteHref.isEmpty() || URLUtil.containsScheme(concreteHref)) return href;
    Matcher externalLinkMatcher = PATTERN_EXTERNAL_LINK.matcher(concreteHref);
    if (externalLinkMatcher.matches()) {
      return PSI_ELEMENT_PROTOCOL + externalLinkMatcher.group(1) + "#" + externalLinkMatcher.group(2);
    }
    if (concreteHref.charAt(0) == '#') {
      return PSI_ELEMENT_PROTOCOL + currentModuleName + concreteHref;
    }
    if (concreteHref.endsWith(".html")) {
      String page = concreteHref.substring(concreteHref.lastIndexOf('/') + 1, concreteHref.length() - 5);
      return PSI_ELEMENT_PROTOCOL + page;
    }
    return href;
  }
}
