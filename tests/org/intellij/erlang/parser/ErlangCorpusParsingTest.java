/*
 * Copyright 2012-2026 Sergey Ignatov
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

package org.intellij.erlang.parser;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.LineColumn;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.JUnit38AssumeSupportRunner;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Assume;
import org.junit.runner.RunWith;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

/**
 * Parses every .erl and .hrl file under CORPUS_DIR (or -Dcorpus.dir), for example the root of an Erlang/OTP
 * installation, and writes the syntax errors the plugin's parser reports there as JSON to CORPUS_REPORT
 * (or -Dcorpus.report). Every file shipped with Erlang/OTP compiles, so each error element is a plugin bug.
 * Without a corpus testCorpus does nothing. It never fails because of parse errors; see .github/workflows/compat.yml.
 * A corpus directory that is a symbolic link is followed; links below it are not. Columns count characters (code
 * points), as erlc does. The other tests check the report on small corpora of their own; the one with a symbolic link
 * is skipped where the platform cannot create one (Windows without the privilege).
 */
@RunWith(JUnit38AssumeSupportRunner.class)
public class ErlangCorpusParsingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final long SLOW_FILE_MILLIS = 10_000;

  @Override
  protected boolean shouldRunTest() {
    return (!"testCorpus".equals(getName()) || setting("CORPUS_DIR", "corpus.dir") != null) && super.shouldRunTest();
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        // The newest mock SDK turns on all release-gated syntax.
        ErlangSdkRelease release = newestMockSdkRelease();
        return ErlangSdkType.createMockSdk(mockSdkHome(release), release);
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setUpProjectSdk();
  }

  public void testCorpus() throws Exception {
    String reportSetting = setting("CORPUS_REPORT", "corpus.report");
    Path report = Paths.get(reportSetting != null ? reportSetting : "build/corpus-report.json").toAbsolutePath();
    writeReport(Paths.get(setting("CORPUS_DIR", "corpus.dir")), report);
  }

  public void testSymbolicLinkToTheCorpusIsFollowed() throws Exception {
    Path temp = FileUtil.createTempDirectory("corpus", null, true).toPath();
    Path corpus = Files.createDirectories(temp.resolve("corpus/lib/src"));
    Files.writeString(corpus.resolve("a.erl"), "-module(a).\nf() -> [1,, 2].\n", StandardCharsets.UTF_8);
    Files.writeString(corpus.resolve("b.hrl"), "-define(B, 1).\n", StandardCharsets.UTF_8);
    Path link;
    try {
      link = Files.createSymbolicLink(temp.resolve("link"), temp.resolve("corpus"));
    }
    catch (IOException | UnsupportedOperationException | SecurityException e) {
      Assume.assumeNoException("This platform cannot create a symbolic link", e);
      return;
    }
    String report = writeReport(link, temp.resolve("report.json"));
    assertTrue(report, report.contains("\"files\": 2,"));
    assertTrue(report, report.contains("\"filesWithErrors\": 1,"));
    assertTrue(report, report.contains("{\"file\": \"lib/src/a.erl\", \"line\": 2, \"column\": 11,"));
  }

  public void testColumnsCountCharactersOutsideTheBmpOnce() throws Exception {
    // erlc reports 3:19 for this error: each character, also one outside the BMP, is one column.
    Path temp = FileUtil.createTempDirectory("corpus", null, true).toPath();
    Files.writeString(temp.resolve("c.erl"), "-module(c).\n-export([f/0]).\nf() -> \"\u00e9\uD83D\uDE00\" ++ [1,, 2].\n",
                      StandardCharsets.UTF_8);
    String report = writeReport(temp, temp.resolve("report.json"));
    assertTrue(report, report.contains("{\"file\": \"c.erl\", \"line\": 3, \"column\": 19,"));
  }

  @NotNull
  private String writeReport(@NotNull Path corpusDir, @NotNull Path report) throws IOException {
    assertTrue("Corpus directory " + corpusDir + " does not exist", Files.isDirectory(corpusDir));
    // Files.walk does not follow a root that is a symbolic link, so walk the directory it points to.
    Path root = corpusDir.toRealPath();

    List<Path> files;
    try (Stream<Path> walk = Files.walk(root)) {
      files = walk.filter(Files::isRegularFile)
        .filter(path -> path.toString().endsWith(".erl") || path.toString().endsWith(".hrl"))
        .sorted()
        .toList();
    }

    int filesWithErrors = 0;
    int errorElements = 0;
    List<String> errors = new ArrayList<>();
    List<String> exceptions = new ArrayList<>();
    List<String> slowFiles = new ArrayList<>();
    for (Path file : files) {
      String relativePath = root.relativize(file).toString().replace(File.separatorChar, '/');
      long start = System.nanoTime();
      try {
        String text = StringUtil.convertLineSeparators(readText(file));
        ErlangFileType fileType = relativePath.endsWith(".hrl") ? ErlangFileType.HEADER : ErlangFileType.MODULE;
        PsiFile psiFile = PsiFileFactory.getInstance(getProject()).createFileFromText(file.getFileName().toString(), fileType, text);
        List<PsiErrorElement> fileErrors = new ArrayList<>(PsiTreeUtil.collectElementsOfType(psiFile, PsiErrorElement.class));
        if (!fileErrors.isEmpty()) filesWithErrors++;
        errorElements += fileErrors.size();
        for (PsiErrorElement error : fileErrors) {
          int offset = error.getTextOffset();
          LineColumn position = StringUtil.offsetToLineColumn(text, offset);
          int column = text.codePointCount(offset - position.column, offset) + 1;
          errors.add("{\"file\": " + json(relativePath) +
                     ", \"line\": " + (position.line + 1) +
                     ", \"column\": " + column +
                     ", \"description\": " + json(error.getErrorDescription()) + "}");
        }
      }
      catch (Throwable e) {
        exceptions.add("{\"file\": " + json(relativePath) + ", \"exception\": " + json(e.toString()) + "}");
      }
      long millis = (System.nanoTime() - start) / 1_000_000;
      if (millis > SLOW_FILE_MILLIS) {
        slowFiles.add("{\"file\": " + json(relativePath) + ", \"millis\": " + millis + "}");
      }
    }

    String json = "{\n" +
                  "  \"corpusDir\": " + json(root.toString()) + ",\n" +
                  "  \"sdkRelease\": " + json(ErlangSdkType.getRelease(getProject()).toString()) + ",\n" +
                  "  \"files\": " + files.size() + ",\n" +
                  "  \"filesWithErrors\": " + filesWithErrors + ",\n" +
                  "  \"errorElements\": " + errorElements + ",\n" +
                  "  \"slowFileMillis\": " + SLOW_FILE_MILLIS + ",\n" +
                  "  \"errors\": " + jsonArray(errors) + ",\n" +
                  "  \"exceptions\": " + jsonArray(exceptions) + ",\n" +
                  "  \"slowFiles\": " + jsonArray(slowFiles) + "\n" +
                  "}\n";
    Files.createDirectories(report.getParent());
    Files.writeString(report, json, StandardCharsets.UTF_8);
    System.out.println("Parsed " + files.size() + " files under " + root + ": " + filesWithErrors + " with errors, " +
                       errorElements + " error elements, " + exceptions.size() + " exceptions, " +
                       slowFiles.size() + " slower than " + SLOW_FILE_MILLIS + " ms. Report: " + report);
    return json;
  }

  @Nullable
  private static String setting(@NotNull String environmentVariable, @NotNull String systemProperty) {
    String value = System.getProperty(systemProperty);
    return StringUtil.nullize(StringUtil.isEmptyOrSpaces(value) ? System.getenv(environmentVariable) : value, true);
  }

  @NotNull
  private static ErlangSdkRelease newestMockSdkRelease() {
    ErlangSdkRelease newest = null;
    for (Field field : ErlangSdkRelease.class.getFields()) {
      if (!Modifier.isStatic(field.getModifiers()) || field.getType() != ErlangSdkRelease.class) continue;
      try {
        ErlangSdkRelease release = (ErlangSdkRelease) field.get(null);
        if (new File(mockSdkHome(release)).isDirectory() && (newest == null || release.isNewerThan(newest))) {
          newest = release;
        }
      }
      catch (IllegalAccessException e) {
        throw new AssertionError(e);
      }
    }
    assertNotNull("No testData/mockSdk-R<release> directory matches an ErlangSdkRelease constant", newest);
    return newest;
  }

  @NotNull
  private static String mockSdkHome(@NotNull ErlangSdkRelease release) {
    return "testData/mockSdk-R" + release.getOtpRelease() + "/";
  }

  @NotNull
  private static String readText(@NotNull Path file) throws IOException {
    byte[] bytes = Files.readAllBytes(file);
    try {
      return StandardCharsets.UTF_8.newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
        .decode(ByteBuffer.wrap(bytes))
        .toString();
    }
    catch (CharacterCodingException e) {
      return new String(bytes, StandardCharsets.ISO_8859_1);
    }
  }

  @NotNull
  private static String jsonArray(@NotNull List<String> items) {
    return items.isEmpty() ? "[]" : "[\n    " + String.join(",\n    ", items) + "\n  ]";
  }

  @NotNull
  private static String json(@Nullable String value) {
    if (value == null) return "null";
    StringBuilder result = new StringBuilder("\"");
    for (char c : value.toCharArray()) {
      switch (c) {
        case '"' -> result.append("\\\"");
        case '\\' -> result.append("\\\\");
        case '\n' -> result.append("\\n");
        case '\r' -> result.append("\\r");
        case '\t' -> result.append("\\t");
        default -> {
          if (c < 0x20) result.append(String.format("\\u%04x", (int) c));
          else result.append(c);
        }
      }
    }
    return result.append('"').toString();
  }
}
