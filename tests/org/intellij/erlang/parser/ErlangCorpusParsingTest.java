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
import com.intellij.openapi.util.text.LineColumn;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.ErlangFileType;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
 * Without a corpus the test does nothing. It never fails because of parse errors; see .github/workflows/compat.yml.
 */
public class ErlangCorpusParsingTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final long SLOW_FILE_MILLIS = 10_000;

  @Override
  protected boolean shouldRunTest() {
    return setting("CORPUS_DIR", "corpus.dir") != null && super.shouldRunTest();
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
    Path root = Paths.get(setting("CORPUS_DIR", "corpus.dir")).toAbsolutePath().normalize();
    String reportSetting = setting("CORPUS_REPORT", "corpus.report");
    Path report = Paths.get(reportSetting != null ? reportSetting : "build/corpus-report.json").toAbsolutePath();
    assertTrue("Corpus directory " + root + " does not exist", Files.isDirectory(root));

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
          LineColumn position = StringUtil.offsetToLineColumn(text, error.getTextOffset());
          errors.add("{\"file\": " + json(relativePath) +
                     ", \"line\": " + (position.line + 1) +
                     ", \"column\": " + (position.column + 1) +
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
