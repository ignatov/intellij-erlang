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

package org.intellij.erlang.elp.lsp;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.jsonrpc.json.MessageJsonHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The answers of the real ELP server on {@code testData/elp/project}, recorded by {@code testData/elp/record_answers.py} into
 * {@code testData/elp/answers}. The placeholders of the recorded paths are replaced by the URI of the test's copy of the project;
 * the OTP root is replaced by a directory that does not exist, as for a file that the IDE cannot open.
 */
final class ElpRecordedAnswers {
  static final Path DIRECTORY = Path.of("testData/elp/answers");
  static final String MISSING_OTP_URI = "file:///elp-test-no-otp";

  private static final Gson GSON = new MessageJsonHandler(Map.of()).getGson();

  private final String myProjectUri;

  /** @param projectUri the URI of the root of the test's copy of the project, without a trailing slash */
  ElpRecordedAnswers(@NotNull String projectUri) {
    myProjectUri = projectUri;
  }

  @NotNull
  String projectUri() {
    return myProjectUri;
  }

  @NotNull
  String uri(@NotNull String relativePath) {
    return myProjectUri + "/" + relativePath;
  }

  /** One recorded request and its answer. */
  record Case(@NotNull String method, @NotNull String kind, @NotNull String file, @NotNull String needle, @NotNull Position position,
              @NotNull JsonObject params, @Nullable JsonElement result, @Nullable JsonElement error) {
    @NotNull
    String uri() {
      return params.getAsJsonObject("textDocument").get("uri").getAsString();
    }

    <T> T result(@NotNull Type type) {
      return result == null || result.isJsonNull() ? null : GSON.fromJson(result, type);
    }

    @Override
    public String toString() {
      return method + " " + kind + " at " + file + ":" + (position.getLine() + 1) + ":" + (position.getCharacter() + 1);
    }
  }

  /** The recorded cases of {@code method}, such as {@code textDocument/references}, in the order of the recorder. */
  @NotNull
  List<Case> cases(@NotNull String method) {
    JsonArray array = load(method.substring(method.indexOf('/') + 1)).getAsJsonArray();
    List<Case> cases = new ArrayList<>();
    for (JsonElement element : array) {
      JsonObject object = element.getAsJsonObject();
      cases.add(new Case(method, object.get("kind").getAsString(), object.get("file").getAsString(),
                         object.get("needle").getAsString(), GSON.fromJson(object.get("position"), Position.class),
                         object.getAsJsonObject("params"), object.get("result"), object.get("error")));
    }
    return cases;
  }

  @NotNull
  Case get(@NotNull String method, @NotNull String kind) {
    for (Case c : cases(method)) {
      if (c.kind().equals(kind)) return c;
    }
    throw new AssertionError("no recorded " + method + " " + kind);
  }

  /** The published diagnostics by the file path relative to the project root. */
  @NotNull
  Map<String, List<Diagnostic>> diagnostics() {
    Map<String, List<Diagnostic>> result = new LinkedHashMap<>();
    for (JsonElement element : load("publishDiagnostics").getAsJsonArray()) {
      JsonObject object = element.getAsJsonObject();
      List<Diagnostic> diagnostics = new ArrayList<>();
      for (JsonElement diagnostic : object.getAsJsonArray("diagnostics")) diagnostics.add(GSON.fromJson(diagnostic, Diagnostic.class));
      result.put(object.get("file").getAsString(), diagnostics);
    }
    return result;
  }

  @NotNull
  InitializeResult initializeResult() {
    return GSON.fromJson(load("initialize"), InitializeResult.class);
  }

  @NotNull
  static String elpVersion() {
    return JsonParser.parseString(read("manifest")).getAsJsonObject().get("elp_version").getAsString();
  }

  @NotNull
  private JsonElement load(@NotNull String name) {
    String text = read(name)
      .replace("${PROJECT_URI}", myProjectUri)
      .replace("${OTP_URI}", MISSING_OTP_URI);
    return JsonParser.parseString(text);
  }

  @NotNull
  private static String read(@NotNull String name) {
    try {
      return Files.readString(DIRECTORY.resolve(name + ".json"), StandardCharsets.UTF_8);
    }
    catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  @NotNull
  static Gson gson() {
    return GSON;
  }
}
