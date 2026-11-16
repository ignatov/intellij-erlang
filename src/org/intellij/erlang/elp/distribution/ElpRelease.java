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

package org.intellij.erlang.elp.distribution;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.intellij.openapi.diagnostic.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The ELP release the plugin downloads: the pinned manifest {@code /elp/elp-release.json}, written by
 * {@code scripts/elp/elp_release_manifest.py} from the GitHub API. Nothing about the release is looked up at run time.
 * <p>
 * The manifest is read with Gson, which every supported IDE (2026.1 to 2026.3) has on the core class path that plugin class
 * loaders delegate to. Jackson 2 is not visible to plugins in 2026.3.
 *
 * @param downloadUrl the URL that the file names of the assets are appended to
 * @param loadError   why the manifest could not be read, for a release without assets that stands in for it; {@code null} otherwise
 */
public record ElpRelease(@NotNull String tag,
                         @NotNull String version,
                         @NotNull String repository,
                         @NotNull String downloadUrl,
                         @NotNull String license,
                         @NotNull List<Asset> assets,
                         @Nullable String loadError) {
  private static final Logger LOG = Logger.getInstance(ElpRelease.class);

  public static final String RESOURCE = "/elp/elp-release.json";

  public static final String MACOS = "macos";
  public static final String LINUX = "linux";
  public static final String WINDOWS = "windows";
  public static final String X86_64 = "x86_64";
  public static final String AARCH64 = "aarch64";

  private static final Pattern SHA256 = Pattern.compile("[0-9a-f]{64}");
  private static final Pattern OTP = Pattern.compile("(\\d+)(\\.\\d+)*");
  private static final String TAR_GZ = ".tar.gz";

  /**
   * One tarball of the release.
   *
   * @param otp        the OTP release the binary was built on, for example {@code 28.5}; it runs on that release or a newer one
   * @param executable the only file in the tarball: {@code elp}, or {@code elp.exe} on Windows
   */
  public record Asset(@NotNull String file,
                      @NotNull String os,
                      @NotNull String arch,
                      @NotNull String target,
                      @NotNull String otp,
                      long size,
                      @NotNull String sha256,
                      @NotNull String executable) {
    public int otpMajor() {
      Matcher matcher = OTP.matcher(otp);
      if (!matcher.matches()) throw new IllegalStateException("Not an OTP release: " + otp);
      return Integer.parseInt(matcher.group(1));
    }

    /** The file name without {@code .tar.gz}: the directory the binary is installed in. */
    @NotNull
    public String directoryName() {
      return file.substring(0, file.length() - TAR_GZ.length());
    }

    /** The size in megabytes, with one decimal, as the UI shows it. */
    @NotNull
    public String sizeInMegabytes() {
      return String.format(Locale.ROOT,"%.1f MB", size / 1_000_000.0);
    }
  }

  public ElpRelease(@NotNull String tag, @NotNull String version, @NotNull String repository, @NotNull String downloadUrl,
                    @NotNull String license, @NotNull List<Asset> assets) {
    this(tag, version, repository, downloadUrl, license, assets, null);
  }

  /** A release without assets that stands in for a manifest that could not be read: no build is offered, and it says why. */
  @NotNull
  public static ElpRelease unavailable(@NotNull String reason) {
    return new ElpRelease("", "(unknown)", "https://github.com/WhatsApp/erlang-language-platform", "", "MIT OR Apache-2.0", List.of(),
                          reason);
  }

  @NotNull
  public String url(@NotNull Asset asset) {
    return downloadUrl + asset.file();
  }

  /**
   * @return the manifest bundled with the plugin, or, when it cannot be read, {@link #unavailable} with the reason; the error is
   * logged once and nothing is thrown, so the class stays usable for the session
   */
  @NotNull
  public static ElpRelease pinned() {
    return Holder.PINNED;
  }

  private static final class Holder {
    private static final ElpRelease PINNED = load();

    private static ElpRelease load() {
      ElpRelease release = read(() -> ElpRelease.class.getResourceAsStream(RESOURCE));
      if (release.loadError() != null) LOG.error("The pinned ELP manifest " + RESOURCE + " cannot be read: " + release.loadError());
      return release;
    }
  }

  /** Opens the manifest; returns {@code null} when there is none. */
  interface ManifestSource {
    @Nullable
    InputStream open() throws IOException;
  }

  /**
   * Reads the manifest from {@code source}. Never throws: a missing or broken manifest, and a JSON library that cannot be loaded,
   * give {@link #unavailable} with the reason.
   */
  @NotNull
  static ElpRelease read(@NotNull ManifestSource source) {
    try (InputStream stream = source.open()) {
      if (stream == null) return unavailable("the plugin has no " + RESOURCE);
      return parse(stream);
    }
    catch (IOException | RuntimeException | LinkageError e) {
      return unavailable(e.getClass().getSimpleName() + (e.getMessage() != null ? ": " + e.getMessage() : ""));
    }
  }

  /**
   * @throws IOException when the manifest is not valid JSON, misses a field, or has an asset without a 64-hex sha256, a positive
   *                     size, an OTP release or a {@code .tar.gz} name
   */
  @NotNull
  public static ElpRelease parse(@NotNull InputStream stream) throws IOException {
    JsonElement root;
    try (Reader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      root = JsonParser.parseReader(reader);
    }
    catch (JsonParseException e) {
      throw new IOException("The ELP manifest is not valid JSON: " + e.getMessage(), e);
    }
    if (!root.isJsonObject()) throw new IOException("The ELP manifest is not a JSON object");
    JsonObject object = root.getAsJsonObject();
    JsonElement assetNodes = object.get("assets");
    if (assetNodes == null || !assetNodes.isJsonArray() || assetNodes.getAsJsonArray().isEmpty()) {
      throw new IOException("The ELP manifest has no assets");
    }
    List<Asset> assets = new ArrayList<>();
    for (JsonElement element : assetNodes.getAsJsonArray()) {
      if (!element.isJsonObject()) throw new IOException("Not an asset object in the ELP manifest: " + element);
      JsonObject node = element.getAsJsonObject();
      Asset asset = new Asset(text(node, "file"), text(node, "os"), text(node, "arch"), text(node, "target"), text(node, "otp"),
                              size(node), text(node, "sha256"), text(node, "executable"));
      if (!asset.file().endsWith(TAR_GZ)) throw new IOException("Not a .tar.gz asset: " + asset.file());
      if (asset.size() <= 0) throw new IOException("No size for " + asset.file());
      if (!SHA256.matcher(asset.sha256()).matches()) throw new IOException("No sha256 for " + asset.file());
      if (!OTP.matcher(asset.otp()).matches()) throw new IOException("No OTP release for " + asset.file());
      assets.add(asset);
    }
    return new ElpRelease(text(object, "tag"), text(object, "version"), text(object, "repository"), text(object, "downloadUrl"),
                          text(object, "license"), List.copyOf(assets));
  }

  @NotNull
  private static String text(@NotNull JsonObject node, @NotNull String field) throws IOException {
    JsonElement value = node.get(field);
    if (!(value instanceof JsonPrimitive primitive) || !primitive.isString() || primitive.getAsString().isBlank()) {
      throw new IOException("The ELP manifest has no '" + field + "' in " + node);
    }
    return primitive.getAsString();
  }

  private static long size(@NotNull JsonObject node) {
    JsonElement value = node.get("size");
    if (!(value instanceof JsonPrimitive primitive) || !primitive.isNumber()) return -1;
    try {
      return primitive.getAsLong();
    }
    catch (NumberFormatException e) {
      return -1;
    }
  }
}
