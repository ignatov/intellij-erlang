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

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The ELP release the plugin downloads: the pinned manifest {@code /elp/elp-release.json}, written by
 * {@code scripts/elp/elp_release_manifest.py} from the GitHub API. Nothing about the release is looked up at run time.
 *
 * @param downloadUrl the URL that the file names of the assets are appended to
 */
public record ElpRelease(@NotNull String tag,
                         @NotNull String version,
                         @NotNull String repository,
                         @NotNull String downloadUrl,
                         @NotNull String license,
                         @NotNull List<Asset> assets) {
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

  @NotNull
  public String url(@NotNull Asset asset) {
    return downloadUrl + asset.file();
  }

  /** @return the manifest bundled with the plugin */
  @NotNull
  public static ElpRelease pinned() {
    return Holder.PINNED;
  }

  private static final class Holder {
    private static final ElpRelease PINNED = load();

    private static ElpRelease load() {
      try (InputStream stream = ElpRelease.class.getResourceAsStream(RESOURCE)) {
        if (stream == null) throw new IOException("No " + RESOURCE + " in the plugin");
        return parse(stream);
      }
      catch (IOException e) {
        throw new UncheckedIOException(e);
      }
    }
  }

  /**
   * @throws IOException when the manifest is not valid JSON, misses a field, or has an asset without a 64-hex sha256, a positive
   *                     size, an OTP release or a {@code .tar.gz} name
   */
  @NotNull
  public static ElpRelease parse(@NotNull InputStream stream) throws IOException {
    JsonNode root = new ObjectMapper().readTree(stream);
    if (root == null || !root.isObject()) throw new IOException("The ELP manifest is not a JSON object");
    List<Asset> assets = new ArrayList<>();
    JsonNode assetNodes = root.get("assets");
    if (assetNodes == null || !assetNodes.isArray() || assetNodes.isEmpty()) throw new IOException("The ELP manifest has no assets");
    for (JsonNode node : assetNodes) {
      Asset asset = new Asset(text(node, "file"), text(node, "os"), text(node, "arch"), text(node, "target"), text(node, "otp"),
                              node.path("size").asLong(-1), text(node, "sha256"), text(node, "executable"));
      if (!asset.file().endsWith(TAR_GZ)) throw new IOException("Not a .tar.gz asset: " + asset.file());
      if (asset.size() <= 0) throw new IOException("No size for " + asset.file());
      if (!SHA256.matcher(asset.sha256()).matches()) throw new IOException("No sha256 for " + asset.file());
      if (!OTP.matcher(asset.otp()).matches()) throw new IOException("No OTP release for " + asset.file());
      assets.add(asset);
    }
    return new ElpRelease(text(root, "tag"), text(root, "version"), text(root, "repository"), text(root, "downloadUrl"),
                          text(root, "license"), List.copyOf(assets));
  }

  @NotNull
  private static String text(@NotNull JsonNode node, @NotNull String field) throws IOException {
    JsonNode value = node.get(field);
    if (value == null || !value.isTextual() || value.asText().isBlank()) {
      throw new IOException("The ELP manifest has no '" + field + "' in " + node);
    }
    return value.asText();
  }
}
