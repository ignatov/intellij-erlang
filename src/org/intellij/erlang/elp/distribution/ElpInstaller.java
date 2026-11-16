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

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.util.io.NioFiles;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.io.Decompressor;
import com.intellij.util.io.HttpRequests;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.UnknownHostException;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;
import java.util.concurrent.atomic.AtomicReference;

/**
 * Downloads a tarball of the pinned ELP release and installs its binary in
 * {@code <base>/<tag>/<asset name without .tar.gz>/elp}, by default under the IDE's system directory, so that several OTP builds
 * and several IDE installations can each have their own.
 * <p>
 * The tarball goes to a temporary file in that directory, and its size and sha256 are checked against the manifest before it is
 * unpacked. The binary is made executable, must answer {@code elp version} with the manifest's version, and is then moved to its
 * place atomically: a binary in its place is complete and checked, so a later start uses it without the network. Leftovers of an
 * interrupted download (temporary files starting with {@code .download-} or {@code .unpack-}) are removed by the next one.
 * <p>
 * Nothing here runs on its own: {@link #install} is called on a background thread after the user asked for the download.
 */
public final class ElpInstaller {
  private static final Logger LOG = Logger.getInstance(ElpInstaller.class);

  public static final String DIRECTORY = "erlang-elp";
  static final String DOWNLOAD_PREFIX = ".download-";
  static final String UNPACK_PREFIX = ".unpack-";
  public static final int VERSION_TIMEOUT_MS = 30_000;
  private static final int CONNECT_TIMEOUT_MS = 30_000;
  private static final int READ_TIMEOUT_MS = 60_000;
  /** The free space asked for, as a multiple of the tarball: the tarball and the unpacked binary are on disk together. */
  private static final int SPACE_FACTOR = 4;

  /** Runs the unpacked binary once; throws when it does not run or is not the manifest's version. */
  public interface VersionCheck {
    void check(@NotNull Path executable, @NotNull ElpRelease release, @NotNull ProgressIndicator indicator) throws ElpInstallException;
  }

  private final Path myBaseDir;
  private final ElpRelease myRelease;
  private final String myDownloadUrl;
  private final VersionCheck myVersionCheck;

  /** The installer of the pinned release into the IDE's system directory. */
  public ElpInstaller() {
    this(defaultBaseDir(), ElpRelease.pinned(), ElpRelease.pinned().downloadUrl(), ElpInstaller::runVersion);
  }

  /**
   * @param downloadUrl the URL the file names of the assets are appended to, {@link ElpRelease#downloadUrl()} outside tests
   */
  public ElpInstaller(@NotNull Path baseDir, @NotNull ElpRelease release, @NotNull String downloadUrl,
                      @NotNull VersionCheck versionCheck) {
    myBaseDir = baseDir;
    myRelease = release;
    myDownloadUrl = downloadUrl;
    myVersionCheck = versionCheck;
  }

  @NotNull
  public static Path defaultBaseDir() {
    return PathManager.getSystemDir().resolve(DIRECTORY);
  }

  @NotNull
  public ElpRelease getRelease() {
    return myRelease;
  }

  @NotNull
  public Path installDirectory(@NotNull ElpRelease.Asset asset) {
    return myBaseDir.resolve(myRelease.tag()).resolve(asset.directoryName());
  }

  @NotNull
  public Path executable(@NotNull ElpRelease.Asset asset) {
    return installDirectory(asset).resolve(asset.executable());
  }

  /** @return the installed binary of {@code asset}, or {@code null} when it is not installed */
  @Nullable
  @RequiresBackgroundThread
  public Path findInstalled(@NotNull ElpRelease.Asset asset) {
    Path executable = executable(asset);
    return Files.isRegularFile(executable) ? executable : null;
  }

  /**
   * Downloads, checks and installs {@code asset}, or returns the binary that is installed already.
   *
   * @return the installed binary
   * @throws ElpInstallException with a message for the user when the download, the check or the installation failed; nothing is
   *                             left behind then
   * @throws com.intellij.openapi.progress.ProcessCanceledException when {@code indicator} was cancelled; nothing is left behind
   */
  @NotNull
  @RequiresBackgroundThread
  public Path install(@NotNull ElpRelease.Asset asset, @NotNull ProgressIndicator indicator) throws ElpInstallException {
    Path installed = findInstalled(asset);
    if (installed != null) return installed;
    Path directory = installDirectory(asset);
    Path archive = null;
    Path unpackDir = null;
    try {
      try {
        Files.createDirectories(directory);
        cleanUp(directory);
        long usable = Files.getFileStore(directory).getUsableSpace();
        if (usable < asset.size() * SPACE_FACTOR) {
          throw new ElpInstallException("Not enough free space in " + directory + ": ELP needs about " +
                                        (asset.size() * SPACE_FACTOR / 1_000_000) + " MB.");
        }
        archive = Files.createTempFile(directory, DOWNLOAD_PREFIX, ".tar.gz");
      }
      catch (ElpInstallException e) {
        throw e;
      }
      catch (IOException e) {
        throw new ElpInstallException("Cannot write to " + directory + ": " + describe(e), e);
      }
      indicator.setIndeterminate(false);
      indicator.setText("Downloading ELP " + myRelease.version() + ", " + asset.sizeInMegabytes());
      download(asset, archive, indicator);

      indicator.checkCanceled();
      indicator.setIndeterminate(true);
      indicator.setText("Unpacking ELP " + myRelease.version());
      Path unpacked;
      try {
        unpackDir = Files.createTempDirectory(directory, UNPACK_PREFIX);
        unpacked = unpack(asset, archive, unpackDir);
        NioFiles.setExecutable(unpacked);
      }
      catch (ElpInstallException e) {
        throw e;
      }
      catch (IOException e) {
        throw new ElpInstallException("Cannot unpack " + asset.file() + " in " + directory + ": " + describe(e), e);
      }

      indicator.checkCanceled();
      indicator.setText("Checking that ELP " + myRelease.version() + " runs");
      myVersionCheck.check(unpacked, myRelease, indicator);

      Path executable = executable(asset);
      try {
        try {
          Files.move(unpacked, executable, StandardCopyOption.ATOMIC_MOVE);
        }
        catch (AtomicMoveNotSupportedException e) {
          Files.move(unpacked, executable, StandardCopyOption.REPLACE_EXISTING);
        }
      }
      catch (IOException e) {
        throw new ElpInstallException("Cannot move ELP to " + executable + ": " + describe(e), e);
      }
      LOG.info("ELP " + myRelease.version() + " installed: " + executable);
      return executable;
    }
    finally {
      deleteQuietly(unpackDir);
      deleteQuietly(archive);
    }
  }

  private static void deleteQuietly(@Nullable Path path) {
    if (path == null) return;
    try {
      NioFiles.deleteRecursively(path);
    }
    catch (IOException e) {
      LOG.warn("Cannot delete " + path, e);
    }
  }

  /** Removes what an interrupted download left in {@code directory}. */
  private static void cleanUp(@NotNull Path directory) throws IOException {
    for (Path child : NioFiles.list(directory)) {
      String name = child.getFileName().toString();
      if (name.startsWith(DOWNLOAD_PREFIX) || name.startsWith(UNPACK_PREFIX)) {
        LOG.info("Removing the leftover of an interrupted ELP download: " + child);
        NioFiles.deleteRecursively(child);
      }
    }
  }

  private void download(@NotNull ElpRelease.Asset asset, @NotNull Path archive, @NotNull ProgressIndicator indicator)
    throws ElpInstallException {
    String url = myDownloadUrl + asset.file();
    MessageDigest digest = sha256();
    long[] received = {0};
    try {
      HttpRequests.request(url)
        .productNameAsUserAgent()
        .connectTimeout(CONNECT_TIMEOUT_MS)
        .readTimeout(READ_TIMEOUT_MS)
        .connect(request -> {
          try (InputStream in = request.getInputStream(); OutputStream out = Files.newOutputStream(archive)) {
            byte[] buffer = new byte[64 * 1024];
            int n;
            while ((n = in.read(buffer)) >= 0) {
              indicator.checkCanceled();
              received[0] += n;
              if (received[0] > asset.size()) throw wrongSize(asset, received[0], url);
              digest.update(buffer, 0, n);
              try {
                out.write(buffer, 0, n);
              }
              catch (IOException e) {
                throw new ElpInstallException("Cannot write the ELP download to " + archive.getParent() + ": " + describe(e), e);
              }
              indicator.setFraction((double)received[0] / asset.size());
            }
          }
          return null;
        });
    }
    catch (ElpInstallException e) {
      throw e;
    }
    catch (HttpRequests.HttpStatusException e) {
      throw new ElpInstallException("Cannot download ELP: HTTP " + e.getStatusCode() + " from " + url + ".", e);
    }
    catch (IOException e) {
      throw new ElpInstallException("Cannot download ELP from " + url + ": " + describe(e), e);
    }
    if (received[0] != asset.size()) throw wrongSize(asset, received[0], url);
    String actual = HexFormat.of().formatHex(digest.digest());
    if (!actual.equals(asset.sha256())) {
      throw new ElpInstallException("The ELP download from " + url + " has the wrong checksum: expected sha256 " + asset.sha256() +
                                    ", got " + actual + ".");
    }
  }

  @NotNull
  private static ElpInstallException wrongSize(@NotNull ElpRelease.Asset asset, long received, @NotNull String url) {
    return new ElpInstallException("The ELP download from " + url + " has the wrong size: expected " + asset.size() + " bytes, got " +
                                   (received > asset.size() ? "more" : received) + ".");
  }

  /** Extracts only the executable, whatever else the tarball holds. */
  @NotNull
  private static Path unpack(@NotNull ElpRelease.Asset asset, @NotNull Path archive, @NotNull Path unpackDir) throws IOException {
    AtomicReference<Path> extracted = new AtomicReference<>();
    new Decompressor.Tar(archive)
      .entryFilter(entry -> entry.type == Decompressor.Entry.Type.FILE && asset.executable().equals(StringUtil.trimStart(entry.name, "./")))
      .postProcessor(path -> extracted.set(path))
      .extract(unpackDir);
    Path path = extracted.get();
    if (path == null || !Files.isRegularFile(path)) throw new ElpInstallException(asset.file() + " has no " + asset.executable() + ".");
    return path;
  }

  /** The {@link VersionCheck} outside tests: {@code elp version} must exit with 0 within the timeout and print the version. */
  public static void runVersion(@NotNull Path executable, @NotNull ElpRelease release, @NotNull ProgressIndicator indicator)
    throws ElpInstallException {
    ProcessOutput output;
    try {
      output = new CapturingProcessHandler(new GeneralCommandLine(executable.toString(), "version"))
        .runProcessWithProgressIndicator(indicator, VERSION_TIMEOUT_MS);
    }
    catch (ExecutionException e) {
      throw new ElpInstallException("The downloaded ELP does not start: " + e.getMessage(), e);
    }
    indicator.checkCanceled();
    String printed = (output.getStdout() + output.getStderr()).trim();
    if (output.isTimeout()) {
      throw new ElpInstallException("The downloaded ELP did not answer 'elp version' in " + VERSION_TIMEOUT_MS / 1000 + " seconds.");
    }
    if (output.getExitCode() != 0) {
      throw new ElpInstallException("The downloaded ELP does not run: 'elp version' exited with " + output.getExitCode() +
                                    (printed.isEmpty() ? "." : ": " + StringUtil.first(printed, 300, true)));
    }
    if (!printed.contains(release.version())) {
      throw new ElpInstallException("The downloaded ELP printed '" + StringUtil.first(printed, 300, true) +
                                    "' for 'elp version', expected version " + release.version() + ".");
    }
    LOG.info("Downloaded ELP: " + printed);
  }

  @NotNull
  private static String describe(@NotNull IOException e) {
    String message = e.getMessage();
    if (e instanceof UnknownHostException) return "unknown host " + message + " (no network?)";
    return StringUtil.isEmpty(message) ? e.getClass().getSimpleName() : message;
  }

  @NotNull
  private static MessageDigest sha256() {
    try {
      return MessageDigest.getInstance("SHA-256");
    }
    catch (NoSuchAlgorithmException e) {
      throw new IllegalStateException(e);
    }
  }
}
