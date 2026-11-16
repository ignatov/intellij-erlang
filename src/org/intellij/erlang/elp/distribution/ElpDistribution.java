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

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.progress.ProcessCanceledException;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Disposer;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.intellij.erlang.elp.ElpConfiguration;
import org.intellij.erlang.elp.ElpServerSource;
import org.intellij.erlang.elp.ElpSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.nio.file.Path;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The ELP server the plugin downloads on demand: the public entry point for the settings and for an offer to use ELP.
 * <p>
 * {@link #availability} answers whether a project has a server, and if not, whether a build can be downloaded (which tarball, how
 * large) or why not. {@link #download} starts the download after the user asked for it. Nothing is ever downloaded otherwise.
 * The build is the one of the pinned release ({@link ElpRelease#pinned()}) for this OS and CPU and the OTP release of the project's
 * Erlang SDK ({@link ElpAssetChoice}); it is installed under the IDE's system directory ({@link ElpInstaller}).
 */
@Service(Service.Level.APP)
public final class ElpDistribution {
  private static final Logger LOG = Logger.getInstance(ElpDistribution.class);

  /** Told on the EDT, in any modality, how a download ended. */
  public interface DownloadCallback {
    /** @param elp the installed binary */
    void installed(@NotNull Path elp);

    /** @param message why the download or the installation failed, for the user; it names both checksums on a mismatch */
    void failed(@NotNull String message);

    /** The user cancelled the download. */
    default void cancelled() {
    }
  }

  private volatile ElpInstaller myInstaller;
  private volatile ElpPlatform myPlatform;
  private final Map<String, CompletableFuture<Path>> myDownloads = new ConcurrentHashMap<>();

  @NotNull
  public static ElpDistribution getInstance() {
    return ApplicationManager.getApplication().getService(ElpDistribution.class);
  }

  @NotNull
  private ElpInstaller installer() {
    ElpInstaller installer = myInstaller;
    if (installer == null) {
      installer = new ElpInstaller();
      myInstaller = installer;
    }
    return installer;
  }

  /** The pinned release. */
  @NotNull
  public ElpRelease getRelease() {
    return installer().getRelease();
  }

  /** This machine's OS and CPU; looked up once, because it looks for the glibc loader on Linux. */
  @NotNull
  @RequiresBackgroundThread
  public ElpPlatform getPlatform() {
    ElpPlatform platform = myPlatform;
    if (platform == null) {
      platform = ElpPlatform.current();
      myPlatform = platform;
    }
    return platform;
  }

  /** @return the build for this machine and the OTP release of the project's Erlang SDK, or why there is none */
  @NotNull
  @RequiresBackgroundThread
  public ElpAssetChoice chooseAsset(@NotNull Project project) {
    Object[] sdk = ReadAction.compute(() -> project.isDisposed() ? new Object[]{false, null} : new Object[]{
      ErlangSdkType.getSdkPath(project) != null, ErlangSdkType.getProjectSdkRelease(project)});
    return ElpAssetChoice.choose(getRelease(), getPlatform(), (Boolean)sdk[0], ElpAssetChoice.otpMajor((ErlangSdkRelease)sdk[1]));
  }

  /** @return the installed binary of the build for the project, or {@code null} when there is no such build or it is not installed */
  @Nullable
  @RequiresBackgroundThread
  public Path findInstalled(@NotNull Project project) {
    ElpRelease.Asset asset = chooseAsset(project).asset();
    return asset != null ? installer().findInstalled(asset) : null;
  }

  /** @return the installed binary of {@code asset}, or {@code null} when it is not installed */
  @Nullable
  @RequiresBackgroundThread
  public Path findInstalled(@NotNull ElpRelease.Asset asset) {
    return installer().findInstalled(asset);
  }

  /**
   * Is there an ELP server for {@code project}, and if not, can one be downloaded (the asset, with its size) or why not? Follows
   * the order of {@link ElpSettings#getEffectiveConfiguration(Project)}: the {@code erlang.elp.path} system property, the custom
   * path, the downloaded build. Does not look at the engine position: the answer is the same in the Plugin and the ELP position.
   * Touches the file system and the project model, so call it on a background thread.
   */
  @NotNull
  @RequiresBackgroundThread
  public ElpAvailability availability(@NotNull Project project) {
    ElpAssetChoice choice = chooseAsset(project);
    ElpSettings settings = ElpSettings.getInstance();
    String property = System.getProperty(ElpConfiguration.PATH_PROPERTY);
    if (property != null || settings.getStoredSource() == ElpServerSource.CUSTOM) {
      String path = ElpSettings.resolvePath(property, settings.getStoredSource(), settings.getStoredPath(), () -> null);
      return new ElpAvailability(ElpAvailability.Kind.CONFIGURED, path.isEmpty() ? null : path, choice, getRelease());
    }
    ElpRelease.Asset asset = choice.asset();
    if (asset == null) return new ElpAvailability(ElpAvailability.Kind.UNAVAILABLE, null, choice, getRelease());
    Path installed = installer().findInstalled(asset);
    return installed != null ? new ElpAvailability(ElpAvailability.Kind.INSTALLED, installed.toString(), choice, getRelease())
                             : new ElpAvailability(ElpAvailability.Kind.DOWNLOADABLE, null, choice, getRelease());
  }

  /** @return whether a download of {@code asset} started by {@link #download} is running */
  public boolean isDownloading(@NotNull ElpRelease.Asset asset) {
    return myDownloads.containsKey(asset.file());
  }

  /**
   * Downloads and installs {@code asset} in the background, with a cancellable progress in the status bar of {@code project}
   * ("Downloading ELP 1.1.0, 17.8 MB"). Call it only when the user asked for the download. When the same asset is being downloaded
   * already, {@code callback} is told when that download ends; an installed asset is not downloaded again. After a new installation
   * the ELP settings listeners are told, so that the servers restart with the new binary.
   *
   * @param project  the project whose status bar shows the progress, or {@code null}
   * @param callback told on the EDT, in any modality, how the download ended
   */
  public void download(@Nullable Project project, @NotNull ElpRelease.Asset asset, @NotNull DownloadCallback callback) {
    CompletableFuture<Path> created = new CompletableFuture<>();
    CompletableFuture<Path> running = myDownloads.putIfAbsent(asset.file(), created);
    CompletableFuture<Path> future = running != null ? running : created;
    future.whenComplete((path, error) -> ApplicationManager.getApplication().invokeLater(() -> {
      if (path != null) {
        callback.installed(path);
        return;
      }
      Throwable cause = error instanceof CompletionException && error.getCause() != null ? error.getCause() : error;
      if (cause instanceof CancellationException) {
        callback.cancelled();
      }
      else {
        callback.failed(cause instanceof ElpInstallException ? cause.getMessage() : "Cannot install ELP: " + cause);
      }
    }, ModalityState.any()));
    if (running != null) return;

    AtomicBoolean newlyInstalled = new AtomicBoolean();
    created.whenComplete((path, error) -> {
      myDownloads.remove(asset.file(), created);
      if (newlyInstalled.get()) {
        ApplicationManager.getApplication().invokeLater(
          () -> ApplicationManager.getApplication().getMessageBus().syncPublisher(ElpSettings.TOPIC).elpSettingsChanged(),
          ModalityState.any());
      }
    });
    ElpInstaller installer = installer();
    new Task.Backgroundable(project, "Downloading ELP " + installer.getRelease().version(), true) {
      @Override
      public void run(@NotNull ProgressIndicator indicator) {
        try {
          Path installed = installer.findInstalled(asset);
          if (installed == null) {
            installed = installer.install(asset, indicator);
            newlyInstalled.set(true);
          }
          created.complete(installed);
        }
        catch (ElpInstallException e) {
          LOG.warn(e);
          created.completeExceptionally(e);
        }
        catch (ProcessCanceledException e) {
          created.cancel(false);
          throw e;
        }
        catch (RuntimeException | Error e) {
          created.completeExceptionally(e);
          throw e;
        }
      }
    }.queue();
  }

  /** Downloads from {@code installer} and pretends to run on {@code platform} until {@code parent} is disposed. */
  @TestOnly
  public void setUpForTests(@NotNull ElpInstaller installer, @NotNull ElpPlatform platform, @NotNull Disposable parent) {
    ElpInstaller oldInstaller = myInstaller;
    ElpPlatform oldPlatform = myPlatform;
    myInstaller = installer;
    myPlatform = platform;
    Disposer.register(parent, () -> {
      myInstaller = oldInstaller;
      myPlatform = oldPlatform;
    });
  }
}
