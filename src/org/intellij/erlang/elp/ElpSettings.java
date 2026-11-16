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

package org.intellij.erlang.elp;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.project.Project;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.messages.Topic;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.file.Path;
import java.util.Objects;
import java.util.function.Supplier;

/**
 * Application-level settings of the experimental ELP language server integration. Off by default: the {@link ElpMode#PLUGIN}
 * position starts no server. The binary is the one the plugin downloaded ({@link ElpServerSource#DOWNLOADED}) or a path the user
 * typed ({@link ElpServerSource#CUSTOM}); settings stored before the source existed have no source, and a non-empty path among
 * them means a custom path.
 */
@State(name = "ErlangElpSettings", storages = @Storage("erlang-elp.xml"))
@Service(Service.Level.APP)
public final class ElpSettings implements PersistentStateComponent<ElpSettings.ElpSettingsState> {
  @Topic.AppLevel
  public static final Topic<Listener> TOPIC = new Topic<>(Listener.class, Topic.BroadcastDirection.NONE);

  public interface Listener {
    void elpSettingsChanged();
  }

  public static final class ElpSettingsState {
    public String path = "";
    /** An {@link ElpServerSource} name; empty in settings stored before it existed. */
    public String source = "";
    public String mode = ElpMode.PLUGIN.name();
    public boolean eqwalizer;
    /** The plugin version for which the offer to try ELP was last shown; empty when it was never shown. */
    public String offerShownForVersion = "";
    /** The user chose "Don't ask again" on the offer to try ELP. */
    public boolean offerNeverAsk;
    /**
     * When the offer to try ELP was last shown or answered, in milliseconds since the epoch; 0, as in settings stored before it
     * existed, is long ago.
     */
    public long offerAnsweredTime;
    /** How many times the offer to try ELP was shown, at most 2; 0 in settings stored before it existed, see {@link #getOfferShownCount}. */
    public int offerShownCount;
  }

  @NotNull private ElpSettingsState myState = new ElpSettingsState();

  @NotNull
  public static ElpSettings getInstance() {
    return ApplicationManager.getApplication().getService(ElpSettings.class);
  }

  @NotNull
  @Override
  public ElpSettingsState getState() {
    return myState;
  }

  /**
   * Rewrites a stored mode of the prototype ({@code DIAGNOSTICS}, {@code ALL}) to its engine position, so the next save stores
   * {@code PLUGIN} or {@code ELP}. An unknown mode is kept: it reads as {@link ElpMode#PLUGIN}.
   */
  @Override
  public void loadState(@NotNull ElpSettingsState state) {
    ElpMode mode = ElpMode.parse(state.mode);
    if (mode != null) state.mode = mode.name();
    myState = state;
  }

  /** The stored path, without the system property override. */
  @NotNull
  public String getStoredPath() {
    return Objects.requireNonNullElse(myState.path, "");
  }

  /**
   * The stored source, without the system property override. Settings without a source (stored before it existed) mean a custom
   * path when the path is not empty, and the downloaded server otherwise.
   */
  @NotNull
  public ElpServerSource getStoredSource() {
    ElpServerSource source = ElpServerSource.parse(myState.source);
    if (source != null) return source;
    return getStoredPath().isBlank() ? ElpServerSource.DOWNLOADED : ElpServerSource.CUSTOM;
  }

  /** The stored mode, without the system property override. */
  @NotNull
  public ElpMode getStoredMode() {
    return Objects.requireNonNullElse(ElpMode.parse(myState.mode), ElpMode.PLUGIN);
  }

  /** The stored eqWAlizer switch, without the system property override. */
  public boolean getStoredEqwalizer() {
    return myState.eqwalizer;
  }

  /** Stores the settings and, when they changed, tells the running servers. The custom path is kept with the downloaded source. */
  public void update(@NotNull ElpServerSource source, @NotNull String path, @NotNull ElpMode mode, boolean eqwalizer) {
    boolean changed = source != getStoredSource() || !path.equals(getStoredPath()) || mode != getStoredMode() ||
                      eqwalizer != getStoredEqwalizer();
    myState.source = source.name();
    myState.path = path;
    myState.mode = mode.name();
    myState.eqwalizer = eqwalizer;
    // Back in the Plugin position no root is ELP's any more, see ElpServedRoots.
    if (mode == ElpMode.PLUGIN) ElpServedRoots.getInstance().clear();
    if (changed)ApplicationManager.getApplication().getMessageBus().syncPublisher(TOPIC).elpSettingsChanged();
  }

  /** Sets the engine position and keeps the rest; tells the running servers when it changed. */
  public void setMode(@NotNull ElpMode mode) {
    update(getStoredSource(), getStoredPath(), mode, getStoredEqwalizer());
  }

  /** The plugin version for which the offer to try ELP was last shown, empty when it was never shown. */
  @NotNull
  public String getOfferShownForVersion() {
    return Objects.requireNonNullElse(myState.offerShownForVersion, "");
  }

  public void setOfferShownForVersion(@NotNull String version) {
    myState.offerShownForVersion = version;
  }

  /** When the offer to try ELP was last shown or answered, in milliseconds since the epoch; 0 when it never was. */
  public long getOfferAnsweredTime() {
    return myState.offerAnsweredTime;
  }

  public void setOfferAnsweredTime(long time) {
    myState.offerAnsweredTime = time;
  }

  /**
   * How many times the offer to try ELP was shown. Settings stored before the count existed have only the version: once, when it
   * is set.
   */
  public int getOfferShownCount() {
    return Math.max(myState.offerShownCount, getOfferShownForVersion().isEmpty() ? 0 : 1);
  }

  public void setOfferShownCount(int count) {
    myState.offerShownCount = count;
  }

  public boolean isOfferNeverAsk() {
    return myState.offerNeverAsk;
  }

  public void setOfferNeverAsk(boolean neverAsk) {
    myState.offerNeverAsk = neverAsk;
  }

  /**
   * The settings in effect without a project: the system properties, then the custom path. With the downloaded server the path is
   * empty here, because the build depends on the project's OTP release; see {@link #getEffectiveConfiguration(Project)}.
   */
  @NotNull
  public ElpConfiguration getEffectiveConfiguration() {
    return ElpConfiguration.resolve(getStoredSource() == ElpServerSource.CUSTOM ? myState.path : "", myState.mode, myState.eqwalizer);
  }

  /**
   * The settings in effect for {@code project}. The path is, in this order: the {@code erlang.elp.path} system property when it is
   * set (even to an empty string); the custom path when the user chose it; the build the plugin downloaded for the OTP release of
   * the project's Erlang SDK when it is installed; otherwise empty, and no server is started. Never downloads anything.
   */
  @NotNull
  @RequiresBackgroundThread
  public ElpConfiguration getEffectiveConfiguration(@NotNull Project project) {
    ElpConfiguration configuration = getEffectiveConfiguration();
    String path = resolvePath(System.getProperty(ElpConfiguration.PATH_PROPERTY), getStoredSource(), getStoredPath(),
                              () -> ElpDistribution.getInstance().findInstalled(project));
    return path.equals(configuration.path()) ? configuration : new ElpConfiguration(path, configuration.mode(), configuration.eqwalizer());
  }

  /**
   * The order of {@link #getEffectiveConfiguration(Project)}.
   *
   * @param downloaded the installed downloaded binary for the project, or {@code null}; asked only for the downloaded source
   */
  @NotNull
  public static String resolvePath(@Nullable String propertyPath, @NotNull ElpServerSource source, @Nullable String storedPath,
                                   @NotNull Supplier<@Nullable Path> downloaded) {
    if (propertyPath != null) return propertyPath.trim();
    if (source == ElpServerSource.CUSTOM) return Objects.requireNonNullElse(storedPath, "").trim();
    Path installed = downloaded.get();
    return installed != null ? installed.toString() : "";
  }

  /**
   * Whether the {@link ElpMode#ELP} position is in effect and a server may be running for some project: a path is set, or the
   * downloaded server is chosen. Touches neither the file system nor the project model, so it may be asked on any thread.
   */
  public boolean isElpPositionWithServer() {
    ElpConfiguration configuration = getEffectiveConfiguration();
    if (configuration.mode() != ElpMode.ELP) return false;
    return configuration.isEnabled() ||
           System.getProperty(ElpConfiguration.PATH_PROPERTY) == null && getStoredSource() == ElpServerSource.DOWNLOADED;
  }
}
