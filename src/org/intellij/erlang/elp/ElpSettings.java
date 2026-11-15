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
import com.intellij.util.messages.Topic;
import org.jetbrains.annotations.NotNull;

import java.util.Objects;

/**
 * Application-level settings of the experimental ELP language server integration. Off by default: no path, no server.
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
    public String mode = ElpMode.DIAGNOSTICS.name();
    public boolean eqwalizer;
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

  @Override
  public void loadState(@NotNull ElpSettingsState state) {
    myState = state;
  }

  /** The stored path, without the system property override. */
  @NotNull
  public String getStoredPath() {
    return Objects.requireNonNullElse(myState.path, "");
  }

  /** The stored mode, without the system property override. */
  @NotNull
  public ElpMode getStoredMode() {
    return Objects.requireNonNullElse(ElpMode.parse(myState.mode), ElpMode.DIAGNOSTICS);
  }

  /** The stored eqWAlizer switch, without the system property override. */
  public boolean getStoredEqwalizer() {
    return myState.eqwalizer;
  }

  public void update(@NotNull String path, @NotNull ElpMode mode, boolean eqwalizer) {
    if (path.equals(getStoredPath()) && mode == getStoredMode() && eqwalizer == getStoredEqwalizer()) return;
    myState.path = path;
    myState.mode = mode.name();
    myState.eqwalizer = eqwalizer;
    ApplicationManager.getApplication().getMessageBus().syncPublisher(TOPIC).elpSettingsChanged();
  }

  @NotNull
  public ElpConfiguration getEffectiveConfiguration() {
    return ElpConfiguration.resolve(myState.path, myState.mode, myState.eqwalizer);
  }
}
