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

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.PersistentStateComponent;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.components.State;
import com.intellij.openapi.components.Storage;
import com.intellij.openapi.util.Disposer;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VirtualFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.TestOnly;

import java.util.ArrayList;
import java.util.List;

/**
 * The workspace roots that ELP serves: a stable, stored fact, not the live state of a server. In a served root every language
 * feature of an Erlang module or header comes from ELP and the plugin's own engine stays silent ({@link ElpEngine}), also while
 * the server loads, after it crashed, or while it restarts.
 * <p>
 * A root enters when, in the {@link ElpMode#ELP} position, the preflight passed and the server of that root reached
 * {@code elp/status Running} for the first time. It leaves when the user switches the engine back to {@link ElpMode#PLUGIN}
 * (the settings or "Switch back to the plugin"), or when a later preflight refuses it. A crash, a restart or a reload of the server
 * does not change it.
 * <p>
 * {@link #isUnderServedRoot} is cheap and thread-safe: a path-prefix check on an immutable snapshot, with no read action and no
 * project, so that it can be asked from an indexer.
 */
@State(name = "ErlangElpServedRoots", storages = @Storage("erlang-elp.xml"))
@Service(Service.Level.APP)
public final class ElpServedRoots implements PersistentStateComponent<ElpServedRoots.ServedRootsState> {
  public static final class ServedRootsState {
    public List<String> roots = new ArrayList<>();
  }

  /** The canonical system-independent paths of the served roots, without a trailing slash. Never modified, only replaced. */
  private volatile List<String> myRoots = List.of();

  @NotNull
  public static ElpServedRoots getInstance() {
    return ApplicationManager.getApplication().getService(ElpServedRoots.class);
  }

  @NotNull
  @Override
  public ServedRootsState getState() {
    ServedRootsState state = new ServedRootsState();
    state.roots = new ArrayList<>(myRoots);
    return state;
  }

  @Override
  public void loadState(@NotNull ServedRootsState state) {
    List<String> roots = new ArrayList<>();
    if (state.roots != null) {
      for (String root : state.roots) {
        String path = canonical(root);
        if (!path.isEmpty() && !roots.contains(path)) roots.add(path);
      }
    }
    myRoots = List.copyOf(roots);
  }

  /** Whether {@code file} is a served root or lies under one. Thread-safe; needs no read action. */
  public boolean isUnderServedRoot(@NotNull VirtualFile file) {
    return isUnderServedRoot(file.getPath());
  }

  /** Whether the system-independent {@code path} is a served root or lies under one. Thread-safe; needs no read action. */
  public boolean isUnderServedRoot(@NotNull String path) {
    List<String> roots = myRoots;
    if (roots.isEmpty()) return false;
    for (String root : roots) {
      if (isUnder(path, root)) return true;
    }
    return false;
  }

  public boolean isEmpty() {
    return myRoots.isEmpty();
  }

  /** The served roots, as stored. */
  @NotNull
  public List<String> getRoots() {
    return myRoots;
  }

  /** @return whether {@code root} was not served before */
  public synchronized boolean add(@NotNull String root) {
    String path = canonical(root);
    if (path.isEmpty() || myRoots.contains(path)) return false;
    List<String> roots = new ArrayList<>(myRoots);
    roots.add(path);
    myRoots = List.copyOf(roots);
    return true;
  }

  /** @return whether {@code root} was served */
  public synchronized boolean remove(@NotNull String root) {
    String path = canonical(root);
    if (!myRoots.contains(path)) return false;
    List<String> roots = new ArrayList<>(myRoots);
    roots.remove(path);
    myRoots = List.copyOf(roots);
    return true;
  }

  /** @return whether a root was served */
  public synchronized boolean clear() {
    boolean wasServed = !myRoots.isEmpty();
    myRoots = List.of();
    return wasServed;
  }

  /** Makes {@code roots} the served roots until {@code parentDisposable} is disposed, then restores the roots served before. */
  @TestOnly
  public void setRootsForTests(@NotNull List<String> roots, @NotNull Disposable parentDisposable) {
    List<String> old = myRoots;
    ServedRootsState state = new ServedRootsState();
    state.roots = new ArrayList<>(roots);
    loadState(state);
    Disposer.register(parentDisposable, () -> myRoots = old);
  }

  @NotNull
  private static String canonical(@NotNull String path) {
    String canonical = FileUtil.toCanonicalPath(FileUtil.toSystemIndependentName(path.trim()));
    return canonical.length() > 1 && canonical.endsWith("/") ? canonical.substring(0, canonical.length() - 1) : canonical;
  }

  private static boolean isUnder(@NotNull String path, @NotNull String root) {
    if (!path.startsWith(root)) return false;
    return path.length() == root.length() || root.endsWith("/") || path.charAt(root.length()) == '/';
  }
}
