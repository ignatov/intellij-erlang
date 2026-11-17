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

import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.EnvironmentUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

/**
 * The environment variables of the ELP server process, on top of the IDE's own environment:
 * <ul>
 *   <li>{@code PATH}: ELP runs {@code erl} at startup to find OTP and {@code escript} for its Erlang service; an IDE started from
 *   the macOS Dock has neither on its PATH, and then ELP never loads the project. The {@code bin} directory of the project's
 *   Erlang SDK goes first, then the directory of the configured rebar, see {@link ElpEnvironment#buildPath}.</li>
 *   <li>{@code ELP_REBAR3}: the configured rebar3, which ELP runs for {@code rebar.config} projects. With it, a PATH that has only
 *   OTP's {@code bin} loads the project fully.</li>
 *   <li>{@code TMPDIR}: a directory that the plugin owns, one per server process. ELP extracts a 23.7 MB eqWAlizer binary into
 *   {@code $TMPDIR} for every session and does not delete it even on a clean shutdown; the plugin deletes the directory when the
 *   process ends and the directories of earlier sessions when the next server starts.</li>
 * </ul>
 */
public final class ElpServerEnvironment {
  private static final Logger LOG = Logger.getInstance(ElpServerEnvironment.class);

  public static final String PATH = "PATH";
  public static final String ELP_REBAR3 = "ELP_REBAR3";
  public static final String TMPDIR = "TMPDIR";

  private static final String SESSION_PREFIX = "session-";
  private static final AtomicInteger ourSessionCounter = new AtomicInteger();
  /** The temporary directories of the server processes of this IDE that have not ended yet. */
  private static final Set<Path> ourLiveSessions = ConcurrentHashMap.newKeySet();

  private ElpServerEnvironment() {
  }

  /**
   * @param inheritedPath the PATH the process would get otherwise
   * @param sdkHome       the home of the project's Erlang SDK, if any
   * @param rebarPath     the configured rebar executable, if any; only a rebar3 becomes {@code ELP_REBAR3}
   * @param tmpDir        the temporary directory of the server process, if any
   * @return the variables to set; PATH is left out when there is nothing to set it to
   */
  @NotNull
  public static Map<String, String> build(@Nullable String inheritedPath,
                                          @Nullable String sdkHome,
                                          @Nullable String rebarPath,
                                          @Nullable Path tmpDir) {
    Map<String, String> variables = new LinkedHashMap<>();
    String path = ElpEnvironment.buildPath(inheritedPath, sdkHome, rebarPath);
    if (path != null) variables.put(PATH, path);
    if (isRebar3(rebarPath)) variables.put(ELP_REBAR3, rebarPath);
    if (tmpDir != null) variables.put(TMPDIR, tmpDir.toString());
    return variables;
  }

  /**
   * @return the variables for a server of {@code project} with {@code tmpDir} as its temporary directory
   */
  @NotNull
  @RequiresBackgroundThread
  public static Map<String, String> forProject(@NotNull Project project, @Nullable Path tmpDir) {
    String[] sdkAndRebar = ReadAction.computeBlocking(() -> project.isDisposed() ? new String[2] : new String[]{
      ErlangSdkType.getSdkPath(project),
      RebarSettings.getInstance(project).getRebarPath()
    });
    return build(EnvironmentUtil.getValue(PATH), sdkAndRebar[0], sdkAndRebar[1], tmpDir);
  }

  static boolean isRebar3(@Nullable String rebarPath) {
    return StringUtil.isNotEmpty(rebarPath) && new File(rebarPath).getName().startsWith("rebar3");
  }

  /**
   * Creates the temporary directory of a new server process, after deleting those of the earlier sessions that have ended,
   * including the ones a crashed IDE left behind.
   *
   * @return the new directory, or {@code null} when it cannot be created; the server then gets the IDE's {@code TMPDIR}
   */
  @Nullable
  @RequiresBackgroundThread
  public static Path createSessionDirectory() {
    Path base = sessionsBase();
    deleteEndedSessions(base);
    try {
      Files.createDirectories(base);
      Path dir = Files.createDirectory(base.resolve(SESSION_PREFIX + ProcessHandle.current().pid() + "-" + ourSessionCounter.incrementAndGet()));
      ourLiveSessions.add(dir);
      return dir;
    }
    catch (IOException e) {
      LOG.warn("Cannot create a temporary directory for ELP under " + base, e);
      return null;
    }
  }

  /**
   * Deletes the temporary directory of a server process that has ended.
   */
  @RequiresBackgroundThread
  public static void deleteSessionDirectory(@NotNull Path dir) {
    ourLiveSessions.remove(dir);
    if (!FileUtil.delete(dir.toFile())) LOG.info("Could not delete " + dir);
  }

  private static void deleteEndedSessions(@NotNull Path base) {
    if (!Files.isDirectory(base)) return;
    try (Stream<Path> children = Files.list(base)) {
      children.filter(child -> child.getFileName().toString().startsWith(SESSION_PREFIX) && !ourLiveSessions.contains(child))
        .forEach(child -> FileUtil.delete(child.toFile()));
    }
    catch (IOException e) {
      LOG.info("Cannot list " + base, e);
    }
  }

  @NotNull
  static Path sessionsBase() {
    return Path.of(PathManager.getSystemPath(), "erlang-elp", "tmp");
  }
}
