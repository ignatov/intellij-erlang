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

import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.components.Service;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectUtil;
import com.intellij.openapi.roots.ModuleRootEvent;
import com.intellij.openapi.roots.ModuleRootListener;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.EnvironmentUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import com.intellij.util.concurrency.annotations.RequiresReadLock;
import com.intellij.util.text.VersionComparatorUtil;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Checks, before every start of an ELP server, that ELP can load the project, so that it is never started where it would load
 * forever or burn CPU; the plugin then answers everything in that project. Measured with ELP 1.1.0 on real projects:
 * <ul>
 *   <li>ELP has no builds for OTP older than 27, and its rebar3 integration needs OTP 27's {@code json} module.</li>
 *   <li>A project with neither a {@code rebar.config} nor a top-level {@code src} directory, such as a flat directory of
 *   {@code .erl} files, makes ELP walk up to an ancestor directory named {@code src}, burn CPU and ignore {@code shutdown}.</li>
 *   <li>In a {@code rebar.config} project ELP runs {@code rebar3}; without it the dependencies are lost.</li>
 * </ul>
 * The facts that need I/O (the SDK's release, rebar3 and its version, the server binary) are gathered off the EDT and cached per
 * project until the roots or the ELP settings change; the layout of a workspace root is read from the VFS on every check.
 */
@Service(Service.Level.PROJECT)
public final class ElpPreflight implements Disposable {
  private static final Logger LOG = Logger.getInstance(ElpPreflight.class);

  public static final String MIN_OTP = "27";
  public static final String MIN_REBAR3 = "3.24";
  private static final Pattern REBAR3_VERSION = Pattern.compile("rebar (\\d+(?:\\.\\d+)*)");
  private static final Pattern LEADING_NUMBER = Pattern.compile("^(\\d+)");
  /** The rebar3 versions by executable and modification time; running {@code rebar3 version} takes about half a second. */
  private static final Map<String, String> ourRebar3Versions = new ConcurrentHashMap<>();

  /** Why ELP is not started, or not offered, for a project. */
  public enum Reason {
    NO_SDK(true),
    OTP_TOO_OLD(true),
    UNSUPPORTED_LAYOUT(true),
    NO_REBAR3(true),
    REBAR3_NOT_RUNNABLE(true),
    REBAR3_TOO_OLD(true),
    NO_SERVER_BINARY(true),
    /** Offered on macOS only for now: it is the only system the offer was measured on. */
    NOT_MACOS(false),
    NO_REBAR_CONFIG(false),
    ERLANG_MK(false);

    private final boolean myBlocksStart;

    Reason(boolean blocksStart) {
      myBlocksStart = blocksStart;
    }

    /** {@code true} when ELP must not start at all; {@code false} when it can start but is not offered. */
    public boolean blocksStart() {
      return myBlocksStart;
    }
  }

  public record Problem(@NotNull Reason reason, @NotNull String text) {
  }

  public record Result(@NotNull List<Problem> problems) {
    /** ELP can be started: none of the problems blocks a start. */
    public boolean canStart() {
      return problems.stream().noneMatch(problem -> problem.reason().blocksStart());
    }

    /** ELP can be started and may be offered to the user. */
    public boolean offerEligible() {
      return problems.isEmpty();
    }

    @NotNull
    public Set<Reason> reasons() {
      Set<Reason> reasons = EnumSet.noneOf(Reason.class);
      for (Problem problem : problems) reasons.add(problem.reason());
      return reasons;
    }

    @NotNull
    public List<Problem> blocking() {
      return problems.stream().filter(problem -> problem.reason().blocksStart()).toList();
    }
  }

  /**
   * What a workspace root holds.
   *
   * @param path              the root, for the texts
   * @param hasRebarConfig    {@code rebar.config} or {@code rebar.config.script}
   * @param hasSrc            a top-level {@code src} directory
   * @param hasErlangMk       {@code erlang.mk}
   * @param hasMakefile       {@code Makefile} or {@code GNUmakefile}
   */
  public record Layout(@NotNull String path, boolean hasRebarConfig, boolean hasSrc, boolean hasErlangMk, boolean hasMakefile) {
    @NotNull
    @RequiresReadLock
    public static Layout of(@NotNull VirtualFile root) {
      VirtualFile src = root.findChild("src");
      return new Layout(root.getPresentableUrl(), child(root, "rebar.config") || child(root, "rebar.config.script"),
                        src != null && src.isDirectory(), child(root, "erlang.mk"), child(root, "Makefile") || child(root, "GNUmakefile"));
    }

    private static boolean child(@NotNull VirtualFile root, @NotNull String name) {
      VirtualFile child = root.findChild(name);
      return child != null && !child.isDirectory();
    }
  }

  /**
   * The facts about the project's tools.
   *
   * @param otpRelease     the OTP release of the project's Erlang SDK, {@code null} without an SDK
   * @param rebar3         the rebar3 the server gets, configured or found on its PATH; {@code null} when there is none
   * @param rebar3Version  the version {@code rebar3} reports, {@code null} when it could not be run
   * @param rebar3Error    why {@code rebar3 version} failed
   * @param serverPath     the ELP binary
   * @param serverRunnable whether {@code serverPath} is an executable file
   * @param macOs          whether the IDE runs on macOS
   */
  public record Facts(@Nullable String otpRelease,
                      @Nullable String rebar3,
                      @Nullable String rebar3Version,
                      @Nullable String rebar3Error,
                      @NotNull String serverPath,
                      boolean serverRunnable,
                      boolean macOs) {
  }

  /** The facts are gathered again after this time even when nothing told the check that they changed, such as the rebar path. */
  private static final long FACTS_TTL_MS = 60_000;

  private final Project myProject;
  private final AtomicBoolean myGathering = new AtomicBoolean();
  private volatile Facts myFacts;
  private volatile long myFactsTime;
  private volatile Set<Reason> myNotifiedReasons = Set.of();

  public ElpPreflight(@NotNull Project project) {
    myProject = project;
    project.getMessageBus().connect(this).subscribe(ModuleRootListener.TOPIC, new ModuleRootListener() {
      @Override
      public void rootsChanged(@NotNull ModuleRootEvent event) {
        invalidate();
      }
    });
  }

  @NotNull
  public static ElpPreflight getInstance(@NotNull Project project) {
    return project.getService(ElpPreflight.class);
  }

  /**
   * @return the facts of the last check, or {@code null} when they have to be gathered again, see {@link #gatherFacts()}
   */
  @Nullable
  public Facts cachedFacts() {
    return System.currentTimeMillis() - myFactsTime < FACTS_TTL_MS ? myFacts : null;
  }

  /**
   * Gathers the facts on a pooled thread, unless that is already under way, and then runs {@code then} there.
   */
  public void gatherFactsInBackground(@NotNull Runnable then) {
    if (!myGathering.compareAndSet(false, true)) return;
    ApplicationManager.getApplication().executeOnPooledThread(() -> {
      try {
        gatherFacts();
      }
      finally {
        myGathering.set(false);
      }
      if (!myProject.isDisposed()) then.run();
    });
  }

  /**
   * @return {@code true} once for each new set of reasons why ELP does not start in the project, so that the user is told once
   */
  public synchronized boolean shouldTell(@NotNull Result result) {
    Set<Reason> reasons = EnumSet.noneOf(Reason.class);
    for (Problem problem : result.blocking()) reasons.add(problem.reason());
    if (reasons.isEmpty() || reasons.equals(myNotifiedReasons)) return false;
    myNotifiedReasons = reasons;
    return true;
  }

  /** Forgets the facts, for example after the ELP settings changed. */
  public void invalidate() {
    myFacts = null;
    myFactsTime = 0;
  }

  /**
   * Gathers the facts again and caches them.
   */
  @NotNull
  @RequiresBackgroundThread
  public Facts gatherFacts() {
    String[] sdk = ReadAction.compute(() -> {
      if (myProject.isDisposed()) return new String[3];
      String home = ErlangSdkType.getSdkPath(myProject);
      String otp = StringUtil.isNotEmpty(home) ? ErlangSdkType.getRelease(myProject).getOtpRelease() : null;
      return new String[]{home, otp, RebarSettings.getInstance(myProject).getRebarPath()};
    });
    String sdkHome = sdk[0];
    String otpRelease = sdk[1];
    String configuredRebar = sdk[2];
    String serverPath = ElpSettings.getInstance().getEffectiveConfiguration().path();
    File server = new File(serverPath);
    String path = ElpEnvironment.buildPath(EnvironmentUtil.getValue("PATH"), sdkHome, configuredRebar);
    String rebar3 = findRebar3(configuredRebar, path);
    String rebar3Version = null;
    String rebar3Error = null;
    if (rebar3 != null) {
      String[] versionOrError = rebar3Version(rebar3, path);
      rebar3Version = versionOrError[0];
      rebar3Error = versionOrError[1];
    }
    Facts facts = new Facts(otpRelease, rebar3, rebar3Version, rebar3Error, serverPath,
                            !serverPath.isEmpty() && server.isFile() && server.canExecute(), SystemInfo.isMac);
    myFacts = facts;
    myFactsTime = System.currentTimeMillis();
    return facts;
  }

  /**
   * The check for the project's own root: the project directory, or else its first content root.
   */
  @NotNull
  @RequiresBackgroundThread
  public static Result check(@NotNull Project project) {
    Facts facts = getInstance(project).gatherFacts();
    Layout layout = ReadAction.compute(() -> {
      if (project.isDisposed()) return null;
      VirtualFile root = ProjectUtil.guessProjectDir(project);
      if (root == null) {
        VirtualFile[] contentRoots = ProjectRootManager.getInstance(project).getContentRoots();
        root = contentRoots.length > 0 ? contentRoots[0] : null;
      }
      return root != null ? Layout.of(root) : null;
    });
    return evaluate(facts, layout);
  }

  /**
   * @param layout the workspace root ELP would be started for, {@code null} when there is none
   */
  @NotNull
  public static Result evaluate(@NotNull Facts facts, @Nullable Layout layout) {
    List<Problem> problems = new ArrayList<>();
    if (facts.otpRelease() == null) {
      problems.add(new Problem(Reason.NO_SDK, "No Erlang SDK is configured for the project; ELP needs OTP " + MIN_OTP + " or newer."));
    }
    else if (!isAtLeastOtp(facts.otpRelease(), MIN_OTP)) {
      problems.add(new Problem(Reason.OTP_TOO_OLD, "The project's Erlang SDK is OTP " + facts.otpRelease() + "; ELP needs OTP " +
                                                   MIN_OTP + " or newer."));
    }
    if (layout == null || !layout.hasRebarConfig() && !layout.hasSrc()) {
      String where = layout == null ? "The project has no directory that" : layout.path() + " is a directory that";
      problems.add(new Problem(Reason.UNSUPPORTED_LAYOUT, where + " has neither a rebar.config nor a src directory; ELP would " +
                                                          "search the parent directories for a project."));
    }
    if (layout != null && layout.hasRebarConfig()) {
      if (facts.rebar3() == null) {
        problems.add(new Problem(Reason.NO_REBAR3, "The project has a rebar.config, but rebar3 is neither set on the Erlang " +
                                                   "External Tools page nor on the PATH."));
      }
      else if (facts.rebar3Version() == null) {
        problems.add(new Problem(Reason.REBAR3_NOT_RUNNABLE, facts.rebar3() + " did not report its version" +
                                                             (StringUtil.isEmpty(facts.rebar3Error()) ? "." : ": " + facts.rebar3Error())));
      }
      else if (VersionComparatorUtil.compare(facts.rebar3Version(), MIN_REBAR3) < 0) {
        problems.add(new Problem(Reason.REBAR3_TOO_OLD, "rebar3 " + facts.rebar3Version() + " is too old; ELP needs rebar3 " +
                                                        MIN_REBAR3 + " or newer."));
      }
    }
    if (!facts.serverRunnable()) {
      problems.add(new Problem(Reason.NO_SERVER_BINARY, "The ELP binary " + (facts.serverPath().isEmpty() ? "is not set" :
                                                                             facts.serverPath() + " is not an executable file") + "."));
    }
    if (!facts.macOs()) {
      problems.add(new Problem(Reason.NOT_MACOS, "ELP is offered on macOS only for now."));
    }
    if (layout != null && !layout.hasRebarConfig()) {
      if (layout.hasErlangMk() || layout.hasMakefile()) {
        problems.add(new Problem(Reason.ERLANG_MK, "The project is built with erlang.mk or a Makefile, not rebar3."));
      }
      else {
        problems.add(new Problem(Reason.NO_REBAR_CONFIG, "The project has no rebar.config."));
      }
    }
    return new Result(problems);
  }

  static boolean isAtLeastOtp(@NotNull String otpRelease, @NotNull String minimum) {
    Matcher matcher = LEADING_NUMBER.matcher(otpRelease);
    return matcher.find() && Integer.parseInt(matcher.group(1)) >= Integer.parseInt(minimum);
  }

  /**
   * @return the configured rebar when it is a rebar3, else {@code rebar3} in a directory of {@code path}
   */
  @Nullable
  static String findRebar3(@Nullable String configured, @Nullable String path) {
    if (ElpServerEnvironment.isRebar3(configured)) {
      File file = new File(configured);
      return file.isFile() ? file.getPath() : null;
    }
    if (path == null) return null;
    for (String dir : StringUtil.split(path, File.pathSeparator)) {
      File candidate = new File(dir, "rebar3");
      if (candidate.isFile() && candidate.canExecute()) return candidate.getPath();
    }
    return null;
  }

  /**
   * Runs {@code rebar3 version} with the server's PATH: rebar3 is an escript and needs {@code escript} on it.
   *
   * @return the version and {@code null}, or {@code null} and why it failed
   */
  @NotNull
  @RequiresBackgroundThread
  static String[] rebar3Version(@NotNull String rebar3, @Nullable String path) {
    String key = rebar3 + "@" + new File(rebar3).lastModified() + "@" + path;
    String cached = ourRebar3Versions.get(key);
    if (cached != null) return new String[]{cached, null};
    GeneralCommandLine commandLine = new GeneralCommandLine(rebar3, "version");
    if (path != null) commandLine.withEnvironment("PATH", path);
    try {
      ProcessOutput output = new CapturingProcessHandler(commandLine).runProcess(30_000);
      String version = parseRebar3Version(output.getStdout());
      if (output.isTimeout()) return new String[]{null, "it did not finish in 30 seconds"};
      if (version == null) {
        String text = StringUtil.trim(output.getStdout() + "\n" + output.getStderr());
        return new String[]{null, "exit code " + output.getExitCode() + (text.isEmpty() ? "" : ", " + StringUtil.shortenTextWithEllipsis(text, 200, 0))};
      }
      ourRebar3Versions.put(key, version);
      return new String[]{version, null};
    }
    catch (Exception e) {
      LOG.info("Cannot run " + commandLine.getCommandLineString(), e);
      return new String[]{null, String.valueOf(e.getMessage())};
    }
  }

  /**
   * @param output the output of {@code rebar3 version}, for example {@code rebar 3.28.0 on Erlang/OTP 29 Erts 17.1}
   */
  @Nullable
  static String parseRebar3Version(@NotNull String output) {
    Matcher matcher = REBAR3_VERSION.matcher(output);
    return matcher.find() ? matcher.group(1) : null;
  }

  @Override
  public void dispose() {
  }
}
