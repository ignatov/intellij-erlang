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

import com.intellij.ide.BrowserUtil;
import com.intellij.ide.plugins.cl.PluginAwareClassLoader;
import com.intellij.openapi.application.ApplicationInfo;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.extensions.PluginDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.intellij.erlang.elp.distribution.ElpAvailability;
import org.intellij.erlang.elp.distribution.ElpDistribution;
import org.intellij.erlang.elp.distribution.ElpRelease;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * "Tell us how it went": a new issue on the plugin's tracker, prefilled with the versions that matter for ELP and three
 * questions. Nothing is sent by the plugin; the user's browser opens the page, and the user decides what to submit. The body
 * holds no project name, path or file name.
 */
public final class ElpFeedback {
  public static final String NEW_ISSUE_URL = "https://github.com/ignatov/intellij-erlang/issues/new";
  public static final String PLUGIN_ID = "org.jetbrains.erlang";

  /**
   * What the issue body tells.
   *
   * @param otpRelease the OTP release of the project's Erlang SDK, {@code null} when there is no SDK or it is unknown
   * @param elpBuild   the ELP build for the project, without any path
   */
  public record Info(@NotNull String pluginVersion,
                     @NotNull String ide,
                     @NotNull String os,
                     @Nullable String otpRelease,
                     @NotNull String elpVersion,
                     @NotNull String elpBuild) {
  }

  private ElpFeedback() {
  }

  /** Opens the new issue page in the browser; the facts are gathered off the EDT. */
  public static void open(@NotNull Project project) {
    ApplicationManager.getApplication().executeOnPooledThread(() -> {
      if (project.isDisposed()) return;
      BrowserUtil.browse(url(collect(project)));
    });
  }

  @NotNull
  @RequiresBackgroundThread
  public static Info collect(@NotNull Project project) {
    String otpRelease = ReadAction.computeBlocking(() -> {
      if (project.isDisposed() || ErlangSdkType.getSdkPath(project) == null) return null;
      ErlangSdkRelease release = ErlangSdkType.getProjectSdkRelease(project);
      return release != null ? release.getOtpRelease() : null;
    });
    ElpDistribution distribution = ElpDistribution.getInstance();
    ElpAvailability availability = distribution.availability(project);
    ElpRelease.Asset asset = availability.choice().asset();
    String build = switch (availability.kind()) {
      case CONFIGURED -> "a custom binary";
      case INSTALLED -> asset != null ? asset.directoryName() + " (installed)" : "installed";
      case DOWNLOADABLE -> asset != null ? asset.directoryName() + " (not downloaded)" : "not downloaded";
      case UNAVAILABLE -> "none fits";
    };
    ApplicationInfo application = ApplicationInfo.getInstance();
    String os = System.getProperty("os.name") + " " + System.getProperty("os.version") + " (" + System.getProperty("os.arch") + ")";
    return new Info(pluginVersion(), application.getFullApplicationName() + " (" + application.getBuild().asString() + ")", os,
                    otpRelease, distribution.getRelease().version(), build);
  }

  /**
   * The version of this plugin, {@code unknown} when the IDE does not know it, such as in tests, where the plugin's classes are not
   * loaded by a plugin class loader. Read from the class loader: the lookups by plugin id or class are internal API from 2026.2 on.
   */
  @NotNull
  public static String pluginVersion() {
    PluginDescriptor plugin = ElpFeedback.class.getClassLoader() instanceof PluginAwareClassLoader loader ? loader.getPluginDescriptor() : null;
    String version = plugin != null ? plugin.getVersion() : null;
    return version != null ? version : "unknown";
  }

  @NotNull
  public static String url(@NotNull Info info) {
    return NEW_ISSUE_URL + "?labels=elp&title=" + encode("ELP feedback") + "&body=" + encode(body(info));
  }

  @NotNull
  public static String body(@NotNull Info info) {
    return "<!-- Prefilled by the Erlang plugin, which sent nothing. Edit anything before you submit. -->\n" +
           "\n" +
           "- Erlang plugin: " + info.pluginVersion() + "\n" +
           "- IDE: " + info.ide() + "\n" +
           "- OS and CPU: " + info.os() + "\n" +
           "- OTP release of the project SDK: " + (info.otpRelease() != null ? info.otpRelease() : "unknown") + "\n" +
           "- ELP: " + info.elpVersion() + ", build " + info.elpBuild() + "\n" +
           "\n" +
           "### What worked?\n" +
           "\n" +
           "\n" +
           "### What did not work?\n" +
           "\n" +
           "\n" +
           "### Did you switch back to the plugin? Why?\n" +
           "\n";
  }

  @NotNull
  private static String encode(@NotNull String text) {
    return URLEncoder.encode(text, StandardCharsets.UTF_8).replace("+", "%20");
  }
}
