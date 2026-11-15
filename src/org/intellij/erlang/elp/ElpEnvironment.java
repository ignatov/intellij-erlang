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

import com.intellij.openapi.application.ReadAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.EnvironmentUtil;
import com.intellij.util.concurrency.annotations.RequiresBackgroundThread;
import org.intellij.erlang.rebar.settings.RebarSettings;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * The PATH of the ELP server process. ELP runs {@code erl} and, in a rebar3 project, {@code rebar3} to load the project,
 * so the {@code bin} directory of the project's Erlang SDK and the directory of the configured rebar go first.
 */
public final class ElpEnvironment {
  private ElpEnvironment() {
  }

  /**
   * @return the PATH for the server process of the project, or {@code null} to keep the inherited environment as it is
   */
  @Nullable
  @RequiresBackgroundThread
  public static String serverPath(@NotNull Project project) {
    String[] sdkAndRebar = ReadAction.compute(() -> project.isDisposed() ? new String[2] : new String[]{
      ErlangSdkType.getSdkPath(project),
      RebarSettings.getInstance(project).getRebarPath()
    });
    return buildPath(EnvironmentUtil.getValue("PATH"), sdkAndRebar[0], sdkAndRebar[1]);
  }

  /**
   * @param inheritedPath the PATH the process would get otherwise
   * @param sdkHome       the home of the project's Erlang SDK, if any
   * @param rebarPath     the configured rebar executable, if any
   * @return {@code inheritedPath} with {@code sdkHome/bin} and the directory of {@code rebarPath} in front of it,
   * or {@code inheritedPath} itself when there is nothing to add
   */
  @Nullable
  public static String buildPath(@Nullable String inheritedPath, @Nullable String sdkHome, @Nullable String rebarPath) {
    Set<String> prepended = new LinkedHashSet<>();
    if (StringUtil.isNotEmpty(sdkHome)) {
      prepended.add(new File(sdkHome, "bin").getPath());
    }
    if (StringUtil.isNotEmpty(rebarPath)) {
      String rebarDir = new File(rebarPath).getParent();
      if (StringUtil.isNotEmpty(rebarDir)) prepended.add(rebarDir);
    }
    if (prepended.isEmpty()) return inheritedPath;
    String head = String.join(File.pathSeparator, prepended);
    return StringUtil.isEmpty(inheritedPath) ? head : head + File.pathSeparator + inheritedPath;
  }
}
