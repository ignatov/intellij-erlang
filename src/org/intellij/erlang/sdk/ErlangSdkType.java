/*
 * Copyright 2012 Sergey Ignatov
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

package org.intellij.erlang.sdk;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.ide.DataManager;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangIcons;
import org.jdom.Element;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;
import java.util.regex.Pattern;

/**
 * @author ignatov
 */
public class ErlangSdkType extends SdkType {
  @NotNull
  public static ErlangSdkType getInstance() {
    return SdkType.findInstance(ErlangSdkType.class);
  }

  public ErlangSdkType() {
    super("Erlang SDK");
  }

  @NotNull
  public Icon getIcon() {
    return ErlangIcons.FILE;
  }

  @Override
  @NotNull
  public Icon getIconForAddAction() {
    return getIcon();
  }

  @Nullable
  public String suggestHomePath() {
    if (SystemInfo.isWindows) {
      return "C:\\cygwin\\bin";
    }
    else if (SystemInfo.isLinux) {
      return "/usr/lib/erlang";
    }
    return null;
  }

  public boolean isValidSdkHome(@NotNull final String path) {
    final File erl = getTopLevelExecutable(path);
    final File erlc = getByteCodeCompilerExecutable(path);
    return erl.canExecute() && erlc.canExecute();
  }

  @NotNull
  public static File getTopLevelExecutable(@NotNull final String sdkHome) {
    return getExecutable(new File(sdkHome, "bin").getAbsolutePath(), "erl");
  }

  @NotNull
  public static File getByteCodeCompilerExecutable(@NotNull final String sdkHome) {
    return getExecutable(new File(sdkHome, "bin").getAbsolutePath(), "erlc");
  }

  @NotNull
  public String suggestSdkName(@Nullable final String currentSdkName, @NotNull final String sdkHome) {
    String version = getVersionString(sdkHome);
    if (version == null) return "Unknown at " + sdkHome;
    return "Erlang " + version;
  }

  @Nullable
  public String getVersionString(@NotNull final String sdkHome) {
    File releases = new File(sdkHome, "releases");
    if (releases.isDirectory()) {
      Pattern pattern = Pattern.compile("R\\d+.*");
      File firstItem = ContainerUtil.getFirstItem(FileUtil.findFilesOrDirsByMask(pattern, releases));
      if (firstItem == null) return null;
      return firstItem.getName();
    }
    return null;
  }

  @Nullable
  public AdditionalDataConfigurable createAdditionalDataConfigurable(@NotNull final SdkModel sdkModel, @NotNull final SdkModificator sdkModificator) {
    return null;
  }

  public void saveAdditionalData(@NotNull final SdkAdditionalData additionalData, @NotNull final Element additional) {
  }

  @NonNls
  public String getPresentableName() {
    return "Erlang SDK";
  }

  public void setupSdkPaths(@NotNull final Sdk sdk) {
    final SdkModificator[] sdkModificatorHolder = new SdkModificator[]{null};
    final ProgressManager progressManager = ProgressManager.getInstance();
    final Project project = PlatformDataKeys.PROJECT.getData(DataManager.getInstance().getDataContext());
    final Task.Modal setupTask = new Task.Modal(project, "Setting up library files", false) {
      public void run(@NotNull final ProgressIndicator indicator) {
        sdkModificatorHolder[0] = setupSdkPathsUnderProgress(sdk);
      }
    };
    progressManager.run(setupTask);
    if (sdkModificatorHolder[0] != null) sdkModificatorHolder[0].commitChanges();
  }

  @NotNull
  protected SdkModificator setupSdkPathsUnderProgress(@NotNull final Sdk sdk) {
    final SdkModificator sdkModificator = sdk.getSdkModificator();
    doSetupSdkPaths(sdkModificator);
    return sdkModificator;
  }

  public void doSetupSdkPaths(@NotNull final SdkModificator sdkModificator) {
    final String sdkHome = sdkModificator.getHomePath();

    {
      final File stdLibDir = new File(new File(sdkHome), "lib");
      if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
    }

    try {
      final String exePath = getByteCodeCompilerExecutable(sdkHome).getAbsolutePath();
      final ProcessOutput processOutput = ErlangSystemUtil.getProcessOutput(sdkHome, exePath, "-where");
      if (processOutput.getExitCode() == 0) {
        final String stdout = processOutput.getStdout().trim();
        if (!stdout.isEmpty()) {
          if (SystemInfo.isWindows && stdout.startsWith("/")) {
            for (final File root : File.listRoots()) {
              final File stdLibDir = new File(root, stdout);
              if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
            }
          }
          else {
            final File stdLibDir = new File(stdout);
            if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
          }
        }
      }
    } catch (final ExecutionException ignore) {
    }

    final File stdLibDir = new File("/usr/lib/erlang");
    tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir);
  }

  private boolean tryToProcessAsStandardLibraryDir(@NotNull final SdkModificator sdkModificator, @NotNull final File stdLibDir) {
    if (!isStandardLibraryDir(stdLibDir)) return false;
    final VirtualFile dir = LocalFileSystem.getInstance().findFileByIoFile(stdLibDir);
    if (dir != null) {
      sdkModificator.addRoot(dir, OrderRootType.SOURCES);
      sdkModificator.addRoot(dir, OrderRootType.CLASSES);
    }
    return true;
  }

  private boolean isStandardLibraryDir(@NotNull final File dir) {
    return dir.isDirectory();
  }

  @NotNull
  private static File getExecutable(@NotNull final String path, @NotNull final String command) {
    return new File(path, SystemInfo.isWindows ? command + ".exe" : command);
  }
}
