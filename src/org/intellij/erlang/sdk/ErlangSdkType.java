/*
 * Copyright 2012-2013 Sergey Ignatov
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

import com.google.common.annotations.VisibleForTesting;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangIcons;
import org.intellij.erlang.jps.model.JpsErlangModelSerializerExtension;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
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
    super(JpsErlangModelSerializerExtension.ERLANG_SDK_TYPE_ID);
  }

  @NotNull
  @Override
  public Icon getIcon() {
    return ErlangIcons.FILE;
  }

  @NotNull
  @Override
  public Icon getIconForAddAction() {
    return getIcon();
  }

  @Nullable
  @Override
  public String suggestHomePath() {
    if (SystemInfo.isWindows) {
      return "C:\\cygwin\\bin";
    }
    else if (SystemInfo.isMac) {
      String macPorts = "/opt/local/lib/erlang";
      if (new File(macPorts).exists()) return macPorts;
      
      // For home brew we trying to find something like /usr/local/Cellar/erlang/*/lib/erlang as SDK root
      File brewRoot = new File("/usr/local/Cellar/erlang");
      if (brewRoot.exists()) {
        final Ref<String> ref = Ref.create();
        FileUtil.processFilesRecursively(brewRoot, new Processor<File>() {
          @Override
          public boolean process(File file) {
            if (!ref.isNull()) return false;
            if (!file.isDirectory()) return true;
            if ("erlang".equals(file.getName()) && file.getParent().endsWith("lib")) {
              ref.set(file.getAbsolutePath());
              return false;
            }
            return true;
          }
        });
        if (!ref.isNull()) return ref.get();
      }
      return null;
    }
    else if (SystemInfo.isLinux) {
      return "/usr/lib/erlang";
    }
    return null;
  }

  @Override
  public boolean isValidSdkHome(@NotNull final String path) {
    final File erl = getTopLevelExecutable(path);
    final File erlc = JpsErlangSdkType.getByteCodeCompilerExecutable(path);
    return erl.canExecute() && erlc.canExecute();
  }

  @NotNull
  public static File getTopLevelExecutable(@NotNull final String sdkHome) {
    return JpsErlangSdkType.getExecutable(new File(sdkHome, "bin").getAbsolutePath(), "erl");
  }

  @NotNull
  @Override
  public String suggestSdkName(@Nullable final String currentSdkName, @NotNull final String sdkHome) {
    String version = getVersionString(sdkHome);
    if (version == null) return "Unknown Erlang version at " + sdkHome;
    return "Erlang " + version;
  }

  @Nullable
  @Override
  public String getVersionString(@NotNull final String sdkHome) {
    return getReleaseString(sdkHome);
  }

  @Nullable
  @Override
  public String getDefaultDocumentationUrl(@NotNull Sdk sdk) {
    return getDefaultDocumentationUrl(getRelease(sdk));
  }

  @Nullable
  @Override
  public AdditionalDataConfigurable createAdditionalDataConfigurable(@NotNull final SdkModel sdkModel, @NotNull final SdkModificator sdkModificator) {
    return null;
  }

  @Override
  public void saveAdditionalData(@NotNull final SdkAdditionalData additionalData, @NotNull final Element additional) {
  }

  @NonNls
  @Override
  public String getPresentableName() {
    return "Erlang SDK";
  }

  @Override
  public void setupSdkPaths(@NotNull final Sdk sdk) {
    configureSdkPaths(sdk);
  }

  @VisibleForTesting
  @NotNull
  public static Sdk createMockSdk(@NotNull final String sdkHome) {
    final String release = getReleaseString(sdkHome);
    final Sdk sdk = new ProjectJdkImpl(release, getInstance());
    final SdkModificator sdkModificator = sdk.getSdkModificator();
    sdkModificator.setHomePath(sdkHome);
    sdkModificator.setVersionString(release); // must be set after home path, otherwise setting home path clears the version string
    sdkModificator.commitChanges();
    configureSdkPaths(sdk);
    return sdk;
  }

  private static void configureSdkPaths(@NotNull final Sdk sdk) {
    final SdkModificator sdkModificator = sdk.getSdkModificator();
    setupLocalSdkPaths(sdkModificator);

    final String externalDocUrl = getDefaultDocumentationUrl(getRelease(sdk));
    if (externalDocUrl != null) {
      final VirtualFile fileByUrl = VirtualFileManager.getInstance().findFileByUrl(externalDocUrl);
      sdkModificator.addRoot(fileByUrl, JavadocOrderRootType.getInstance());
    }
    sdkModificator.commitChanges();
  }

  @Nullable
  public static ErlangSdkRelease getRelease(@NotNull final Sdk sdk) {
    String versionString = sdk.getVersionString();
    try {
      return versionString == null ? null : ErlangSdkRelease.valueOf(versionString);
    } catch (IllegalArgumentException e) {
      return null;
    }
  }

  @Nullable
  private static String getReleaseString(@NotNull final String sdkHome) {
    Pattern pattern = Pattern.compile("R\\d+.*");
    // determine the version from the 'releases' directory, if it exists
    File releases = new File(sdkHome, "releases");
    if (releases.isDirectory()) {
      File firstItem = ContainerUtil.getFirstItem(FileUtil.findFilesOrDirsByMask(pattern, releases));
      if (firstItem == null) return null;
      return firstItem.getName();
    }
    else {
      // releases dir did not exist, so let's see if we can parse the version by walking up the parents
      File current = releases.getParentFile();
      while (current != null) {
        if (pattern.matcher(current.getName()).matches()) {
          return current.getName();
        }
        current = current.getParentFile();
      }
    }
    return null;
  }

  @Nullable
  private static String getDefaultDocumentationUrl(@Nullable final ErlangSdkRelease release) {
    return release == null ? null : "http://www.erlang.org/documentation/doc-" + release.getVersion();
  }

  private static void setupLocalSdkPaths(@NotNull final SdkModificator sdkModificator) {
    final String sdkHome = sdkModificator.getHomePath();

    {
      final File stdLibDir = new File(new File(sdkHome), "lib");
      if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
    }

    try {
      final String exePath = JpsErlangSdkType.getByteCodeCompilerExecutable(sdkHome).getAbsolutePath();
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

  private static boolean tryToProcessAsStandardLibraryDir(@NotNull final SdkModificator sdkModificator, @NotNull final File stdLibDir) {
    if (!isStandardLibraryDir(stdLibDir)) return false;
    final VirtualFile dir = LocalFileSystem.getInstance().findFileByIoFile(stdLibDir);
    if (dir != null) {
      sdkModificator.addRoot(dir, OrderRootType.SOURCES);
      sdkModificator.addRoot(dir, OrderRootType.CLASSES);
    }
    return true;
  }

  private static boolean isStandardLibraryDir(@NotNull final File dir) {
    return dir.isDirectory();
  }
}
