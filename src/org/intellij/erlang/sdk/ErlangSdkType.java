/*
 * Copyright 2012-2014 Sergey Ignatov
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
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.psi.PsiElement;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.icons.ErlangIcons;
import org.intellij.erlang.jps.model.JpsErlangModelSerializerExtension;
import org.intellij.erlang.jps.model.JpsErlangSdkType;
import org.jdom.Element;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class ErlangSdkType extends SdkType {
  private static final String OTP_RELEASE_PREFIX_LINE = "ErlangSdkType_OTP_RELEASE:";
  private static final String ERTS_VERSION_PREFIX_LINE = "ErlangSdkType_ERTS_VERSION:";
  private static final String PRINT_VERSION_INFO_EXPRESSION =
    "io:format(\"~n~s~n~s~n~s~n~s~n\",[" +
    "\"" + OTP_RELEASE_PREFIX_LINE + "\"," +
    "erlang:system_info(otp_release)," +
    "\"" + ERTS_VERSION_PREFIX_LINE + "\"," +
    "erlang:system_info(version)" +
    "]),erlang:halt().";
  private static final Logger LOG = Logger.getInstance(ErlangSdkType.class);

  private final Map<String, ErlangSdkRelease> mySdkHomeToReleaseCache = ApplicationManager.getApplication().isUnitTestMode() ?
                                                                        new HashMap<>() : new WeakHashMap<>();

  @NotNull
  public static ErlangSdkType getInstance() {
    return SdkType.findInstance(ErlangSdkType.class);
  }

  private ErlangSdkType() {
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
      return suggestWindowsErlangPath();
    }
    else if (SystemInfo.isMac) {
      return suggestMacOSErlangPath();
    }
    else if (SystemInfo.isLinux) {
      return suggestLinuxErlangPath();
    }
    return null;
  }

  private static @Nullable String suggestMacOSErlangPath() {
    String macPorts = "/opt/local/lib/erlang";
    if (new File(macPorts).exists()) return macPorts;

    var searchRoots = new String[]{
      System.getProperty("user.home") + "/.asdf/installs/erlang",
      "/opt/brewroot/Cellar/erlang", // Brew install location for M1 arm64
      "/usr/local/Cellar/erlang",
      "/usr/local/Cellar/erlang-r14",
      "/usr/local/Cellar/erlang-r15",
      "/usr/local/Cellar/erlang-r16",
      };

    return searchForErlangRecursivelyIn(searchRoots);
  }

  // Look into nested directories under each of the roots, and see if there's */lib/erlang in them
  // Reason is that on MacOS and Linux the erl/erlc binaries are often installed separately
  // from the rest of the OTP files
  private static @Nullable String searchForErlangRecursivelyIn(String[] roots) {
    // For home brew we trying to find something like /usr/local/Cellar/erlang/*/lib/erlang as SDK root
    for (String trySearchIn : roots) {
      File brewRoot = new File(trySearchIn);
      if (brewRoot.exists()) {
        final Ref<String> ref = Ref.create(); // store return value from lambda below

        FileUtil.processFilesRecursively(brewRoot, file -> {
          if (!ref.isNull()) return false;
          if (!file.isDirectory()) return true;
          if ("erlang".equals(file.getName()) && file.getParent().endsWith("lib")) {
            ref.set(file.getAbsolutePath());
            return false;
          }
          return true;
        });

        if (!ref.isNull()) return ref.get();
      }
    }
    return null;
  }

  private static @Nullable String suggestLinuxErlangPath() {
    var searchRoots = new String[]{
      System.getProperty("user.home") + "/.asdf/installs/erlang",
      "/usr/lib/erlang",
      };

    return searchForErlangRecursivelyIn(searchRoots);
  }

  @NotNull
  private static String suggestWindowsErlangPath() {
    return "C:\\cygwin\\bin";
  }

  @Override
  public boolean isValidSdkHome(@NotNull String path) {
    File erl = JpsErlangSdkType.getByteCodeInterpreterExecutable(path);
    File erlc = JpsErlangSdkType.getByteCodeCompilerExecutable(path);
    return erl.canExecute() && erlc.canExecute();
  }

  @NotNull
  @Override
  public String suggestSdkName(@Nullable String currentSdkName, @NotNull String sdkHome) {
    return getDefaultSdkName(sdkHome, detectSdkVersion(sdkHome));
  }

  @Nullable
  @Override
  public String getVersionString(@NotNull String sdkHome) {
    return getVersionString(detectSdkVersion(sdkHome));
  }

  @Nullable
  @Override
  public String getDefaultDocumentationUrl(@NotNull Sdk sdk) {
    return getDefaultDocumentationUrl(getRelease(sdk));
  }

  @Nullable
  @Override
  public AdditionalDataConfigurable createAdditionalDataConfigurable(@NotNull SdkModel sdkModel,
                                                                     @NotNull SdkModificator sdkModificator) {
    return null;
  }

  @Override
  public void saveAdditionalData(@NotNull SdkAdditionalData additionalData, @NotNull Element additional) {
  }

  @NotNull
  @NonNls
  @Override
  public String getPresentableName() {
    return "Erlang SDK";
  }

  @Override
  public void setupSdkPaths(@NotNull Sdk sdk) {
    configureSdkPaths(sdk);
  }

  @Nullable
  public static String getSdkPath(@NotNull final Project project) {
    if (ErlangSystemUtil.isSmallIde()) {
      return ErlangSdkForSmallIdes.getSdkHome(project);
    }
    Sdk sdk = ProjectRootManager.getInstance(project).getProjectSdk();
    return sdk != null && sdk.getSdkType() == getInstance() ? sdk.getHomePath() : null;
  }

  @Nullable
  public static ErlangSdkRelease getRelease(@NotNull PsiElement element) {
    if (ErlangSystemUtil.isSmallIde()) {
      return getReleaseForSmallIde(element.getProject());
    }

    Module module = ModuleUtilCore.findModuleForPsiElement(element);
    ErlangSdkRelease byModuleSdk = getRelease(module != null ? ModuleRootManager.getInstance(module).getSdk() : null);

    return byModuleSdk != null ? byModuleSdk : getRelease(element.getProject());
  }

  @Nullable
  public static ErlangSdkRelease getRelease(@NotNull Project project) {
    if (ErlangSystemUtil.isSmallIde()) {
      return getReleaseForSmallIde(project);
    }
    return getRelease(ProjectRootManager.getInstance(project).getProjectSdk());
  }

  @TestOnly
  @NotNull
  public static Sdk createMockSdk(@NotNull String sdkHome, @NotNull ErlangSdkRelease version) {
    getInstance().mySdkHomeToReleaseCache.put(getVersionCacheKey(sdkHome), version); // we'll not try to detect sdk version in tests environment
    Sdk sdk = new ProjectJdkImpl(getDefaultSdkName(sdkHome, version), getInstance());
    SdkModificator sdkModificator = sdk.getSdkModificator();
    sdkModificator.setHomePath(sdkHome);
    sdkModificator.setVersionString(getVersionString(version)); // must be set after home path, otherwise setting home path clears the version string
    sdkModificator.commitChanges();
    configureSdkPaths(sdk);
    return sdk;
  }

  @Nullable
  private ErlangSdkRelease detectSdkVersion(@NotNull String sdkHome) {
    ErlangSdkRelease release = mySdkHomeToReleaseCache.computeIfAbsent(getVersionCacheKey(sdkHome), ErlangSdkType::getRelease);
    if (release == null && ApplicationManager.getApplication().isUnitTestMode()) {
      throw new AssertionError("SDK version detection failed. If you're using a mock SDK, make sure you have your SDK version pre-cached");
    }
    return release;
  }

  @Nullable
  private static ErlangSdkRelease getRelease(@NotNull String sdkHome) {
    ErlangSdkRelease withOTPVERSION = detectReleaseWithOTPVERSION(sdkHome);
    if (withOTPVERSION != null) return withOTPVERSION;
    ErlangSdkRelease withFiles = detectReleaseWithFiles(sdkHome);
    if (withFiles != null) return withFiles;
    return detectReleaseWithProcess(sdkHome);
  }

  private static @Nullable ErlangSdkRelease detectReleaseWithOTPVERSION(String sdkHome) {
    // Enumerate directories and read into OTP_VERSION file
    var otpRelease = readOtpVersionFile(sdkHome);

    // Try read either SDKDIR/erts-x.x or SDKDIR/lib/erts-x.x
    var ertsVersion = Optional
      .ofNullable(readErtsDirectoryVersion(sdkHome))
      .orElse(readErtsDirectoryVersion(sdkHome + "/lib"));
    if (otpRelease != null && ertsVersion != null) {
      return new ErlangSdkRelease(otpRelease, ertsVersion);
    }
    return null;
  }

  private static @Nullable String readErtsDirectoryVersion(String searchIn) {
    var ertsDir = new File(searchIn)
      .listFiles((dir, name) -> {
        var f = new File(dir, name);
        return f.isDirectory() && name.startsWith("erts-");
      });
    if (ertsDir != null && ertsDir.length > 0) {
      return ertsDir[0].getName().substring(5); // Trim leading "erts-" keep the version
    }
    return null;
  }

  private static @Nullable String readOtpVersionFile(String sdkHome) {
    // Enumerate directories and read into OTP_VERSION file
    var releaseFiles = new File(sdkHome + "/releases")
      .listFiles((File dir, String name) -> {
        // Filter directories in SDKDIR/releases/* which contain OTP_VERSION file
        var otpversion = new File(dir.getPath() + "/" + name, "OTP_VERSION");
        return otpversion.exists() && otpversion.isFile();
      });
    if (releaseFiles != null && releaseFiles.length > 0) {
      try {
        // Read the OTP_VERSION file in the first result
        return Files.readString(Path.of(releaseFiles[0].toString() + "/OTP_VERSION")).trim();
      }
      catch (IOException ignored) {
      }
    }
    return null;
  }

  // This does not work on OTP versions after 16+, use detectReleaseWithOTPVERSION instead
  @Nullable
  private static ErlangSdkRelease detectReleaseWithFiles(@NotNull String sdkHome) {
    try {
      File startErl = Arrays.stream(new File[]{
        new File(sdkHome, "releases/start_erl.data"),
        new File(sdkHome, "lib/erlang/releases/start_erl.data"),
        }).filter(File::exists).findFirst().orElse(null);

      if (startErl == null) return null;

      String line = ContainerUtil.getFirstItem(FileUtil.loadLines(startErl));
      List<String> split = StringUtil.split(line, " ");
      if (split.size() == 2) {
        return new ErlangSdkRelease(split.get(1), split.get(0));
      }
    }
    catch (IOException ignore) {
    }
    return null;
  }

  @Nullable
  private static ErlangSdkRelease detectReleaseWithProcess(@NotNull String sdkHome) {
    try {
      File erl = JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkHome);
      if (!erl.canExecute()) {
        String reason = erl.getPath() + (erl.exists() ? " is not executable." : " is missing.");
        LOG.warn("Can't detect Erlang version: " + reason);
        return null;
      }

      ProcessOutput output = ErlangSystemUtil.getProcessOutput(sdkHome, erl.getAbsolutePath(), "-noshell",
                                                               "-eval", PRINT_VERSION_INFO_EXPRESSION);
      ErlangSdkRelease release = output.getExitCode() != 0 || output.isCancelled() || output.isTimeout()
                                 ? null
                                 : parseSdkVersion(output.getStdoutLines());

      if (release == null) {
        LOG.warn("Failed to detect Erlang version.\n" +
                 "StdOut: " + output.getStdout() + "\n" +
                 "StdErr: " + output.getStderr());
      }
      return release;
    }
    catch (ExecutionException e) {
      LOG.warn(e);
      return null;
    }
  }

  @Nullable
  private static ErlangSdkRelease parseSdkVersion(@NotNull List<String> printVersionInfoOutput) {
    String otpRelease = null;
    String ertsVersion = null;

    ListIterator<String> iterator = printVersionInfoOutput.listIterator();
    while (iterator.hasNext()) {
      String line = iterator.next();
      if (OTP_RELEASE_PREFIX_LINE.equals(line) && iterator.hasNext()) {
        otpRelease = iterator.next();
      }
      else if (ERTS_VERSION_PREFIX_LINE.equals(line) && iterator.hasNext()) {
        ertsVersion = iterator.next();
      }
    }

    return otpRelease != null && ertsVersion != null ? new ErlangSdkRelease(otpRelease, ertsVersion) : null;
  }

  private static void configureSdkPaths(@NotNull Sdk sdk) {
    SdkModificator sdkModificator = sdk.getSdkModificator();
    setupLocalSdkPaths(sdkModificator);
    String externalDocUrl = getDefaultDocumentationUrl(getRelease(sdk));
    VirtualFile fileByUrl = externalDocUrl == null ? null : VirtualFileManager.getInstance().findFileByUrl(externalDocUrl);
    if (fileByUrl != null) {
      sdkModificator.addRoot(fileByUrl, JavadocOrderRootType.getInstance());
    }
    sdkModificator.commitChanges();
  }

  @Nullable
  private static String getDefaultDocumentationUrl(@Nullable ErlangSdkRelease version) {
    return version == null ? null : "http://www.erlang.org/documentation/doc-" + version.getErtsVersion();
  }

  private static void setupLocalSdkPaths(@NotNull SdkModificator sdkModificator) {
    String sdkHome = sdkModificator.getHomePath();

    {
      File stdLibDir = new File(new File(sdkHome), "lib");
      if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
    }

    assert !ApplicationManager.getApplication().isUnitTestMode() : "Failed to setup a mock SDK!";

    try {
      String exePath = JpsErlangSdkType.getByteCodeCompilerExecutable(sdkHome).getAbsolutePath();
      ProcessOutput processOutput = ErlangSystemUtil.getProcessOutput(sdkHome, exePath, "-where");
      if (processOutput.getExitCode() == 0) {
        String stdout = processOutput.getStdout().trim();
        if (!stdout.isEmpty()) {
          if (SystemInfo.isWindows && stdout.startsWith("/")) {
            for (File root : File.listRoots()) {
              File stdLibDir = new File(root, stdout);
              if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
            }
          }
          else {
            File stdLibDir = new File(stdout);
            if (tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir)) return;
          }
        }
      }
    }
    catch (ExecutionException ignore) {
    }

    File stdLibDir = new File("/usr/lib/erlang");
    tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir);
  }

  private static boolean tryToProcessAsStandardLibraryDir(@NotNull SdkModificator sdkModificator,
                                                          @NotNull File stdLibDir) {
    if (!isStandardLibraryDir(stdLibDir)) return false;
    VirtualFile dir = LocalFileSystem.getInstance().findFileByIoFile(stdLibDir);
    if (dir != null) {
      sdkModificator.addRoot(dir, OrderRootType.SOURCES);
      sdkModificator.addRoot(dir, OrderRootType.CLASSES);
    }
    return true;
  }

  private static boolean isStandardLibraryDir(@NotNull File dir) {
    return dir.isDirectory();
  }

  @NotNull
  private static String getDefaultSdkName(@NotNull String sdkHome, @Nullable ErlangSdkRelease version) {
    return version != null ? "Erlang " + version.getOtpRelease() : "Unknown Erlang version at " + sdkHome;
  }

  @Nullable
  private static String getVersionString(@Nullable ErlangSdkRelease version) {
    return version != null ? version.toString() : null;
  }

  @Nullable
  private static ErlangSdkRelease getRelease(@Nullable Sdk sdk) {
    if (sdk != null && sdk.getSdkType() == getInstance()) {
      ErlangSdkRelease fromVersionString = ErlangSdkRelease.fromString(sdk.getVersionString());
      return fromVersionString != null ? fromVersionString :
             getInstance().detectSdkVersion(StringUtil.notNullize(sdk.getHomePath()));
    }
    return null;
  }

  @Nullable
  private static ErlangSdkRelease getReleaseForSmallIde(@NotNull Project project) {
    String sdkPath = getSdkPath(project);
    return StringUtil.isEmpty(sdkPath) ? null : getInstance().detectSdkVersion(sdkPath);
  }

  @Nullable
  private static String getVersionCacheKey(@Nullable String sdkHome) {
    return sdkHome != null ? new File(sdkHome).getAbsolutePath() : null;
  }
}
