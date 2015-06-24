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
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.containers.WeakHashMap;
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ErlangSdkType extends SdkType {
  private static final Logger LOG = Logger.getInstance(ErlangSdkType.class);

  private final Map<String, ErlangSdkRelease> mySdkHomeToReleaseCache = ApplicationManager.getApplication().isUnitTestMode() ?
    new HashMap<String, ErlangSdkRelease>() : new WeakHashMap<String, ErlangSdkRelease>();

  @NotNull
  public static ErlangSdkType getInstance() {
    ErlangSdkType instance = SdkType.findInstance(ErlangSdkType.class);
    assert instance != null : "Make sure ErlangSdkType is registered in plugin.xml";
    return instance;
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
      for (String version : new String[]{"", "-r14", "-r15", "-r16"}) {
        File brewRoot = new File("/usr/local/Cellar/erlang" + version);
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
      }
      return null;
    }
    else if (SystemInfo.isLinux) {
      return "/usr/lib/erlang";
    }
    return null;
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
  public AdditionalDataConfigurable createAdditionalDataConfigurable(@NotNull SdkModel sdkModel, @NotNull SdkModificator sdkModificator) {
    return null;
  }

  @Override
  public void saveAdditionalData(@NotNull SdkAdditionalData additionalData, @NotNull Element additional) {
  }

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
    ErlangSdkRelease cachedRelease = mySdkHomeToReleaseCache.get(getVersionCacheKey(sdkHome));
    if (cachedRelease != null) {
      return cachedRelease;
    }

    assert !ApplicationManager.getApplication().isUnitTestMode() : "Unit tests should have their SDK versions pre-cached!";

    File erl = JpsErlangSdkType.getByteCodeInterpreterExecutable(sdkHome);
    if (!erl.canExecute()) {
      String reason = erl.getPath() + (erl.exists() ? " is not executable." : " is missing.");
      LOG.warn("Can't detect Erlang version: " + reason);
      return null;
    }

    try {
      ProcessOutput output = ErlangSystemUtil.getProcessOutput(sdkHome, erl.getAbsolutePath(), "-noshell",
        "-eval", "io:format(\"~s~n~s\",[erlang:system_info(otp_release),erlang:system_info(version)]),erlang:halt().");
      List<String> lines = output.getExitCode() != 0 || output.isTimeout() || output.isCancelled() ?
        ContainerUtil.<String>emptyList() : output.getStdoutLines();
      if (lines.size() == 2) {
        ErlangSdkRelease release = new ErlangSdkRelease(lines.get(0), lines.get(1));
        mySdkHomeToReleaseCache.put(getVersionCacheKey(sdkHome), release);
        return release;
      }
      else {
        LOG.warn("Failed to detect Erlang version.\n" + output.getStderr());
      }
    } catch (ExecutionException e) {
      LOG.warn(e);
    }

    return null;
  }

  private static void configureSdkPaths(@NotNull Sdk sdk) {
    SdkModificator sdkModificator = sdk.getSdkModificator();
    setupLocalSdkPaths(sdkModificator);
    String externalDocUrl = getDefaultDocumentationUrl(getRelease(sdk));
    if (externalDocUrl != null) {
      VirtualFile fileByUrl = VirtualFileManager.getInstance().findFileByUrl(externalDocUrl);
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
    } catch (ExecutionException ignore) {
    }

    File stdLibDir = new File("/usr/lib/erlang");
    tryToProcessAsStandardLibraryDir(sdkModificator, stdLibDir);
  }

  private static boolean tryToProcessAsStandardLibraryDir(@NotNull SdkModificator sdkModificator, @NotNull File stdLibDir) {
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
    return  version != null ? "Erlang " + version.getOtpRelease() : "Unknown Erlang version at " + sdkHome;
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
