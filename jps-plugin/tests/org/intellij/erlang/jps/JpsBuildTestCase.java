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
package org.intellij.erlang.jps;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.testFramework.UsefulTestCase;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.jps.api.CanceledStatus;
import org.jetbrains.jps.builders.impl.BuildDataPathsImpl;
import org.jetbrains.jps.builders.impl.BuildRootIndexImpl;
import org.jetbrains.jps.builders.impl.BuildTargetIndexImpl;
import org.jetbrains.jps.builders.impl.BuildTargetRegistryImpl;
import org.jetbrains.jps.builders.logging.BuildLoggingManager;
import org.jetbrains.jps.builders.storage.BuildDataPaths;
import org.jetbrains.jps.cmdline.ClasspathBootstrap;
import org.jetbrains.jps.cmdline.ProjectDescriptor;
import org.jetbrains.jps.incremental.BuilderRegistry;
import org.jetbrains.jps.incremental.IncProjectBuilder;
import org.jetbrains.jps.incremental.RebuildRequestedException;
import org.jetbrains.jps.incremental.fs.BuildFSState;
import org.jetbrains.jps.incremental.relativizer.PathRelativizerService;
import org.jetbrains.jps.incremental.storage.BuildDataManager;
import org.jetbrains.jps.incremental.storage.BuildTargetsState;
import org.jetbrains.jps.incremental.storage.ProjectStamps;
import org.jetbrains.jps.indices.ModuleExcludeIndex;
import org.jetbrains.jps.indices.impl.IgnoredFileIndexImpl;
import org.jetbrains.jps.indices.impl.ModuleExcludeIndexImpl;
import org.jetbrains.jps.model.*;
import org.jetbrains.jps.model.java.*;
import org.jetbrains.jps.model.library.JpsOrderRootType;
import org.jetbrains.jps.model.library.JpsTypedLibrary;
import org.jetbrains.jps.model.library.sdk.JpsSdk;
import org.jetbrains.jps.model.library.sdk.JpsSdkReference;
import org.jetbrains.jps.model.library.sdk.JpsSdkType;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.model.module.JpsModuleType;
import org.jetbrains.jps.model.module.JpsSdkReferencesTable;
import org.jetbrains.jps.util.JpsPathUtil;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public abstract class JpsBuildTestCase extends UsefulTestCase {
  @Nullable
  private File myProjectDir;
  private JpsProject myProject;
  protected JpsModel myModel;
  private JpsSdk<JpsDummyElement> myJdk;
  private File myDataStorageRoot;
  private TestProjectBuilderLogger myLogger;

  private Map<String, String> myBuildParams;

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myModel = JpsElementFactory.getInstance().createModel();
    myProject = myModel.getProject();
    myDataStorageRoot = FileUtil.createTempDirectory("compile-server-" + getProjectName(), null);
    myLogger = new TestProjectBuilderLogger();
    myBuildParams = new HashMap<>();
  }

  @Override
  protected void tearDown() throws Exception {
    myProjectDir = null;
    super.tearDown();
  }

  private JpsSdk<JpsDummyElement> addJdk(String name) {
    try {
      return addJdk(name, FileUtil.toSystemIndependentName(ClasspathBootstrap.getResourceFile(Object.class).getCanonicalPath()));
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  protected JpsSdk<JpsDummyElement> addJdk(String name, String path) {
    String homePath = System.getProperty("java.home");
    String versionString = System.getProperty("java.version");
    JpsTypedLibrary<JpsSdk<JpsDummyElement>> jdk = myModel.getGlobal().addSdk(name, homePath, versionString, JpsJavaSdkType.INSTANCE);
    jdk.addRoot(JpsPathUtil.pathToUrl(path), JpsOrderRootType.COMPILED);
    return jdk.getProperties();
  }

  private String getProjectName() {
    return StringUtil.decapitalize(StringUtil.trimStart(getName(), "test"));
  }

  private ProjectDescriptor createProjectDescriptor(BuildLoggingManager buildLoggingManager) {
    try {
      BuildTargetRegistryImpl targetRegistry = new BuildTargetRegistryImpl(myModel);
      ModuleExcludeIndex index = new ModuleExcludeIndexImpl(myModel);
      IgnoredFileIndexImpl ignoredFileIndex = new IgnoredFileIndexImpl(myModel);
      BuildDataPaths dataPaths = new BuildDataPathsImpl(myDataStorageRoot);
      BuildRootIndexImpl buildRootIndex = new BuildRootIndexImpl(targetRegistry, myModel, index, dataPaths, ignoredFileIndex);
      BuildTargetIndexImpl targetIndex = new BuildTargetIndexImpl(targetRegistry, buildRootIndex);
      BuildTargetsState targetsState = new BuildTargetsState(dataPaths, myModel, buildRootIndex);
      PathRelativizerService relativizer = new PathRelativizerService();
      ProjectStamps timestamps = new ProjectStamps(myDataStorageRoot, targetsState, relativizer);
      BuildDataManager dataManager = new BuildDataManager(dataPaths, targetsState, relativizer);
      return new ProjectDescriptor(myModel,
                                   new BuildFSState(true),
                                   timestamps,
                                   dataManager,
                                   buildLoggingManager,
                                   index,
                                   targetIndex,
                                   buildRootIndex,
                                   ignoredFileIndex);
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  protected <T extends JpsElement> void addModule(String moduleName,
                                                  String[] srcPaths,
                                                  @Nullable String outputPath,
                                                  @Nullable String testOutputPath,
                                                  JpsSdk<T> sdk) {
    addModule(moduleName, srcPaths, outputPath, testOutputPath, sdk, JpsJavaModuleType.INSTANCE);
  }

  protected <T extends JpsElement, M extends JpsModuleType & JpsElementTypeWithDefaultProperties> JpsModule addModule(String moduleName,
                                                       String[] srcPaths,
                                                       @Nullable String outputPath,
                                                       @Nullable String testOutputPath,
                                                       JpsSdk<T> sdk,
                                                       M moduleType) {
    JpsModule module = myProject.addModule(moduleName, moduleType);
    JpsSdkType<T> sdkType = sdk.getSdkType();
    JpsSdkReferencesTable sdkTable = module.getSdkReferencesTable();
    sdkTable.setSdkReference(sdkType, sdk.createReference());

    if (sdkType instanceof JpsJavaSdkTypeWrapper) {
      JpsSdkReference<T> wrapperRef = sdk.createReference();
      sdkTable.setSdkReference(JpsJavaSdkType.INSTANCE, JpsJavaExtensionService.
        getInstance().createWrappedJavaSdkReference((JpsJavaSdkTypeWrapper)sdkType, wrapperRef));
    }
    module.getDependenciesList().addSdkDependency(sdkType);
    if (srcPaths.length > 0 || outputPath != null) {
      for (String srcPath : srcPaths) {
        module.getContentRootsList().addUrl(JpsPathUtil.pathToUrl(srcPath));
        module.addSourceRoot(JpsPathUtil.pathToUrl(srcPath), JavaSourceRootType.SOURCE);
      }
      JpsJavaModuleExtension extension = JpsJavaExtensionService.getInstance().getOrCreateModuleExtension(module);
      if (outputPath != null) {
        extension.setOutputUrl(JpsPathUtil.pathToUrl(outputPath));
        if (!StringUtil.isEmpty(testOutputPath)) {
          extension.setTestOutputUrl(JpsPathUtil.pathToUrl(testOutputPath));
        }
        else {
          extension.setTestOutputUrl(extension.getOutputUrl());
        }
      }
      else {
        extension.setInheritOutput(true);
      }
    }
    return module;
  }

  protected void rebuildAll() {
    doBuild(CompileScopeTestBuilder.rebuild().all()).assertSuccessful();
  }

  private BuildResult doBuild(CompileScopeTestBuilder scope) {
    ProjectDescriptor descriptor = createProjectDescriptor(new BuildLoggingManager(myLogger));
    try {
      myLogger.clear();
      return doBuild(descriptor, scope);
    }
    finally {
      descriptor.release();
    }
  }

  private BuildResult doBuild(ProjectDescriptor descriptor, CompileScopeTestBuilder scopeBuilder) {
    IncProjectBuilder builder = new IncProjectBuilder(descriptor, BuilderRegistry.getInstance(), myBuildParams, CanceledStatus.NULL, true);
    BuildResult result = new BuildResult();
    builder.addMessageHandler(result);
    try {
      builder.build(scopeBuilder.build(), false);
    }
    catch (RebuildRequestedException e) {
      throw new RuntimeException(e);
    }
    return result;
  }

  protected String createFile(String relativePath, String text) {
    try {
      File file = new File(getOrCreateProjectDir(), relativePath);
      FileUtil.writeToFile(file, text);
      return FileUtil.toSystemIndependentName(file.getAbsolutePath());
    }
    catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private File getOrCreateProjectDir() {
    if (myProjectDir == null) {
      try {
        myProjectDir = doGetProjectDir();
      }
      catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
    return myProjectDir;
  }

  private static File doGetProjectDir() throws IOException {
    return FileUtil.createTempDirectory("prj", null);
  }

  protected String getAbsolutePath(String pathRelativeToProjectRoot) {
    return FileUtil.toSystemIndependentName(new File(getOrCreateProjectDir(), pathRelativeToProjectRoot).getAbsolutePath());
  }

  protected void addModule(String moduleName, String... srcPaths) {
    if (myJdk == null) {
      myJdk = addJdk("1.6");
    }
    addModule(moduleName, srcPaths, getAbsolutePath("out/production/" + moduleName), null, myJdk);
  }
}
