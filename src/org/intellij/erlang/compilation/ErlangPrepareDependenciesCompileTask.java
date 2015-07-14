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

package org.intellij.erlang.compilation;

import com.intellij.compiler.server.BuildManager;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileTask;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.JDOMUtil;
import com.intellij.util.SystemProperties;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.xmlb.SkipDefaultValuesSerializationFilters;
import com.intellij.util.xmlb.XmlSerializationException;
import com.intellij.util.xmlb.XmlSerializer;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.facet.ErlangFacet;
import org.intellij.erlang.jps.builder.ErlangBuilder;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrderDescriptor;
import org.intellij.erlang.jps.builder.ErlangModuleBuildOrders;
import org.intellij.erlang.jps.builder.ErlangModuleDescriptor;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.utils.ErlangModulesUtil;
import org.jdom.Document;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.TestOnly;

import java.io.File;
import java.io.IOException;
import java.util.*;


public class ErlangPrepareDependenciesCompileTask implements CompileTask {
  @Override
  public boolean execute(final CompileContext context) {
    Project project = context.getProject();
    if (ErlangCompilerSettings.getInstance(project).isUseRebarCompilerEnabled()) {
      // delegate dependencies resolution to rebar
      return true;
    }

    File projectSystemDirectory = BuildManager.getInstance().getProjectSystemDirectory(project);
    if (projectSystemDirectory == null) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }

    ErlangModuleBuildOrders buildOrders = ApplicationManager.getApplication().runReadAction(new Computable<ErlangModuleBuildOrders>() {
      @Nullable
      @Override
      public ErlangModuleBuildOrders compute() {
        return getModuleBuildOrders(context);
      }
    });
    if (buildOrders == null) {
      return false; // errors are reported to context.
    }
    try {
      Document serializedBuildOrders = new Document(XmlSerializer.serialize(buildOrders, new SkipDefaultValuesSerializationFilters()));
      File file = new File(projectSystemDirectory, ErlangBuilder.DEPENDENCIES_CONFIG_FILE_PATH);
      //noinspection ResultOfMethodCallIgnored
      file.getParentFile().mkdirs();
      JDOMUtil.writeDocument(serializedBuildOrders, file, SystemProperties.getLineSeparator());
    } catch (XmlSerializationException e) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    } catch (IOException e) {
      addPrepareDependenciesFailedMessage(context);
      return true;
    }

    return true;
  }

  public static void addPrepareDependenciesFailedMessage(CompileContext context) {
    context.addMessage(CompilerMessageCategory.WARNING, "Failed to submit dependencies info to compiler. Parse transform failures may occur.", null, -1, -1);
  }

  @Nullable
  private static ErlangModuleBuildOrders getModuleBuildOrders(CompileContext context) {
    Module[] modulesToCompile = context.getCompileScope().getAffectedModules();
    ErlangModuleBuildOrders buildOrders = new ErlangModuleBuildOrders(modulesToCompile.length);
    try {
      for (Module module : modulesToCompile) {
        buildOrders.myModuleBuildOrderDescriptors.add(getModuleBuildOrderInner(module));
      }
    } catch (CyclicDependencyFoundException e) {
      context.addMessage(CompilerMessageCategory.ERROR, "Cyclic erlang module dependency detected. Check parse_transform usages.", null, -1, -1);
      return null;
    }
    return buildOrders;
  }

  // It's public for test purposes only
  @TestOnly
  public static ErlangModuleBuildOrderDescriptor getModuleBuildOrder(Module module) throws CyclicDependencyFoundException {
    return getModuleBuildOrderInner(module);
  }

  private static ErlangModuleBuildOrderDescriptor getModuleBuildOrderInner(Module module) throws CyclicDependencyFoundException {
    ErlangModuleBuildOrderDescriptor buildOrder = new ErlangModuleBuildOrderDescriptor();
    ErlangFacet erlangFacet = ErlangFacet.getFacet(module);
    List<String> globalParseTransforms = erlangFacet != null ? erlangFacet.getConfiguration().getParseTransforms() : ContainerUtil.<String>emptyList();
    buildOrder.myModuleName = module.getName();
    buildOrder.myOrderedErlangModulePaths = getTopologicallySortedErlangModulePaths(ErlangModulesUtil.getErlangModules(module, false), globalParseTransforms);
    buildOrder.myOrderedErlangTestModulePaths = getTopologicallySortedErlangModulePaths(ErlangModulesUtil.getErlangModules(module, true), ContainerUtil.<String>emptyList());
    return buildOrder;
  }

  private static List<ErlangModuleDescriptor> getTopologicallySortedErlangModulePaths(Collection<ErlangFile> erlangModules,
                                                                                      List<String> globalParseTransforms) throws CyclicDependencyFoundException {
    ErlangModulesSorter sorter =  new ErlangModulesSorter(erlangModules,globalParseTransforms);
    return sorter.getSortedModules();
  }

}