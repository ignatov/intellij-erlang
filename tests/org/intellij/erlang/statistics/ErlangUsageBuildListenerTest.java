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

package org.intellij.erlang.statistics;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.intellij.openapi.project.Project;
import com.intellij.task.ProjectTaskManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.configuration.ErlangCompilerSettings;
import org.intellij.erlang.module.ErlangModuleType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.Proxy;

// the build listener counts only in projects with an Erlang module, which the light project of the other tests has not
public class ErlangUsageBuildListenerTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public @NotNull String getModuleTypeId() {
        return ErlangModuleType.getInstance().getId();
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    ErlangUsageStatistics.getInstance().resetForTests();
    ErlangUsageStatistics.setAllowedForTests(true);
    ErlangUsageStatistics.setErlangProjectForTests(getProject(), true);
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      ErlangCompilerSettings.getInstance(getProject()).setUseRebarCompilerEnabled(false);
      ErlangUsageStatistics.getInstance().resetForTests();
      ErlangUsageStatistics.setAllowedForTests(null);
      ErlangUsageStatistics.setErlangProjectForTests(getProject(), null);
    }
    catch (Throwable e) {
      addSuppressedException(e);
    }
    finally {
      super.tearDown();
    }
  }

  public void testBuiltInBuildIsCounted() {
    collector().setEnabled(true);
    new ErlangUsageListeners.Build(getProject()).finished(buildResult(false));
    JsonObject properties = previewProperties();
    assertEquals(1, properties.get(ErlangUsageKey.BUILD_BUILT_IN.jsonName()).getAsInt());
    assertFalse(properties.has(ErlangUsageKey.BUILD_REBAR.jsonName()));
  }

  public void testRebarBuildIsCounted() {
    collector().setEnabled(true);
    ErlangCompilerSettings.getInstance(getProject()).setUseRebarCompilerEnabled(true);
    new ErlangUsageListeners.Build(getProject()).finished(buildResult(false));
    JsonObject properties = previewProperties();
    assertEquals(1, properties.get(ErlangUsageKey.BUILD_REBAR.jsonName()).getAsInt());
    assertFalse(properties.has(ErlangUsageKey.BUILD_BUILT_IN.jsonName()));
  }

  public void testAbortedBuildIsNotCounted() {
    collector().setEnabled(true);
    new ErlangUsageListeners.Build(getProject()).finished(buildResult(true));
    assertFalse(previewProperties().has(ErlangUsageKey.BUILD_BUILT_IN.jsonName()));
  }

  public void testBuildIsNotCountedWhileOff() {
    new ErlangUsageListeners.Build(getProject()).finished(buildResult(false));
    collector().setEnabled(true);
    assertFalse(previewProperties().has(ErlangUsageKey.BUILD_BUILT_IN.jsonName()));
  }

  // a closed project has no modules to ask for; asking would fail
  public void testBuildOfAClosedProjectIsNotCounted() {
    collector().setEnabled(true);
    new ErlangUsageListeners.Build(closedProject()).finished(buildResult(false));
    assertFalse(previewProperties().has(ErlangUsageKey.BUILD_BUILT_IN.jsonName()));
  }

  @NotNull
  private static Project closedProject() {
    return (Project)Proxy.newProxyInstance(Project.class.getClassLoader(), new Class<?>[]{Project.class}, (proxy, method, args) ->
      switch (method.getName()) {
        case "isDisposed" -> true;
        case "hashCode" -> System.identityHashCode(proxy);
        case "equals" -> proxy == args[0];
        case "toString" -> "closed project";
        default -> method.getReturnType() == boolean.class ? false : null;
      });
  }

  @NotNull
  private static ProjectTaskManager.Result buildResult(boolean aborted) {
    return (ProjectTaskManager.Result)Proxy.newProxyInstance(
      ProjectTaskManager.Result.class.getClassLoader(), new Class<?>[]{ProjectTaskManager.Result.class}, (proxy, method, args) ->
        switch (method.getName()) {
          case "isAborted" -> aborted;
          case "hashCode" -> System.identityHashCode(proxy);
          case "equals" -> proxy == args[0];
          case "toString" -> "build result";
          default -> method.getReturnType() == boolean.class ? false : null;
        });
  }

  @NotNull
  private static JsonObject previewProperties() {
    String json = collector().previewToday();
    return JsonParser.parseString(json).getAsJsonObject().getAsJsonArray("events").get(0).getAsJsonObject()
      .getAsJsonObject("event_properties");
  }

  @NotNull
  private static ErlangUsageCollector collector() {
    return ErlangUsageStatistics.getInstance().getCollector();
  }
}
