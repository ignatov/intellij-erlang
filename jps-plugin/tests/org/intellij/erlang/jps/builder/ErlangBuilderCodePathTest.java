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

package org.intellij.erlang.jps.builder;

import com.intellij.openapi.util.io.FileUtil;
import com.intellij.testFramework.UsefulTestCase;
import org.intellij.erlang.jps.model.JpsErlangModuleType;
import org.jetbrains.jps.incremental.CompileContext;
import org.jetbrains.jps.model.JpsElementFactory;
import org.jetbrains.jps.model.java.JpsJavaExtensionService;
import org.jetbrains.jps.model.module.JpsModule;
import org.jetbrains.jps.util.JpsPathUtil;

import java.io.File;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;

public class ErlangBuilderCodePathTest extends UsefulTestCase {
  public void testWindowsContentRootKeepsDriveLetter() throws Exception {
    assertEquals("C:/proj", getContentRootCodePath("file://C:/proj"));
  }

  public void testUnixContentRoot() throws Exception {
    assertEquals("/home/u/proj", getContentRootCodePath("file:///home/u/proj"));
  }

  private static String getContentRootCodePath(String contentRootUrl) throws Exception {
    File output = FileUtil.createTempDirectory("erlang-code-path", null);
    JpsModule module = JpsElementFactory.getInstance().createModel().getProject().addModule("m", JpsErlangModuleType.INSTANCE);
    JpsJavaExtensionService.getInstance().getOrCreateModuleExtension(module).setOutputUrl(JpsPathUtil.pathToUrl(output.getPath()));
    module.getContentRootsList().addUrl(contentRootUrl);
    CompileContext context = (CompileContext) Proxy.newProxyInstance(
      CompileContext.class.getClassLoader(), new Class<?>[]{CompileContext.class},
      (proxy, method, args) -> { throw new AssertionError("Unexpected call: " + method); });

    Method addModuleToCodePath = ErlangBuilder.class.getDeclaredMethod(
      "addModuleToCodePath", List.class, JpsModule.class, boolean.class, CompileContext.class);
    addModuleToCodePath.setAccessible(true);
    List<String> command = new ArrayList<>();
    addModuleToCodePath.invoke(null, command, module, false, context);

    assertEquals(List.of("-pa", output.getPath(), "-pa"), command.subList(0, 3));
    assertEquals(4, command.size());
    return command.get(3);
  }
}
