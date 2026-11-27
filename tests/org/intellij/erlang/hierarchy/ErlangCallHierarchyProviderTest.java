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

package org.intellij.erlang.hierarchy;

import com.intellij.ide.hierarchy.CallHierarchyBrowserBase;
import com.intellij.ide.hierarchy.HierarchyBrowser;
import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManagerCore;
import com.intellij.openapi.actionSystem.CommonDataKeys;
import com.intellij.openapi.actionSystem.impl.SimpleDataContext;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.extensions.PluginId;
import com.intellij.openapi.util.Disposer;
import com.intellij.psi.PsiElement;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The call hierarchy provider is registered for every IDE, and the dependency of the plugin on Java is optional,
 * so the classes it runs must not come from the Java plugin: in PyCharm, WebStorm and the like they do not exist.
 */
public class ErlangCallHierarchyProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final Pattern CLASS_NAME = Pattern.compile("(?:com|org)/[A-Za-z0-9_/$]+");

  public void testUsesNoClassOfTheJavaPlugin() throws IOException {
    IdeaPluginDescriptor java = PluginManagerCore.getPlugin(PluginId.getId("com.intellij.java"));
    assertNotNull("the Java plugin is expected on the test classpath", java);
    Path javaPluginPath = java.getPluginPath();

    Set<String> fromJavaPlugin = new TreeSet<>();
    for (String className : new String[]{"ErlangCallHierarchyProvider", "ErlangCallHierarchyProvider$MyCallHierarchyBrowserBase"}) {
      for (String referenced : referencedClasses(className + ".class")) {
        String root = PathManager.getResourceRoot(ErlangCallHierarchyProvider.class.getClassLoader(), referenced + ".class");
        if (root != null && Path.of(root).startsWith(javaPluginPath)) fromJavaPlugin.add(referenced);
      }
    }
    assertEmpty(fromJavaPlugin);
  }

  public void testBrowserIsCreatedAndActivated() {
    myFixture.configureByText("calls.erl", """
      -module(calls).
      a() -> b<caret>().
      b() -> ok.
      """);
    ErlangCallHierarchyProvider provider = new ErlangCallHierarchyProvider();
    PsiElement target = provider.getTarget(SimpleDataContext.getSimpleContext(CommonDataKeys.PSI_ELEMENT, myFixture.getElementAtCaret()));
    assertInstanceOf(target, ErlangFunction.class);
    assertEquals("b", ((ErlangFunction)target).getName());
    HierarchyBrowser browser = provider.createHierarchyBrowser(target);
    try {
      provider.browserActivated(browser);
    }
    finally {
      Disposer.dispose((CallHierarchyBrowserBase)browser);
    }
  }

  private static Set<String> referencedClasses(String classFile) throws IOException {
    try (InputStream in = ErlangCallHierarchyProvider.class.getResourceAsStream(classFile)) {
      assertNotNull(classFile, in);
      String text = new String(in.readAllBytes(), StandardCharsets.ISO_8859_1);
      Set<String> names = new TreeSet<>();
      Matcher matcher = CLASS_NAME.matcher(text);
      while (matcher.find()) names.add(matcher.group());
      return names;
    }
  }
}
