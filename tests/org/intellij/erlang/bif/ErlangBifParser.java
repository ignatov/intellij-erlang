/*
 * Copyright 2012-2024 Sergey Ignatov
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

package org.intellij.erlang.bif;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.documentation.ErlangDocumentationProvider;
import org.intellij.erlang.psi.ErlangFunctionCallExpression;
import org.intellij.erlang.psi.ErlangGlobalFunctionCallExpression;
import org.intellij.erlang.psi.impl.ErlangElementFactory;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.junit.Ignore;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings({"JUnitTestClassNamingConvention"})
@Ignore
public class ErlangBifParser extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final Pattern PATTERN_FUNC_DECLARATION = Pattern.compile(
    "<span\\s+class=\"bold_code\">.*\\((.*)\\) -&gt;.*</span>", Pattern.MULTILINE);
  private static final String BIF_TABLE_PATH = "src/org/intellij/erlang/bif/bif.tab.txt";
  private static final String BIF_AUTOIMPORT_TABLE_PATH = "src/org/intellij/erlang/bif/bif.autoimport.tab.txt";
  private static final String GENERATED_FILE = "src/org/intellij/erlang/bif/ErlangBifTable.java";
  private static final String ERLANG_SDK_PATH = "/usr/lib/erlang";
  private static final String ERLANG_DOC_PATH = "/home/ignatov/Downloads/otp_doc_html_R16B";
  private static final Pattern BIF_DECLARATION = Pattern.compile("bif (\\w+):(\\w+)/(\\d+)");
  private static final Pattern BIF_SEPARATOR = Pattern.compile("# New Bifs in (R.+)");

  private ErlangDocumentationProvider myDocProvider;

  private static Set<String> getAutoimportedFunctions() throws IOException {
    File bifAutoimportTableFile = new File(BIF_AUTOIMPORT_TABLE_PATH);
    String[] bifAutoimportTableText = StringUtil.splitByLines(FileUtilRt.loadFile(bifAutoimportTableFile));
    return new HashSet<>(List.of(bifAutoimportTableText));
  }

  public void testSomething() throws Exception {
    File bifTableFile = new File(BIF_TABLE_PATH);
    String[] bifTableText = StringUtil.splitByLines(FileUtilRt.loadFile(bifTableFile));

    try (PrintStream bifTableJavaBuilder = new PrintStream(new File(GENERATED_FILE))) {
      Set<String> autoimported = getAutoimportedFunctions();
      bifTableJavaBuilder.append("""
                                   package org.intellij.erlang.bif;

                                   import org.jetbrains.annotations.NotNull;
                                   import org.jetbrains.annotations.Nullable;
                                   import com.intellij.util.containers.MultiMap;

                                   import java.util.ArrayList;
                                   import java.util.Collection;
                                   import java.util.TreeSet;
                                   import java.util.List;

                                   public final class ErlangBifTable {
                                     private static final MultiMap<String, ErlangBifDescriptor> bifMap = new MultiMap<String, ErlangBifDescriptor>() {
                                       @Override
                                       protected Collection<ErlangBifDescriptor> createCollection() {
                                         return new TreeSet<ErlangBifDescriptor>();
                                       }
                                     };

                                     static {
                                   """);
      for (String s : bifTableText) {
        Matcher matcher;
        if ((matcher = BIF_DECLARATION.matcher(s)).find()) {
          String module = matcher.group(1);
          String name = matcher.group(2);
          String arity = matcher.group(3);
          String isAuroimport = autoimported.contains(name + "/" + arity) ? ", true" : "";
          bifTableJavaBuilder.append("    bifMap.putValue(\"").append(module)
                             .append("\", new ErlangBifDescriptor(\"")
                             .append(module).append("\", \"").append(name).append("\", ").append(arity)
                             .append(", \"").append(fetchSpec(module, name, Integer.valueOf(arity))).append("\"").append(isAuroimport).append("));\n");
        }
        else if ((matcher = BIF_SEPARATOR.matcher(s)).find()) {
          String version = matcher.group(1).replaceAll("\\.", "");
          bifTableJavaBuilder.append("    // Since ").append(version).append("\n");
        }
      }
      bifTableJavaBuilder.append("""
                                     }

                                     private ErlangBifTable() {
                                     }

                                     @NotNull
                                     public static Collection<ErlangBifDescriptor> getBifs(@NotNull String moduleName) {
                                       return bifMap.get(moduleName);
                                     }

                                     @NotNull
                                     public static Collection<ErlangBifDescriptor> getBifs(@NotNull String moduleName,
                                                                                           @NotNull String functionName) {
                                       final List<ErlangBifDescriptor> bifDescriptors = new ArrayList<ErlangBifDescriptor>();
                                       for (ErlangBifDescriptor bifDescriptor : bifMap.get(moduleName)) {
                                         if (functionName.equals(bifDescriptor.getName())) {
                                           bifDescriptors.add(bifDescriptor);
                                         }
                                       }
                                       return bifDescriptors;
                                     }


                                     public static boolean isBif(@NotNull String moduleName, @NotNull String functionName, int arity) {
                                       final Collection<ErlangBifDescriptor> erlangBifDescriptors = bifMap.get(moduleName);
                                       for (ErlangBifDescriptor bifDescriptor : erlangBifDescriptors) {
                                         if (bifDescriptor.getModule().equals(moduleName) && bifDescriptor.getName().equals(functionName) &&
                                           bifDescriptor.getArity() == arity) {
                                           return true;
                                         }
                                       }
                                       return false;
                                     }
                                   }
                                   """);
    }
  }

  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        Sdk mockSdk = ErlangSdkType.createMockSdk(ERLANG_SDK_PATH, ErlangSdkRelease.V_R16B);
        // Set local SDK documentation path
        SdkModificator sdkModificator = mockSdk.getSdkModificator();
        VirtualFile localDocDir = LocalFileSystem.getInstance().findFileByPath(ERLANG_DOC_PATH);
        sdkModificator.addRoot(localDocDir, JavadocOrderRootType.getInstance());
        sdkModificator.commitChanges();
        return mockSdk;
      }
    };
  }

  @Override
  protected void setUp() throws Exception {
    System.setProperty("idea.platform.prefix", "Idea");
    super.setUp();
    myDocProvider = new ErlangDocumentationProvider();
    setUpProjectSdk();
  }

  private String fetchSpec(@NotNull String moduleName, @NotNull String functionName, int arity) {
    String paramList = paramList(arity);
    String functionCallText = moduleName + ":" + functionName + "(" + paramList + ")";
    ErlangFunctionCallExpression erlFunctionCall = ((ErlangGlobalFunctionCallExpression) ErlangElementFactory.createExpressionFromText(
      getProject(), functionCallText)).getFunctionCallExpression();
    String httpDoc = myDocProvider.generateDoc(erlFunctionCall, null);
    if (httpDoc != null) {
      Matcher matcher = PATTERN_FUNC_DECLARATION.matcher(httpDoc);
      if (matcher.find()) {
        return matcher.group(1);
      }
    }
    return paramList;
  }

  @NotNull
  private static String paramList(int arity) {
    if (arity > 0) {
      StringBuilder argumentListBuilder = new StringBuilder();
      argumentListBuilder.append("P1");
      for (int i = 2; i <= arity; ++i) {
        argumentListBuilder.append(", P").append(i);
      }
      return argumentListBuilder.toString();
    }
    else {
      return "";
    }
  }
}
