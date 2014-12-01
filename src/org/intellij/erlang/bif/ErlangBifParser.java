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

import java.io.File;
import java.io.PrintStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@SuppressWarnings({"JUnitTestClassNamingConvention"})
public class ErlangBifParser extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final Pattern PATTERN_FUNC_DECLARATION = Pattern.compile(
    "<span\\s+class=\"bold_code\">.*\\((.*)\\) -&gt;.*</span>", Pattern.MULTILINE);
  private static final String BIF_TABLE_PATH = "src/org/intellij/erlang/bif/bif.tab.txt";
  private static final String GENERATED_FILE = "src/org/intellij/erlang/bif/ErlangBifTable.java";
  private static final String ERLANG_SDK_PATH = "/usr/lib/erlang";
  private static final String ERLANG_DOC_PATH = "/home/ignatov/Downloads/otp_doc_html_R16B";
  private static final Pattern BIF_DECLARATION = Pattern.compile("bif (\\w+)\\:(\\w+)/(\\d+)");
  private static final Pattern BIF_SEPARATOR = Pattern.compile("# New Bifs in (R.+)");

  private ErlangDocumentationProvider myDocProvider;

  public void testSomething() throws Exception {
    File bifTableFile = new File(BIF_TABLE_PATH);
    String[] bifTableText = StringUtil.splitByLines(FileUtilRt.loadFile(bifTableFile));
    PrintStream bifTableJavaBuilder = new PrintStream(new File(GENERATED_FILE));
    try {
      bifTableJavaBuilder.append("package org.intellij.erlang.bif;\n" +
        "\n" +
        "import org.jetbrains.annotations.NotNull;\n" +
        "import org.jetbrains.annotations.Nullable;\n" +
        "import com.intellij.util.containers.MultiMap;\n" +
        "\n" +
        "import java.util.ArrayList;\n" +
        "import java.util.Collection;\n" +
        "import java.util.TreeSet;\n" +
        "import java.util.List;\n" +
        "\n" +
        "public final class ErlangBifTable {\n" +
        "  private static final MultiMap<String, ErlangBifDescriptor> bifMap = new MultiMap<String, ErlangBifDescriptor>() {\n" +
        "    @Override\n" +
        "    protected Collection<ErlangBifDescriptor> createCollection() {\n" +
        "      return new TreeSet<ErlangBifDescriptor>();\n" +
        "    }\n" +
        "  };\n" +
        "\n" +
        "  static {\n");
      for (String s : bifTableText) {
        Matcher matcher;
        if ((matcher = BIF_DECLARATION.matcher(s)).find()) {
          String module = matcher.group(1);
          String name = matcher.group(2);
          String arity = matcher.group(3);
          bifTableJavaBuilder.append("    bifMap.putValue(\"").append(module)
            .append("\", new ErlangBifDescriptor(\"")
            .append(module).append("\", \"").append(name).append("\", ").append(arity)
            .append(", \"").append(fetchSpec(module, name, Integer.valueOf(arity))).append("\"));\n");
        }
        else if ((matcher = BIF_SEPARATOR.matcher(s)).find()) {
          String version = matcher.group(1).replaceAll("\\.", "");
          bifTableJavaBuilder.append("    // Since ").append(version).append("\n");
        }
      }
      bifTableJavaBuilder.append("  }\n" +
        "\n" +
        "  private ErlangBifTable() {\n" +
        "  }\n" +
        "\n" +
        "  @NotNull\n" +
        "  public static Collection<ErlangBifDescriptor> getBifs(@NotNull String moduleName) {\n" +
        "    return bifMap.get(moduleName);\n" +
        "  }\n" +
        "\n" +
        "  @NotNull\n" +
        "  public static Collection<ErlangBifDescriptor> getBifs(@NotNull String moduleName,\n" +
        "                                                        @NotNull String functionName) {\n" +
        "    final List<ErlangBifDescriptor> bifDescriptors = new ArrayList<ErlangBifDescriptor>();\n" +
        "    for (ErlangBifDescriptor bifDescriptor : bifMap.get(moduleName)) {\n" +
        "      if (functionName.equals(bifDescriptor.getName())) {\n" +
        "        bifDescriptors.add(bifDescriptor);\n" +
        "      }\n" +
        "    }\n" +
        "    return bifDescriptors;\n" +
        "  }\n" +
        "\n" +
        "\n" +
        "  public static boolean isBif(@NotNull String moduleName, @NotNull String functionName, int arity) {\n" +
        "    final Collection<ErlangBifDescriptor> erlangBifDescriptors = bifMap.get(moduleName);\n" +
        "    for (ErlangBifDescriptor bifDescriptor : erlangBifDescriptors) {\n" +
        "      if (bifDescriptor.getModule().equals(moduleName) && bifDescriptor.getName().equals(functionName) &&\n" +
        "        bifDescriptor.getArity() == arity) {\n" +
        "        return true;\n" +
        "      }\n" +
        "    }\n" +
        "    return false;\n" +
        "  }\n" +
        "}\n");
    } finally {
      bifTableJavaBuilder.close();
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

  @Override
  protected boolean isWriteActionRequired() {
    return false;
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
