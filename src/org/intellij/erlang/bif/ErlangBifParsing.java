/*
 * Copyright 2012 Sergey Ignatov
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

import com.intellij.openapi.util.io.FileUtilRt;
import com.intellij.openapi.util.text.StringUtil;

import java.io.File;
import java.io.PrintStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author ignatov
 */
public class ErlangBifParsing {
  private ErlangBifParsing() {
  }

  public static void main(String[] args) throws Exception {
    File file = new File("src/org/intellij/erlang/bif/bif.tab.txt");
    Pattern bif = Pattern.compile("bif (\\w+)\\:(\\w+)/(\\d+)");
    Pattern newBifIn = Pattern.compile("# New Bifs in (R.+)");

    String[] strings = StringUtil.splitByLines(FileUtilRt.loadFile(file));

    PrintStream g = new PrintStream(new File("src/org/intellij/erlang/bif/ErlangBifTable.java"));

    try {
      g.append("package org.intellij.erlang.bif;\n" +
        "\n" +
        "import org.jetbrains.annotations.NotNull;\n" +
        "import com.intellij.util.containers.MultiMap;\n" +
        "\n" +
        "import java.util.Collection;\n" +
        "import java.util.HashSet;\n" +
        "\n" +
        "public final class ErlangBifTable {\n" +
        "  private static final MultiMap<String, ErlangBifDescriptor> bifMap = new MultiMap<String, ErlangBifDescriptor>() {\n" +
        "    @Override\n" +
        "    protected Collection<ErlangBifDescriptor> createCollection() {\n" +
        "      return new HashSet<ErlangBifDescriptor>();\n" +
        "    }\n" +
        "  };\n" +
        "\n" +
        "  static {\n");

      for (String s : strings) {
        Matcher matcher;
        if ((matcher = bif.matcher(s)).find()) {
          String module = matcher.group(1);
          String name = matcher.group(2);
          String arity = matcher.group(3);
          g.append("    bifMap.putValue(\"").append(module).append("\", new ErlangBifDescriptor(\"").append(module).append("\", \"").append(name).append("\", ").append(arity).append("));\n");
        }
        else if ((matcher = newBifIn.matcher(s)).find()) {
          String version = matcher.group(1).replaceAll("\\.", "");
          g.append("    // Since ").append(version).append("\n");
        }
      }
      g.append("  }\n" +
        "\n" +
        "  private ErlangBifTable() {\n" +
        "  }\n" +
        "\n" +
        "  @NotNull\n" +
        "  public static Collection<ErlangBifDescriptor> getModuleBifs(@NotNull String module) {\n" +
        "    return bifMap.get(module);\n" +
        "  }\n" +
        "\n" +
        "  public static boolean isBif(@NotNull String module, @NotNull String function, int arity) {\n" +
        "    return bifMap.get(module).contains(new ErlangBifDescriptor(module, function, arity));\n" +
        "  }\n" +
        "}\n");
    } finally {
      g.close();
    }
  }
}