/*
 * Copyright 2012-2016 Sergey Ignatov
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

import com.intellij.testFramework.UsefulTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.regex.Matcher;

import static org.intellij.erlang.jps.builder.ErlangCompilerError.*;


public class ErlangCompilerErrorTest extends UsefulTestCase {

  public void testMatchesMessageFromErlcOnWindows() {
    doRegexMatchTest(
      "e:/test/test_db.erl:47: syntax error before: 'case'",
      "e:/test/test_db.erl",
      "47",
      null,
      "syntax error before: 'case'"
    );
  }

  public void testMatchesMessageFromErlcOnMac() {
    doRegexMatchTest(
      "test/testmeah.erl:18: unterminated atom starting with 't compile\\n'",
      "test/testmeah.erl",
      "18",
      null,
      "unterminated atom starting with 't compile\\n'"
    );
  }

  public void testMatchesMessageFromErlc24OnLinux() {
    doRegexMatchTest(
      "/srv/work/docker-dev/MSVE_dev_env/project/mpro/src/mpro_rest_proxy.erl:476:1: Warning: function merge_pdus/1 is unused\\n'",
      "/srv/work/docker-dev/MSVE_dev_env/project/mpro/src/mpro_rest_proxy.erl",
      "476:1",
      " Warning:",
      "function merge_pdus/1 is unused\\n'"
    );
  }

  private static void doRegexMatchTest(
    @NotNull String erlcMessage,
    @Nullable String path,
    @Nullable String line,
    @Nullable String warning,
    @Nullable String details
  ) {
    Matcher matcher = COMPILER_MESSAGE_PATTERN.matcher(erlcMessage);
    assertTrue(matcher.matches());
    String pathT=matcher.group(PATH_MATCH_INDEX);
    String lineT=matcher.group(LINE_MATCH_INDEX);
    String warnT=matcher.group(WARNING_MATCH_INDEX);
    String detaT=matcher.group(DETAILS_MATCH_INDEX);

    assertEquals(path, pathT);
    assertEquals(line, lineT);
    assertEquals(warning,warnT );
    assertEquals(details, detaT);
  }
}
