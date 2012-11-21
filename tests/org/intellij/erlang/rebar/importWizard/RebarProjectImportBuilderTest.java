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

package org.intellij.erlang.rebar.importWizard;

import com.intellij.testFramework.IdeaTestCase;
import org.junit.Test;

import java.util.regex.Matcher;

import static org.junit.Assert.assertEquals;

public class RebarProjectImportBuilderTest extends IdeaTestCase {

  public void testApplicationNamePattern() {
    Matcher matcher = RebarProjectImportBuilder.APPLICATION_NAME_PATTERN.matcher(
      "asdfa asdf asdf { asdf, asdf.} {application,   bl ah  , sdfl : sdf} llkjnasdf asd");
    assertEquals(true, matcher.find());
    assertEquals("bl ah", matcher.group(1));
  }

  public void testDirsListPattern() {
    Matcher matcher = RebarProjectImportBuilder.DIRS_LIST_PATTERN.matcher(
      "asdfa  {sub_dirs ,[\"aaa\", \"bb\"]} {application, [\"ccc\", \"ddddd\"]} llkjnasdf asd");
    assertEquals(true, matcher.find());
    assertEquals("\"aaa\", \"bb\"", matcher.group(1));
  }

  public void testSplitDirListPattern() {
    String[] dirs = RebarProjectImportBuilder.SPLIT_DIR_LIST_PATTERN.split("\"aaa\", \"bb\"   ,   \"ccc\"  ,\"ddddd\"");
    assertEquals("aaa", dirs[1]);
    assertEquals("bb", dirs[2]);
    assertEquals("ccc", dirs[3]);
    assertEquals("ddddd", dirs[4]);
  }
}
