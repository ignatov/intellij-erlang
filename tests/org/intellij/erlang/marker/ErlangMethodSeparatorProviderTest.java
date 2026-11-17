/*
 * Copyright 2012-2015 Sergey Ignatov
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

package org.intellij.erlang.marker;

import com.intellij.codeInsight.daemon.DaemonCodeAnalyzerSettings;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.lang.reflect.Method;
import java.util.List;

public class ErlangMethodSeparatorProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected void setUp() throws Exception {
    super.setUp();
    setShowMethodSeparatorsEnabled(true);
  }

  @Override
  protected void tearDown() throws Exception {
    super.tearDown();
    setShowMethodSeparatorsEnabled(false);
  }

  @Override
  protected String getTestDataPath() {
    return "testData/marker/methodSeparators/";
  }

  private static void setShowMethodSeparatorsEnabled(boolean b) {
    DaemonCodeAnalyzerSettings settings = DaemonCodeAnalyzerSettings.getInstance();
    settings.SHOW_METHOD_SEPARATORS = b;
  }

  private void doTest(Integer... expectedLineMarkers) throws Exception {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.doHighlighting();
    assertSameElements(getMarkedLineNumbers(), expectedLineMarkers);
  }

  /**
   * The line markers that the highlighting left in the editor. 2026.3 removed DaemonCodeAnalyzerImpl.getLineMarkers();
   * LineMarkersPass.getDisplayedLineMarkers(), which the platform's test fixture uses there, does not exist in 2026.1
   * and 2026.2. This branch is tested against all three, so the method is looked up by name.
   */
  @SuppressWarnings("unchecked")
  private List<? extends LineMarkerInfo<?>> getLineMarkers() throws Exception {
    String[][] candidates = {{"com.intellij.codeInsight.daemon.impl.LineMarkersPass", "getDisplayedLineMarkers"},
                             {"com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl", "getLineMarkers"}};
    for (String[] candidate : candidates) {
      Method method;
      try {
        method = Class.forName(candidate[0]).getMethod(candidate[1], Document.class, Project.class);
      }
      catch (NoSuchMethodException | ClassNotFoundException e) {
        continue;
      }
      return (List<? extends LineMarkerInfo<?>>)method.invoke(null, myFixture.getEditor().getDocument(), myFixture.getProject());
    }
    fail("No method to read the line markers in this platform");
    return null;
  }

  private Integer[] getMarkedLineNumbers() throws Exception {
    List<? extends LineMarkerInfo<?>> lineMarkers = getLineMarkers();
    assertNotNull(lineMarkers);
    Integer[] markedLineNumbers = new Integer[lineMarkers.size()];
    for (int i = 0; i < lineMarkers.size(); i++) {
      markedLineNumbers[i] = myFixture.getEditor().getDocument().getLineNumber(lineMarkers.get(i).startOffset);
    }
    return markedLineNumbers;
  }

  public void testFunction() throws Exception              { doTest(0); }
  public void testTwoFunctions() throws Exception          { doTest(0, 3); }
  public void testSpec() throws Exception                  { doTest(0); }
  public void testComment() throws Exception               { doTest(0); }
  public void testSpecAndComment() throws Exception        { doTest(0); }
  public void testCommentAndSpec() throws Exception        { doTest(0); }
  public void testCommentSpecAndComment() throws Exception { doTest(0); }
  public void testSpecOfOtherFunction() throws Exception   { doTest(1); }
}
