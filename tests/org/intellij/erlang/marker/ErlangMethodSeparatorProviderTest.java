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
import com.intellij.codeInsight.daemon.impl.DaemonCodeAnalyzerImpl;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

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

  @Override
  protected boolean isWriteActionRequired() {
    return false;
  }

  private static void setShowMethodSeparatorsEnabled(boolean b) {
    DaemonCodeAnalyzerSettings settings = DaemonCodeAnalyzerSettings.getInstance();
    settings.SHOW_METHOD_SEPARATORS = b;
  }

  private void doTest(Integer... expectedLineMarkers) {
    myFixture.configureByFile(getTestName(true) + ".erl");
    myFixture.doHighlighting();
    assertSameElements(getMarkedLineNumbers(), expectedLineMarkers);
  }

  private Integer[] getMarkedLineNumbers() {
    List<LineMarkerInfo> lineMarkers = DaemonCodeAnalyzerImpl.getLineMarkers(myFixture.getEditor().getDocument(), myFixture.getProject());
    assertNotNull(lineMarkers);
    Integer[] markedLineNumbers = new Integer[lineMarkers.size()];
    for (int i = 0; i < lineMarkers.size(); i++) {
      markedLineNumbers[i] = myFixture.getEditor().getDocument().getLineNumber(lineMarkers.get(i).startOffset);
    }
    return markedLineNumbers;
  }

  public void testFunction()              { doTest(0); }
  public void testTwoFunctions()          { doTest(0, 3); }
  public void testSpec()                  { doTest(0); }
  public void testComment()               { doTest(0); }
  public void testSpecAndComment()        { doTest(0); }
  public void testCommentAndSpec()        { doTest(0); }
  public void testCommentSpecAndComment() { doTest(0); }
  public void testSpecOfOtherFunction()   { doTest(1); }
}
