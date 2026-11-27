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

import com.intellij.codeInsight.daemon.GutterMark;
import com.intellij.codeInsight.daemon.LineMarkerInfo;
import com.intellij.openapi.editor.Document;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;

import java.util.ArrayList;
import java.util.List;

public class ErlangBehaviourMarkerProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/marker/behaviour/";
  }

  /** #1696: a callback whose name is quoted gets one Implements callback icon, as a plain name does. */
  public void testQuotedCallbackNameHasOneMarker() {
    myFixture.configureByFiles("m.erl", "b.erl");
    Document document = myFixture.getEditor().getDocument();
    List<String> marked = new ArrayList<>();
    for (GutterMark mark : myFixture.findAllGutters()) {
      String tooltip = mark.getTooltipText();
      if (tooltip != null && tooltip.startsWith("Implements callback") && mark instanceof LineMarkerInfo.LineMarkerGutterIconRenderer<?> r) {
        marked.add((document.getLineNumber(r.getLineMarkerInfo().startOffset) + 1) + ": " + tooltip);
      }
    }
    assertEquals(List.of("5: Implements callback ''my-cb'/0'",
                         "7: Implements callback 'init/1'",
                         "9: Implements callback 'plain/0'"), marked);
  }
}
