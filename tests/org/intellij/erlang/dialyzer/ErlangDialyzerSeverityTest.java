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

package org.intellij.erlang.dialyzer;

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInsight.daemon.HighlightDisplayKey;
import com.intellij.lang.ExternalLanguageAnnotators;
import com.intellij.profile.codeInspection.InspectionProjectProfileManager;
import com.intellij.psi.PsiFile;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class ErlangDialyzerSeverityTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  @Override
  protected String getTestDataPath() {
    return "testData/dialyzer/";
  }

  public void testErrorSeverity() {
    // The output line of OTP 27 dialyzer for this module is "ErrorSeverity.erl:5:1: Function g/1 has no local return".
    ExternalLanguageAnnotators.INSTANCE.addExplicitExtension(ErlangLanguage.INSTANCE, new CannedDialyzerAnnotator(
      new ErlangDialyzerExternalAnnotator.Problem(5, "Function g/1 has no local return")), getTestRootDisposable());
    myFixture.enableInspections(new ErlangDialyzerInspection());
    InspectionProjectProfileManager.getInstance(getProject()).getCurrentProfile()
      .setErrorLevel(HighlightDisplayKey.find(ErlangDialyzerInspection.INSPECTION_SHORT_NAME), HighlightDisplayLevel.ERROR, getProject());

    myFixture.configureByFile(getTestName(false) + ".erl");
    myFixture.checkHighlighting();
  }

  private static class CannedDialyzerAnnotator extends ErlangDialyzerExternalAnnotator {
    private final Problem myProblem;

    CannedDialyzerAnnotator(@NotNull Problem problem) {
      myProblem = problem;
    }

    @Override
    public State collectInformation(@NotNull PsiFile file) {
      State state = new State(file.getProject(), "", "", "", "");
      state.problems.add(myProblem);
      return state;
    }

    @Override
    public State doAnnotate(State state) {
      return state;
    }
  }
}
