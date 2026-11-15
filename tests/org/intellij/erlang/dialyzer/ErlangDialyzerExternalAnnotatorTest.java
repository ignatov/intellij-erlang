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

package org.intellij.erlang.dialyzer;

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInsight.daemon.HighlightDisplayKey;
import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.lang.ExternalLanguageAnnotators;
import com.intellij.lang.annotation.HighlightSeverity;
import com.intellij.profile.codeInspection.ProjectInspectionProfileManager;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangLanguage;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class ErlangDialyzerExternalAnnotatorTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private static final String MESSAGE = "Dialyzer: Function foo/0 has no local return";

  @Override
  protected void setUp() throws Exception {
    super.setUp();
    myFixture.enableInspections(ErlangDialyzerInspection.class);
    ExternalLanguageAnnotators.INSTANCE.addExplicitExtension(ErlangLanguage.INSTANCE, new FakeDialyzerAnnotator(), getTestRootDisposable());
  }

  public void testDefaultSeverityIsWarning() {
    assertEquals(HighlightSeverity.WARNING, highlightDialyzerProblem().getSeverity());
  }

  public void testSeverityFromProfile() {
    HighlightDisplayKey key = HighlightDisplayKey.find(ErlangDialyzerInspection.INSPECTION_SHORT_NAME);
    assertNotNull(key);
    ProjectInspectionProfileManager.getInstance(getProject()).getCurrentProfile()
                                   .setErrorLevel(key, HighlightDisplayLevel.ERROR, getProject());
    assertEquals(HighlightSeverity.ERROR, highlightDialyzerProblem().getSeverity());
  }

  @NotNull
  private HighlightInfo highlightDialyzerProblem() {
    myFixture.configureByText("foo.erl", "-module(foo).\nfoo() -> erlang:error(bar).\n");
    List<HighlightInfo> infos = ContainerUtil.filter(myFixture.doHighlighting(), info -> MESSAGE.equals(info.getDescription()));
    assertSize(1, infos);
    return infos.get(0);
  }

  private static class FakeDialyzerAnnotator extends ErlangDialyzerExternalAnnotator {
    @Override
    public State collectInformation(@NotNull PsiFile file) {
      State state = new State(file.getProject(), "dialyzer", null, "foo.erl", "");
      state.problems.add(new Problem(2, "Function foo/0 has no local return"));
      return state;
    }

    @Override
    public State doAnnotate(State state) {
      return state;
    }
  }
}
