/*
 * Copyright 2012-2013 Sergey Ignatov
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

package org.intellij.erlang;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.intellij.erlang.compilation.ErlangDependenciesResolutionTest;
import org.intellij.erlang.completion.ErlangCompletionTest;
import org.intellij.erlang.completion.ErlangCompletionWithSdkTest;
import org.intellij.erlang.completion.ErlangSmallIdeCompletionTest;
import org.intellij.erlang.console.FileReferenceFilterTest;
import org.intellij.erlang.debugger.ErlangSourcePositionTest;
import org.intellij.erlang.documentation.ErlangDocumentationProviderTest;
import org.intellij.erlang.eunit.ErlangEunitDetectionTest;
import org.intellij.erlang.eunit.ErlangUnitTestElementUtilTest;
import org.intellij.erlang.folding.ErlangFoldingBuilderTest;
import org.intellij.erlang.formatting.ErlangAutoIndentTest;
import org.intellij.erlang.formatting.ErlangFormattingTest;
import org.intellij.erlang.highlighting.ErlangBehaviourInspectionsTest;
import org.intellij.erlang.highlighting.ErlangConsoleViewTest;
import org.intellij.erlang.highlighting.ErlangHighlightingTest;
import org.intellij.erlang.highlighting.ErlangSmallIdeHighlightingTest;
import org.intellij.erlang.highlighting.generate.ErlangGenerateTest;
import org.intellij.erlang.info.ErlangParameterInfoHandlerTest;
import org.intellij.erlang.navigation.ErlangGotoSuperTest;
import org.intellij.erlang.parser.ErlangAppParserTest;
import org.intellij.erlang.parser.ErlangParserTest;
import org.intellij.erlang.performance.ErlangPerformanceTest;
import org.intellij.erlang.quickfixes.*;
import org.intellij.erlang.rebar.importWizard.RebarProjectImportBuilderTest;
import org.intellij.erlang.refactoring.ErlangExtractFunctionTest;
import org.intellij.erlang.refactoring.ErlangInlineVariableTest;
import org.intellij.erlang.refactoring.ErlangIntroduceVariableTest;
import org.intellij.erlang.refactoring.ErlangSafeDeleteTest;
import org.intellij.erlang.resolve.ErlangIncludeLibResolveTest;
import org.intellij.erlang.resolve.ErlangIncludeResolveTest;
import org.intellij.erlang.resolve.ErlangModuleResolutionTest;
import org.intellij.erlang.resolve.ErlangSmallIdeIncludeResolveTest;
import org.intellij.erlang.sdk.ErlangSdkReleaseTest;
import org.intellij.erlang.selection.ErlangWordSelectionTest;
import org.intellij.erlang.typing.ErlangEnterHandlerTest;
import org.intellij.erlang.typing.ErlangSmartEnterClauseProcessorTest;
import org.intellij.erlang.typing.ErlangTypedHandlerTest;

@SuppressWarnings("ALL")
public class ErlangTestCase extends TestCase {
  public static TestSuite suite() {
    TestSuite suite = new TestSuite();
    suite.addTestSuite(ErlangParserTest.class);
    suite.addTestSuite(ErlangAppParserTest.class);
    suite.addTestSuite(ErlangHighlightingTest.class);
    suite.addTestSuite(ErlangSmallIdeHighlightingTest.class);
    suite.addTestSuite(ErlangFormattingTest.class);
    suite.addTestSuite(ErlangAutoIndentTest.class);
    suite.addTestSuite(ErlangCompletionTest.class);
    suite.addTestSuite(ErlangSmallIdeCompletionTest.class);
    suite.addTestSuite(ErlangCompletionWithSdkTest.class);
    suite.addTestSuite(RebarProjectImportBuilderTest.class);
    suite.addTestSuite(FileReferenceFilterTest.class);
    suite.addTestSuite(ErlangDocumentationProviderTest.class);
    suite.addTestSuite(ErlangParameterInfoHandlerTest.class);
    suite.addTestSuite(ErlangFoldingBuilderTest.class);
    suite.addTestSuite(ErlangFunctionFixesTest.class);
    suite.addTestSuite(ErlangExportTypeFixTest.class);
    suite.addTestSuite(ErlangHeadMismatchFixTest.class);
    suite.addTestSuite(ErlangIntroduceFunctionFixTest.class);
    suite.addTestSuite(ErlangIntroduceVariableFixTest.class);
    suite.addTestSuite(ErlangIntroduceRecordFixTest.class);
    suite.addTestSuite(ErlangIntroduceRecordFieldTest.class);
    suite.addTestSuite(ErlangIntroduceMacroQuickFixTest.class);
    suite.addTestSuite(ErlangIntroduceVariableTest.class);
    suite.addTestSuite(ErlangFindIncludeQuickFixTest.class);
    suite.addTestSuite(ErlangExtractFunctionTest.class);
    suite.addTestSuite(ErlangSafeDeleteTest.class);
    suite.addTestSuite(ErlangSmartEnterClauseProcessorTest.class);
    suite.addTestSuite(ErlangSdkReleaseTest.class);
    suite.addTestSuite(ErlangBehaviourInspectionsTest.class);
    suite.addTestSuite(ErlangCopyFileTest.class);
    suite.addTestSuite(ErlangGenerateTest.class);
    suite.addTestSuite(ErlangInlineVariableTest.class);
    suite.addTestSuite(ErlangGotoSuperTest.class);
    suite.addTestSuite(ErlangTypedHandlerTest.class);
    suite.addTestSuite(ErlangEnterHandlerTest.class);
    suite.addTestSuite(ErlangWordSelectionTest.class);
    suite.addTestSuite(ErlangIncludeLibResolveTest.class);
    suite.addTestSuite(ErlangIncludeResolveTest.class);
    suite.addTestSuite(ErlangSmallIdeIncludeResolveTest.class);
    suite.addTestSuite(ErlangUnitTestElementUtilTest.class);
    suite.addTestSuite(ErlangEunitDetectionTest.class);
    suite.addTestSuite(ErlangDependenciesResolutionTest.class);
    suite.addTestSuite(ErlangConsoleViewTest.class);
    suite.addTestSuite(ErlangSourcePositionTest.class);
    suite.addTestSuite(ErlangModuleResolutionTest.class);
    suite.addTestSuite(ErlangPerformanceTest.class);
    return suite;
  }
}
