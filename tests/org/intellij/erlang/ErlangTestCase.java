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

package org.intellij.erlang;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import org.intellij.erlang.completion.ErlangCompletionTest;
import org.intellij.erlang.documentation.ErlangDocumentationProviderTest;
import org.intellij.erlang.folding.ErlangFoldingBuilderTest;
import org.intellij.erlang.formatting.ErlangAutoIndentTest;
import org.intellij.erlang.formatting.ErlangFormattingTest;
import org.intellij.erlang.highlighting.ErlangHighlightingTest;
import org.intellij.erlang.info.ErlangParameterInfoHandlerTest;
import org.intellij.erlang.parser.ErlangAppParserTest;
import org.intellij.erlang.parser.ErlangParserTest;
import org.intellij.erlang.quickfixes.ErlangExportFunctionFixTest;
import org.intellij.erlang.quickfixes.ErlangExportTypeFixTest;
import org.intellij.erlang.quickfixes.ErlangHeadMismatchFixTest;
import org.intellij.erlang.rebar.importWizard.RebarProjectImportBuilderTest;
import org.intellij.erlang.rebar.runner.FileReferenceFilterTest;
import org.intellij.erlang.sdk.ErlangSdkReleaseTest;
import org.intellij.erlang.typing.ErlangTypedHandlerTest;

/**
 * @author ignatov
 */
@SuppressWarnings("ALL")
public class ErlangTestCase extends TestCase {
  public static TestSuite suite() {
    TestSuite suite = new TestSuite();
    suite.addTestSuite(ErlangParserTest.class);
    suite.addTestSuite(ErlangAppParserTest.class);
    suite.addTestSuite(ErlangHighlightingTest.class);
    suite.addTestSuite(ErlangFormattingTest.class);
    suite.addTestSuite(ErlangAutoIndentTest.class);
    suite.addTestSuite(ErlangCompletionTest.class);
    suite.addTestSuite(RebarProjectImportBuilderTest.class);
    suite.addTestSuite(FileReferenceFilterTest.class);
    suite.addTestSuite(ErlangDocumentationProviderTest.class);
    suite.addTestSuite(ErlangParameterInfoHandlerTest.class);
    suite.addTestSuite(ErlangFoldingBuilderTest.class);
    suite.addTestSuite(ErlangExportFunctionFixTest.class);
    suite.addTestSuite(ErlangExportTypeFixTest.class);
    suite.addTestSuite(ErlangHeadMismatchFixTest.class);
    suite.addTestSuite(ErlangTypedHandlerTest.class);
    suite.addTestSuite(ErlangSdkReleaseTest.class);
    return suite;
  }
}
