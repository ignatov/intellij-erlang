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

package org.intellij.erlang.documentation;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("ConstantConditions")
public class ErlangDocumentationProviderTest extends LightCodeInsightFixtureTestCase {
  private ErlangDocumentationProvider myErlangDocProvider;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    myErlangDocProvider = new ErlangDocumentationProvider();
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        final Sdk sdk = getProjectDescriptor().getSdk();
        ProjectJdkTable.getInstance().addJdk(sdk);
        ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
      }
    });
  }

  public void testExternalUrlSdkFunction() throws Exception {
    doTestGetUrls("http://www.erlang.org/documentation/doc-5.9.2/lib/stdlib-1.18.2/doc/html/lists.html#foreach-2",
      "-module(test).\n" +
        "test() ->\n" +
        "    lists:for<caret>each(foo, bar).\n");
  }

  public void testExternalUrlSdkBif() throws Exception {
    doTestGetUrls("http://www.erlang.org/documentation/doc-5.9.2/lib/stdlib-1.18.2/doc/html/lists.html#member-2",
      "-module(test).\n" +
        "test() ->\n" +
        "    lists:mem<caret>ber(foo, bar).\n");
  }

  public void testExternalUrlSdkModule() throws Exception {
    doTestGetUrls("http://www.erlang.org/documentation/doc-5.9.2/lib/stdlib-1.18.2/doc/html/lists.html",
      "-module(test).\n" +
        "test() ->\n" +
        "    lis<caret>ts:foreach(foo, bar).\n");
  }

  public void testGenerateDocSdkBif() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"member-2\"><span class=\"bold_code\">member(Elem, List) -&gt; boolean()</span></a><br></p><div class=\"REFBODY\">\n" +
        "<p>Types:</p>\n" +
        "        <div class=\"REFTYPES\">\n" +
        "<span class=\"bold_code\">Elem = term()</span><br>\n" +
        "</div>\n" +
        "        <div class=\"REFTYPES\">\n" +
        "<span class=\"bold_code\">List = [term()]</span><br>\n" +
        "</div>\n" +
        "      </div>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Returns <span class=\"code\">true</span> if <span class=\"code\">Elem</span> matches some element of\n" +
        "          <span class=\"code\">List</span>, otherwise <span class=\"code\">false</span>.</p>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    lists:mem<caret>ber(foo, bar).\n");
  }

  public void testGenerateDocSdkFunction() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"foreach-2\"></a><span class=\"bold_code\">foreach(Fun, List) -&gt; ok</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Fun = fun((Elem :: T) -&gt; term())</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">List = [T]</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">T = term()</span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Calls <span class=\"code\">Fun(Elem)</span> for each element <span class=\"code\">Elem</span> in\n" +
        "          <span class=\"code\">List</span>. This function is used for its side effects and\n" +
        "          the evaluation order is defined to be the same as the order\n" +
        "          of the elements in the list.</p>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    lists:for<caret>each(foo, bar).\n");
  }

  public void testGenerateDocSdkFunctionMulti() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"seq-2\"></a><span class=\"bold_code\">seq(From, To) -&gt; Seq</span><br><a name=\"seq-3\"></a><span class=\"bold_code\">seq(From, To, Incr) -&gt; Seq</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">From = To = Incr = integer()</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\"></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\"></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Seq = [integer()]</span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Returns a sequence of integers which starts with <span class=\"code\">From</span>\n" +
        "          and contains the successive results of adding <span class=\"code\">Incr</span> to\n" +
        "          the previous element, until <span class=\"code\">To</span> has been reached or\n" +
        "          passed (in the latter case, <span class=\"code\">To</span> is not an element of\n" +
        "          the sequence). <span class=\"code\">Incr</span> defaults to 1.</p>\n" +
        "        <p>Failure: If <span class=\"code\">To&lt;From-Incr</span> and <span class=\"code\">Incr</span>\n" +
        "          is positive, or if <span class=\"code\">To&gt;From-Incr</span> and <span class=\"code\">Incr</span> is\n" +
        "          negative, or if <span class=\"code\">Incr==0</span> and <span class=\"code\">From/=To</span>.</p>\n" +
        "        <p>The following equalities hold for all sequences:</p>\n" +
        "        <div class=\"example\"><pre>\n" +
        "length(lists:seq(From, To)) == To-From+1\n" +
        "length(lists:seq(From, To, Incr)) == (To-From+Incr) div Incr</pre></div>\n" +
        "        <p>Examples:</p>\n" +
        "        <div class=\"example\"><pre>\n" +
        "&gt; <span class=\"bold_code\">lists:seq(1, 10).</span>\n" +
        "[1,2,3,4,5,6,7,8,9,10]\n" +
        "&gt; <span class=\"bold_code\">lists:seq(1, 20, 3).</span>\n" +
        "[1,4,7,10,13,16,19]\n" +
        "&gt; <span class=\"bold_code\">lists:seq(1, 0, 1).</span>\n" +
        "[]\n" +
        "&gt; <span class=\"bold_code\">lists:seq(10, 6, 4).</span>\n" +
        "[]\n" +
        "&gt; <span class=\"bold_code\">lists:seq(1, 1, 0).</span>\n" +
        "[1]</pre></div>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    lists:s<caret>eq(foo, bar, blah).\n");
  }

  public void testGenerateDocSdkLastFunction() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"zipwith3-4\"></a><span class=\"bold_code\">zipwith3(Combine, List1, List2, List3) -&gt; List4</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Combine = fun((X, Y, Z) -&gt; T)</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">List1 = [X]</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">List2 = [Y]</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">List3 = [Z]</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">List4 = [T]</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">X = Y = Z = T = term()</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\"></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\"></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\"></span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Combine the elements of three lists of equal length into one\n" +
        "          list. For each triple <span class=\"code\">X, Y, Z</span> of list elements from\n" +
        "          the three lists, the element in the result list will be\n" +
        "          <span class=\"code\">Combine(X, Y, Z)</span>.</p>\n" +
        "        <p><span class=\"code\">zipwith3(fun(X, Y, Z) -&gt; {X,Y,Z} end, List1, List2, List3)</span> is equivalent to <span class=\"code\">zip3(List1, List2, List3)</span>.</p>\n" +
        "        <p>Examples:</p>\n" +
        "        <div class=\"example\"><pre>\n" +
        "&gt; <span class=\"bold_code\">lists:zipwith3(fun(X, Y, Z) -&gt; X+Y+Z end, [1,2,3], [4,5,6], [7,8,9]).</span>\n" +
        "[12,15,18]\n" +
        "&gt; <span class=\"bold_code\">lists:zipwith3(fun(X, Y, Z) -&gt; [X,Y,Z] end, [a,b,c], [x,y,z], [1,2,3]).</span>\n" +
        "[[a,x,1],[b,y,2],[c,z,3]]</pre></div>\n" +
        "      </p></div>\n" +
        "  \n" +
        "</div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    lists:zip<caret>with3(foo, bar, preved, medved).\n");
  }

  public void testGenerateDocSdkModule() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "  <h3>MODULE</h3><div class=\"REFBODY\">lists</div>\n" +
        "  <h3>MODULE SUMMARY</h3>\n" +
        "<div class=\"REFBODY\">List Processing Functions</div>\n" +
        "  <h3>DESCRIPTION</h3>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "    <p>This module contains functions for list processing.</p>\n" +
        "\n" +
        "    <p>Unless otherwise stated, all functions assume that position\n" +
        "      numbering starts at 1. That is, the first element of a list is at\n" +
        "      position 1.</p>\n" +
        "\n" +
        "    <p>Two terms <span class=\"code\">T1</span> and <span class=\"code\">T2</span> compare equal if\n" +
        "      <span class=\"code\">T1 == T2</span> evaluates to <span class=\"code\">true</span>. They match\n" +
        "      if <span class=\"code\">T1 =:= T2</span> evaluates to <span class=\"code\">true</span>.</p>\n" +
        "\n" +
        "    <p>Whenever an <a name=\"ordering_function\"></a><strong>ordering function</strong>\n" +
        "      <span class=\"code\">F</span> is expected as argument, it is assumed that the\n" +
        "      following properties hold of <span class=\"code\">F</span> for all x, y and z:</p>\n" +
        "    <ul>\n" +
        "      <li>\n" +
        "<p>if x <span class=\"code\">F</span> y and y <span class=\"code\">F</span> x then x = y (<span class=\"code\">F</span>\n" +
        "        is antisymmetric);</p>\n" +
        "      </li>\n" +
        "      <li>\n" +
        "<p>if x <span class=\"code\">F</span> y and y <span class=\"code\">F</span> z then x <span class=\"code\">F</span> z\n" +
        "        (<span class=\"code\">F</span> is transitive);</p>\n" +
        "      </li>\n" +
        "      <li>\n" +
        "<p>x <span class=\"code\">F</span> y or y <span class=\"code\">F</span> x (<span class=\"code\">F</span> is total).</p>\n" +
        "      </li>\n" +
        "    </ul>\n" +
        "    <p>An example of a typical ordering function is less than or equal\n" +
        "      to, <span class=\"code\">=&lt;/2</span>.</p>\n" +
        "\n" +
        "  </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    lis<caret>ts:foreach(foo, bar).\n");
  }

  public void testGenerateDocSdkType() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "      <span class=\"bold_code\"><a name=\"type-file_info\">file_info()</a> = <br>    #file_info{size = undefined | integer() &gt;= 0,<br>               type = undefined<br>                     | device<br>                     | directory<br>                     | other<br>                     | regular<br>                     | symlink,<br>               access = undefined<br>                       | read<br>                       | write<br>                       | read_write<br>                       | none,<br>               atime = undefined | <span class=\"bold_code\"><a href=\"file.html#type-date_time\">file:date_time()</a></span> | integer(),<br>               mtime = undefined | <span class=\"bold_code\"><a href=\"file.html#type-date_time\">file:date_time()</a></span> | integer(),<br>               ctime = undefined | <span class=\"bold_code\"><a href=\"file.html#type-date_time\">file:date_time()</a></span> | integer(),<br>               mode = undefined | integer(),<br>               links = undefined | integer() &gt;= 0,<br>               major_device = undefined | integer(),<br>               minor_device = undefined | integer(),<br>               inode = undefined | integer(),<br>               uid = undefined | integer(),<br>               gid = undefined | integer()}</span><br></p>    <p>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "-spec test(file:fil<caret>e_info()) -> ok." +
        "test(_FileInfo) ->\n" +
        "    ok.\n");
  }

  public void testGenerateDocSdkLastType() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        AbstractSdkDocProvider.HTTP_STYLE +
        "<body>\n" +
        "      <span class=\"bold_code\"><a name=\"type-sendfile_option\">sendfile_option()</a> = {chunk_size, integer() &gt;= 0}</span><br></p>   \n" +
        "\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "-spec test(file:send<caret>file_option()) -> ok." +
        "test(_SendFileOption) ->\n" +
        "    ok.\n");
  }

  @NotNull
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        final Sdk mockSdk = ErlangSdkType.createMockSdk("testData/mockSdk-R15B02/");
        // Set local SDK documentation path
        final SdkModificator sdkModificator = mockSdk.getSdkModificator();
        final VirtualFile localDocDir = LocalFileSystem.getInstance().findFileByPath("testData/mockSdk-R15B02/");
        sdkModificator.addRoot(localDocDir, JavadocOrderRootType.getInstance());
        sdkModificator.commitChanges();
        return mockSdk;
      }
    };
  }

  private void doTestGetUrls(@NotNull String expected, @NotNull String text) throws Exception {
    final PsiElement element = resolveElementAtCaret(text);
    assertEquals(expected, myErlangDocProvider.getUrlFor(element, null).get(0));
  }

  private void doTestGenerateDoc(@NotNull String expected, @NotNull String text) throws Exception {
    final PsiElement element = resolveElementAtCaret(text);
    assertEquals(expected, myErlangDocProvider.generateDoc(element, null));
  }

  private PsiElement resolveElementAtCaret(String text) {
    myFixture.configureByText("test.erl", text);
    final int caretPosition = myFixture.getEditor().getCaretModel().getOffset();
    final PsiReference psiReference = myFixture.getFile().findReferenceAt(caretPosition);
    final PsiElement resolve = psiReference.resolve();
    assertNotNull(resolve);
    return resolve;
  }
}