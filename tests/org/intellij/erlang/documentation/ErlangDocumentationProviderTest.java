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

package org.intellij.erlang.documentation;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.roots.JavadocOrderRootType;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.psi.ErlangFunction;
import org.intellij.erlang.psi.ErlangModule;
import org.intellij.erlang.psi.ErlangTypeDefinition;
import org.intellij.erlang.sdk.ErlangSdkType;
import org.intellij.erlang.utils.ErlangLightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

@SuppressWarnings("ConstantConditions")
public class ErlangDocumentationProviderTest extends ErlangLightPlatformCodeInsightFixtureTestCase {
  private ErlangDocumentationProvider myErlangDocProvider;

  @Override
  public void setUp() throws Exception {
    super.setUp();
    myErlangDocProvider = new ErlangDocumentationProvider();
    setUpProjectSdk();
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
        "<body>\n" +
        "      <span class=\"bold_code\"><a name=\"type-file_info\">file_info()</a> = <br>    #file_info{size = undefined | integer() &gt;= 0,<br>               type = undefined<br>                     | device<br>                     | directory<br>                     | other<br>                     | regular<br>                     | symlink,<br>               access = undefined<br>                       | read<br>                       | write<br>                       | read_write<br>                       | none,<br>               atime = undefined | <span class=\"bold_code\"><a href=\"psi_element://file#type-date_time\">file:date_time()</a></span> | integer(),<br>               mtime = undefined | <span class=\"bold_code\"><a href=\"psi_element://file#type-date_time\">file:date_time()</a></span> | integer(),<br>               ctime = undefined | <span class=\"bold_code\"><a href=\"psi_element://file#type-date_time\">file:date_time()</a></span> | integer(),<br>               mode = undefined | integer(),<br>               links = undefined | integer() &gt;= 0,<br>               major_device = undefined | integer(),<br>               minor_device = undefined | integer(),<br>               inode = undefined | integer(),<br>               uid = undefined | integer(),<br>               gid = undefined | integer()}</span><br></p>    <p>\n" +
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
        ErlangSdkDocProviderBase.HTTP_STYLE +
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

  public void testLinkConverterLocal() throws Exception {
    // <a href="#write_$FUNC$-$ARITY$">
    // <a href="#type-$TYPE$">
    doTestGenerateDoc(
      "<html>\n" +
        ErlangSdkDocProviderBase.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"change_time-2\"></a><span class=\"bold_code\">change_time(Filename, Mtime) -&gt; ok | {error, Reason}</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Filename = <span class=\"bold_code\"><a href=\"psi_element://file#type-name\">name()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Mtime = <span class=\"bold_code\"><a href=\"psi_element://file#type-date_time\">date_time()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Reason = <span class=\"bold_code\"><a href=\"psi_element://file#type-posix\">posix()</a></span> | badarg</span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Changes the modification and access times of a file. See\n" +
        "          <span class=\"bold_code\"><a href=\"psi_element://file#write_file_info-2\">write_file_info/2</a></span>.</p>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    file:cha<caret>nge_time(foo, bar).\n");
  }

  public void testLinkConverterErlRef() throws Exception {
    // <a href="javascript:erlhref('$REL-PATH$','$APPLICATION$','$MODULE$.html#type-$TYPE$');">
    // <a href="javascript:erlhref('$REL-PATH$','$APPLICATION$','$MODULE$.html');">
    doTestGenerateDoc(
      "<html>\n" +
        ErlangSdkDocProviderBase.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"path_script-3\"></a><span class=\"bold_code\">path_script(Path, Filename, Bindings) -&gt;<br>               {ok, Value, FullName} | {error, Reason}</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Path = [Dir :: <span class=\"bold_code\"><a href=\"psi_element://file#type-name\">name()</a></span>]</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Filename = <span class=\"bold_code\"><a href=\"psi_element://file#type-name\">name()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Bindings = <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-binding_struct\">erl_eval:binding_struct()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Value = term()</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">FullName = <span class=\"bold_code\"><a href=\"psi_element://file#type-filename\">filename()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Reason = <span class=\"bold_code\"><a href=\"psi_element://file#type-posix\">posix()</a></span><br>       | badarg<br>       | terminated<br>       | system_limit<br>       | {Line :: integer(), Mod :: module(), Term :: term()}</span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>The same as <span class=\"code\">path_script/2</span> but the variable bindings\n" +
        "          <span class=\"code\">Bindings</span> are used in the evaluation. See\n" +
        "          <span class=\"bold_code\"><a href=\"psi_element://erl_eval\">erl_eval(3)</a></span> about\n" +
        "          variable bindings.</p>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    file:pat<caret>h_script(1, 2, 3).\n");
  }

  public void testLinkConverterErlRef2() throws Exception {
    // <a href="javascript:erlhref('$REL-PATH$','$APPLICATION$','$MODULE$.html#$FUNCTION$-$ARITY$');">
    doTestGenerateDoc(
      "<html>\n" +
        ErlangSdkDocProviderBase.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"read_line-1\"></a><span class=\"bold_code\">read_line(IoDevice) -&gt; {ok, Data} | eof | {error, Reason}</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">IoDevice = <span class=\"bold_code\"><a href=\"psi_element://file#type-io_device\">io_device()</a></span> | atom()</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Data = string() | binary()</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Reason = <span class=\"bold_code\"><a href=\"psi_element://file#type-posix\">posix()</a></span> | badarg | terminated</span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Reads a line of bytes/characters from the file referenced by\n" +
        "          <span class=\"code\">IoDevice</span>. Lines are defined to be delimited by the linefeed (LF, <span class=\"code\">\\n</span>) character, but any carriage return (CR, <span class=\"code\">\\r</span>) followed by a newline is also treated as a single LF character (the carriage return is silently ignored). The line is returned <strong>including</strong> the LF, but excluding any CR immediately followed by a LF. This behaviour is consistent with the behaviour of <span class=\"bold_code\"><a href=\"psi_element://io#get_line-2\">io:get_line/2</a></span>. If end of file is reached without any LF ending the last line, a line with no trailing LF is returned.</p>\n" +
        "\t  <p>The function can be used on files opened in <span class=\"code\">raw</span> mode. It is however inefficient to use it on <span class=\"code\">raw</span> files if the file is not opened with the option <span class=\"code\">{read_ahead, Size}</span> specified, why combining <span class=\"code\">raw</span> and <span class=\"code\">{read_ahead, Size}</span> is highly recommended when opening a text file for raw line oriented reading.</p> \n" +
        "\t  <p>If <span class=\"code\">encoding</span> is set to something else than <span class=\"code\">latin1</span>, the <span class=\"code\">read_line/1</span> call will fail if the data contains characters larger than 255, why the <span class=\"bold_code\"><a href=\"psi_element://io\">io(3)</a></span> module is to be preferred when reading such a file.</p> \n" +
        "\t  <p>The function returns:</p>\n" +
        "        <dl>\n" +
        "          <dt><strong><span class=\"code\">{ok, Data}</span></strong></dt>\n" +
        "          <dd>\n" +
        "\t    <p>One line from the file is returned, including the trailing LF, but with CRLF sequences replaced by a single LF (see above).</p>\n" +
        "            <p>If the file was opened in binary mode, the read bytes are\n" +
        "              returned in a binary, otherwise in a list.</p>\n" +
        "          </dd>\n" +
        "          <dt><strong><span class=\"code\">eof</span></strong></dt>\n" +
        "          <dd>\n" +
        "            <p>Returned if end of file was reached\n" +
        "              before anything at all could be read.</p>\n" +
        "          </dd>\n" +
        "          <dt><strong><span class=\"code\">{error, Reason}</span></strong></dt>\n" +
        "          <dd>\n" +
        "            <p>An error occurred.</p>\n" +
        "          </dd>\n" +
        "        </dl>\n" +
        "        <p>Typical error reasons:</p>\n" +
        "        <dl>\n" +
        "          <dt><strong><span class=\"code\">ebadf</span></strong></dt>\n" +
        "          <dd>\n" +
        "            <p>The file is not opened for reading.</p>\n" +
        "          </dd>\n" +
        "          <dt><strong><span class=\"code\">{no_translation, unicode, latin1}</span></strong></dt>\n" +
        "          <dd>\n" +
        "            <p>The file is was opened with another <span class=\"code\">encoding</span> than <span class=\"code\">latin1</span> and the data on the file can not be translated to the byte-oriented data that this function returns.</p>\n" +
        "          </dd>\n" +
        "        </dl>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    file:rea<caret>d_line(1).\n");
  }

  public void testLinkConverterInModule() throws Exception {
    // <a href="$MODULE$.html#$FUNCTION$-$ARITY$">
    // <a href="$MODULE$.html#type-$TYPE$">
    doTestGenerateDoc(
      "<html>\n" +
        ErlangSdkDocProviderBase.HTTP_STYLE +
        "<body>\n" +
        "    <p><a name=\"sendfile-5\"></a><span class=\"bold_code\">sendfile(RawFile, Socket, Offset, Bytes, Opts) -&gt;<br>            {ok, integer() &gt;= 0} |<br>            {error, <span class=\"bold_code\"><a href=\"psi_element://inet#type-posix\">inet:posix()</a></span> | closed | badarg | not_owner}</span><br><div class=\"REFBODY\"><p>Types:</p>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">RawFile = <span class=\"bold_code\"><a href=\"psi_element://file#type-fd\">file:fd()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Socket = <span class=\"bold_code\"><a href=\"psi_element://inet#type-socket\">inet:socket()</a></span></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Offset = Bytes = integer() &gt;= 0</span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\"></span></div>\n" +
        "<div class=\"REFTYPES\"><span class=\"bold_code\">Opts = [<span class=\"bold_code\"><a href=\"psi_element://file#type-sendfile_option\">sendfile_option()</a></span>]</span></div>\n" +
        "</div></p>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "        <p>Sends <span class=\"code\">Bytes</span> from the file\n" +
        "        referenced by <span class=\"code\">RawFile</span> beginning at <span class=\"code\">Offset</span> to\n" +
        "        <span class=\"code\">Socket</span>.\n" +
        "        Returns <span class=\"code\">{ok, BytesSent}</span> if successful,\n" +
        "        otherwise <span class=\"code\">{error, Reason}</span>. If <span class=\"code\">Bytes</span> is set to\n" +
        "\t0 all data after the given <span class=\"code\">Offset</span> is sent.</p>\n" +
        "\t<p>The file used must be opened using the raw flag, and the process\n" +
        "\tcalling sendfile must be the controlling process of the socket.\n" +
        "\tSee <span class=\"bold_code\"><a href=\"psi_element://gen_tcp#controlling_process-2\">gen_tcp:controlling_process/2</a></span></p>\n" +
        "\t<p>If the OS used does not support sendfile, an Erlang fallback\n" +
        "\tusing file:read and gen_tcp:send is used.</p>\n" +
        "\t<p>The option list can contain the following options:\n" +
        "\t<dl>\n" +
        "          <dt><strong><span class=\"code\">chunk_size</span></strong></dt>\n" +
        "          <dd>The chunk size used by the erlang fallback to send\n" +
        "\t  data. If using the fallback, this should be set to a value\n" +
        "\t  which comfortably fits in the systems memory. Default is 20 MB.</dd>\n" +
        "\t</dl>\n" +
        "\t</p>\n" +
        "\t<p>On operating systems with thread support, it is recommended to use\n" +
        "\tasync threads. See the command line flag\n" +
        "\t<span class=\"code\">+A</span> in <span class=\"bold_code\"><a href=\"psi_element://erl\">erl(1)</a></span>. If it is not\n" +
        "\tpossible to use async threads for sendfile, it is recommended to use\n" +
        "\ta relatively small value for the send buffer on the socket. Otherwise\n" +
        "\tthe Erlang VM might loose some of its soft realtime guarantees.\n" +
        "\tWhich size to use depends on the OS/hardware and the requirements\n" +
        "\tof the application.</p>\n" +
        "      </p></div>\n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    file:sen<caret>dfile(1,2,3,4,5).\n");
  }

  public void testLinkConverterWithLineBreak() throws Exception {
    doTestGenerateDoc(
      "<html>\n" +
        ErlangSdkDocProviderBase.HTTP_STYLE +
        "<body>\n" +
        "  <h3>MODULE</h3><div class=\"REFBODY\">erl_eval</div>\n" +
        "  <h3>MODULE SUMMARY</h3>\n" +
        "<div class=\"REFBODY\">The Erlang Meta Interpreter</div>\n" +
        "  <h3>DESCRIPTION</h3>\n" +
        "<div class=\"REFBODY\"><p>\n" +
        "    <p>This module provides an interpreter for Erlang expressions. The\n" +
        "      expressions are in the abstract syntax as returned by\n" +
        "      <span class=\"bold_code\"><a href=\"psi_element://erl_parse\"><span class=\"code\">erl_parse</span></a></span>,\n" +
        // Link to module `io` is multi-line
        "      the Erlang parser, or <span class=\"bold_code\"><a href=\"psi_element://io\">\n" +
        "      <span class=\"code\">io</span></a></span>.</p>\n" +
        "  </p></div>\n" +
        "  <h3>DATA TYPES</h3>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-bindings\">bindings()</a> = [{<span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-name\">name()</a></span>, <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-value\">value()</a></span>}]</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-binding_struct\">binding_struct()</a> = <span class=\"bold_code\"><a href=\"psi_element://orddict#type-orddict\">orddict:orddict()</a></span></span><br></p>\n" +
        "<div class=\"REFBODY\"><p><p>A binding structure.</p></p></div>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-expression\">expression()</a> = <span class=\"bold_code\"><a href=\"psi_element://erl_parse#type-abstract_expr\">erl_parse:abstract_expr()</a></span></span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-expressions\">expressions()</a> = [<span class=\"bold_code\"><a href=\"psi_element://erl_parse#type-abstract_expr\">erl_parse:abstract_expr()</a></span>]</span><br></p>\n" +
        "<div class=\"REFBODY\"><p><p>As returned by <span class=\"bold_code\"><a href=\"psi_element://erl_parse#parse_exprs-1\">\n" +
        "        <span class=\"code\">erl_parse:parse_exprs/1</span></a></span> or\n" +
        "        <span class=\"bold_code\"><a href=\"psi_element://io#parse_erl_exprs-2\">\n" +
        "        <span class=\"code\">io:parse_erl_exprs/2</span></a></span>.</p></p></div>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-expression_list\">expression_list()</a> = [<span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-expression\">expression()</a></span>]</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-func_spec\">func_spec()</a> = {Module :: module(), Function :: atom()}<br>            | function()</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-lfun_eval_handler\">lfun_eval_handler()</a> = <br>    fun((Name :: atom(),<br>         Arguments :: <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-expression_list\">expression_list()</a></span>,<br>         Bindings :: <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-binding_struct\">binding_struct()</a></span>) -&gt;<br>            {value,<br>             Value :: <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-value\">value()</a></span>,<br>             NewBindings :: <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-binding_struct\">binding_struct()</a></span>})</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-lfun_value_handler\">lfun_value_handler()</a> = <br>    fun((Name :: atom(), Arguments :: [term()]) -&gt;<br>            Value :: <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-value\">value()</a></span>)</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-local_function_handler\">local_function_handler()</a> = {value, <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-lfun_value_handler\">lfun_value_handler()</a></span>}<br>                         | {eval, <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-lfun_eval_handler\">lfun_eval_handler()</a></span>}<br>                         | none</span><br></p>\n" +
        "<div class=\"REFBODY\"><p><p>Further described\n" +
        "        <span class=\"bold_code\"><a href=\"psi_element://erl_eval#local_function_handler\">below.</a></span></p>\n" +
        "      </p></div>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-name\">name()</a> = term()</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-nlfun_handler\">nlfun_handler()</a> = <br>    fun((FuncSpec :: <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-func_spec\">func_spec()</a></span>, Arguments :: [term()]) -&gt; term())</span><br></p>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-non_local_function_handler\">non_local_function_handler()</a> = {value, <span class=\"bold_code\"><a href=\"psi_element://erl_eval#type-nlfun_handler\">nlfun_handler()</a></span>} | none</span><br></p>\n" +
        "<div class=\"REFBODY\"><p><p>Further described\n" +
        "        <span class=\"bold_code\"><a href=\"psi_element://erl_eval#non_local_function_handler\">below.</a></span></p>\n" +
        "      </p></div>\n" +
        "    <p>\n" +
        "      <span class=\"bold_code\"><a name=\"type-value\">value()</a> = term()</span><br></p>\n" +
        "  \n" +
        "</body></html>\n",
      "" +
        "-module(test).\n" +
        "test() ->\n" +
        "    erl<caret>_eval:expr(1, 2).\n");
  }

  public void testResolveLinkModule() {
    final PsiElement psiElement = myErlangDocProvider.getDocumentationElementForLink(getPsiManager(), "file", null);
    assertTrue("Not a module: " + psiElement, psiElement instanceof ErlangModule);
  }

  public void testResolveLinkFunction() {
    final PsiElement psiElement = myErlangDocProvider.getDocumentationElementForLink(getPsiManager(), "file#read_line-1", null);
    assertTrue("Not a function: " + psiElement, psiElement instanceof ErlangFunction);
  }

  public void testResolveLinkType() {
    final PsiElement psiElement = myErlangDocProvider.getDocumentationElementForLink(getPsiManager(), "file#type-filename", null);
    assertTrue("Not a type: " + psiElement, psiElement instanceof ErlangTypeDefinition);
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