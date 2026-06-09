-export([quotes/0, sigils/0, sigil_delimiters/0]).

quotes() ->
  <error descr="Triple quotes are only supported in Erlang 27 and newer versions">"""
  missing new line
  """</error>.

%% Sigils should report errors on SDK < 27
sigils() ->
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~"vanilla sigil"</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~b"binary sigil"</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~B"verbatim binary"</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~s"string sigil"</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~S"verbatim string"</error>,
    ok.

%% Every delimiter form of a sigil is unsupported on SDK < 27
sigil_delimiters() ->
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~(parens)</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~[brackets]</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~{braces}</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~<angles></error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~/slash/</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~|pipe|</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~'single quote'</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~`backtick`</error>,
    <error descr="Sigils are only supported in Erlang 27 and newer versions">~#hash#</error>,
    ok.
