-export([quotes/0, single_line/0, bad_indent/0, sigils/0, sigil_delimiters/0,
         sigil_escapes/0, sigil_triple_quoted/0]).

quotes() ->
  """
  missing new line
  """.


single_line() ->
    <error>"""main"""</error>,
    ok.

bad_indent() ->
  <error>"""
  main"""</error>.

%% Sigils should not report Erlang27SyntaxInspection errors on OTP 27+
sigils() ->
    ~"vanilla sigil",
    ~b"binary sigil",
    ~B"verbatim binary",
    ~s"string sigil",
    ~S"verbatim string",
    ok.

%% Every start-end delimiter pair and symmetric delimiter is valid on OTP 27+
sigil_delimiters() ->
    ~(paren delimiter),
    ~[bracket delimiter],
    ~{brace delimiter},
    ~<angle delimiter>,
    ~/slash delimiter/,
    ~|pipe delimiter|,
    ~'single quote delimiter',
    ~`backtick delimiter`,
    ~#hash delimiter#,
    ~b(binary parens),
    ~B[verbatim brackets],
    ~s{string braces},
    ~S<verbatim angles>,
    ok.

%% Escapes are valid in lowercase sigils and ignored (verbatim) in uppercase ones
sigil_escapes() ->
    ~b"tab \t newline \n quote \"",
    ~B"no escapes here \n stays literal",
    ~s"escape \n \t",
    ~S"verbatim string \n",
    ok.

%% Triple-quoted sigils, including binary/string variants
sigil_triple_quoted() ->
    ~"""
    multi line vanilla
    """,
    ~b"""
    multi line binary
    """,
    ~B"""
    verbatim triple quoted
    """,
    ok.
