-export([quotes/0, single_line/0, bad_indent/0, sigils/0]).

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

sigils() ->
    ~"hello",
    ~b(binary),
    ~S[verbatim],
    ok.