-export([quotes/0, single_line/0]).

quotes() ->
  """
  missing new line
  """.


single_line() ->
    <error>"""main"""</error>,
    ok.
