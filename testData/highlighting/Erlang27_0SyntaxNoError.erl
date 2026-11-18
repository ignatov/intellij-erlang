-export([quotes/0, sigils/0]).

quotes() ->
  """
  triple-quoted string
  """.

sigils() ->
  [~"vanilla sigil", ~b"binary sigil", ~s(string sigil)].
