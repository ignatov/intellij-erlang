-export([f/0, g/0, h/0, sigils/0, single_line/0, bad_indent/0, bad_sigil_opening/0, other_delimiters/0]).

-moduledoc """"
  Example:
  -doc """
  Doc.
  """.
  """".

f() -> """"
  a """ inside
  """".

g() -> """""
  b """" inside
  """"".

h() -> """""""
  seven
  """"""".

sigils() ->
    ~""""
    x
    """",
    ~s""""
    y """ y
    """",
    ~B"""""
    z
    """"",
    ok.

single_line() ->
    <error>""""main""""</error>,
    ok.

bad_indent() ->
  <error>""""
  main""""</error>.

bad_sigil_opening() ->
  <error descr="Not white space after start of triple-quoted string">~s""""main
  """"</error>,
  ok.

other_delimiters() ->
  ~("""x),
  ~["""x],
  ~'"""x',
  ~/"""x/,
  ~<"""x>,
  ~|"""x|,
  ~`"""x`,
  ~#"""x#,
  ~(""""),
  ok.
