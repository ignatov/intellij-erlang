-export([f/0, g/0]).

f() -> <error descr="Not white space after start of triple-quoted string">""" x
  text
  """</error>.

g() -> <error descr="Not white space after start of triple-quoted string">"""	 x
  text
  """</error>.
