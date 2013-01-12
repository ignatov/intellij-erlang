-define(mymacro(Foo), {ok, Foo}).

-export([failing_fun/0, my_fun/0]).

my_fun() -> {ok, bar}.

failing_fun() ->
  case my_fun() of
    ?mymacro(Foo) -> Foo;
    _ -> baz
  end.

