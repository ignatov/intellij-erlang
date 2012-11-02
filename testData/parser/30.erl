-module(empty).
-export([start/2]).

-record(m, {id}).
-define(mmmm, {id}).

bar() ->
  F = fun({Eep}) ->
    ok,
    #'rec'{something = Eep} = Eep#'rec'.something,
    1 + 1 * 109,
    foo([{a}, {b}, ]),
    blah
  end,
  F.



-export([bar/0]).

-record(rec, {something}).

foo(Blah) -> Blah.

bar() ->
  F = fun({Eep}) ->
    ok,
    #'rec'{something = Eep} = Eep#'rec'.something,
    1 + 1 * 109,
    foo([{a}, {b}, ]),
    blah
  end,
  F.

bar2() ->
  F = fun({Eep}) ->
    ok,
    #'rec'{something = Eep} = Eep#'rec'.something,
    1 + 1 * 109,
    foo([{a}, {b}, 1]),
    blah
  end,
  F.