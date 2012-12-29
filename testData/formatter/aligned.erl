%%  Copyright (c) 2012. Sergey Ignatov.
-module(aaa).
-author("ignatov").

%% API
-export([new/1,
              match/2]).

-export_type([args_spec/0,
                  opt_args_spec/0,
                args_matcher/0]).


-record(abd, {a = 1, c=1,
 d = 1, b = {1,
              2} }  ).


is_digits(
    [Dig | Tail],
    Options) ->
  if 
      $0 =< Dig, Dig =< $9 -> is_digits(Tail);
  true -> false
  end;
is_digits([]) ->
  L = [ a,
        b,
        c],
  R = L#abd{a=1,
            b=1, c=1},

  true.

f(N) ->
if
N == 42 -> true;
true -> false
end.

foo(L) ->
case L of
{answer, N} when N == 42 -> true;
_ -> false
end.


tf(L) ->
try find(L) of
{answer, N} when N == 42 -> true;
_ -> false
catch
{notanumber, R} when is_list(R) -> alist;
{notanumber, R} when is_float(R) -> afloat;
_ -> noidea
end.